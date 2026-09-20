#!/bin/bash

# Build/run driver for the DCMIP 2012 Test 1-1 mass-conservation
# attribution: PARTMCSL_DIAG_MASS + PARTMCSL_SKIP_HORIZONTAL_ONLY.
# Only partmcsl_vertical_step runs; partmcsl_step_forward is compiled
# out.  Pairs with run_homme_partmcsl_diag_horizontal_only.sh (which
# skips vertical instead) to attribute the ~4.7e-4 day-1 Q5 mass loss
# between the two partmcsl steps.
#
# Expected outcome interpretation (day-1 FV(pre) - FV(IC) drift):
#   - Q5 drift matches the full run (~-4.7e-4)  => vertical step leaks;
#     horizontal step is conservative.
#   - Q5 drift is ~0                            => horizontal step leaks;
#     vertical step is conservative.
#   - Intermediate                              => both leak.
#
# Separate build tree because PARTMCSL_SKIP_HORIZONTAL_ONLY is compile-time.
#
# Usage:
#   ./run_homme_partmcsl_diag_vertical_only.sh -c    # configure
#   ./run_homme_partmcsl_diag_vertical_only.sh -b    # build
#   ./run_homme_partmcsl_diag_vertical_only.sh -r    # run
#   ./run_homme_partmcsl_diag_vertical_only.sh -s    # emit batch script
#
# Output:
#   /scratch/pabosle/e3sm-pclap-dcmip-diag-vonly/movies_dcmip_diag_vonly/dcmip2012_test1_11.nc
#   /scratch/pabosle/e3sm-pclap-dcmip-diag-vonly/homme-out-dcmip-diag-vonly.txt

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap-dcmip-diag-vonly

execName=theta-l-nlev64-native

baseNamelist=$homme/partmcsl_dcmip12_transport.nl
nlFile=/tmp/partmcsl_dcmip_diag_vonly.nl
outDir=./movies_dcmip_diag_vonly/

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFile=batch_run_dcmip_diag_vonly.cmd
nnodes=1
wtime="10:00:00"
res=flight-cldera
acct=fy210162
ntasks=448
runTimeout="36000s"

while getopts 'cbrs' OPTION
do
  case $OPTION in
    c) configFlag=1 ;;
    b) buildFlag=1 ;;
    r) runFlag=1 ;;
    s) submitFlag=1 ;;
    ?) printf "Usage: %s: [-cbrs]\n" $(basename $0) >&2
       exit 2
       ;;
  esac
done
shift $(($OPTIND -1))

if [ "$configFlag" ]
then
  printf "Configuring standalone Homme (PARTMCSL_DIAG_MASS=ON, PARTMCSL_SKIP_HORIZONTAL_ONLY=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=OFF \
    -DPARTMCSL_DIAG_MASS=ON \
    -DPARTMCSL_SKIP_HORIZONTAL_ONLY=ON $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  cd $wdir
  make -j 24 $execName
fi

sed "s|output_dir *=.*|output_dir        = \"${outDir}\"|" $baseNamelist > $nlFile

if [ "$submitFlag" ]
then
  printf "creating job command: ${jobFile}\n"
  cat <<EOF > $jobFile
#!/bin/bash
#SBATCH -N $nnodes
#SBATCH -t $wtime
#SBATCH -A $acct
#SBATCH -n $ntasks
#SBATCH --reservation $res
mkdir -p $wdir/$outDir
cd $wdir
mpirun --map-by core --bind-to core --n $ntasks $wdir/test_execs/$execName/$execName < $nlFile
EOF
  chmod +x $jobFile
  cat $jobFile
fi

if [ "$runFlag" ]
then
  printf "Running ${execName} (ne=30, vertical-only partmcsl) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  host=$(hostname -s)
  case $host in
    cee-compute046*) mpi_map='ppr:60:socket:PE=1'; mpi_n=480 ;;
    cee-compute005*) mpi_map='ppr:28:socket:PE=1'; mpi_n=448 ;;
    *)               mpi_map='ppr:28:socket:PE=1'; mpi_n=448
                     printf "WARN: unknown host %s, defaulting to cee-compute005 layout (28x16=448)\n" "$host" ;;
  esac
  printf "Launching on %s: --map-by %s --n %d\n" "$host" "$mpi_map" "$mpi_n"
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by $mpi_map --bind-to core --n $mpi_n \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-dcmip-diag-vonly.txt || true
  printf "Finished (or timed out) ne=30 vertical-only diag run.\n"
  printf "\nExtract mass diagnostic lines with:\n"
  printf "  grep PARTMCSL_DIAG_MASS %s/homme-out-dcmip-diag-vonly.txt\n" "$wdir"
fi
