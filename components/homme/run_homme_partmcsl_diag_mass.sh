#!/bin/bash

# Build/run driver for the DCMIP 2012 Test 1-1 mass-conservation
# diagnostic with -DPARTMCSL_DIAG_MASS=ON.  Same physics as
# run_homme_partmcsl_dcmip.sh (ne=30, nlev=64, qsize=7, 12 days) but
# with a runtime print of tracer mass on both the FV (pg_data%q) and
# GLL (state%Q) grids at every output snapshot.
#
# Separate build tree because PARTMCSL_DIAG_MASS is compile-time.
#
# Interpretation of the printed lines (see partmcsl_report_mass in
# dcmip12_wrapper.F90):
#   - FV(day N) vs FV(day 0)      => internal PartMCSL mass drift
#   - GLL(day N, post_projection)
#         vs FV(day N, pre_projection) => FV->GLL projection drift
#   - FV(t=0) vs GLL(t=0, IC)     => gfr_dyn_to_fv_phys seed sanity check
#
# Usage:
#   ./run_homme_partmcsl_diag_mass.sh -c    # configure (PARTMCSL_DIAG_MASS=ON)
#   ./run_homme_partmcsl_diag_mass.sh -b    # build theta-l-nlev64-native
#   ./run_homme_partmcsl_diag_mass.sh -r    # run
#   ./run_homme_partmcsl_diag_mass.sh -s    # emit batch script
#
# Output:
#   /scratch/pabosle/e3sm-pclap-dcmip-diag-mass/movies_dcmip_diag_mass/dcmip2012_test1_11.nc
#   /scratch/pabosle/e3sm-pclap-dcmip-diag-mass/homme-out-dcmip-diag-mass.txt
#
# After -r, extract just the mass lines with:
#   grep PARTMCSL_DIAG_MASS $wdir/homme-out-dcmip-diag-mass.txt

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Separate build tree so PARTMCSL_DIAG_MASS=ON does not disturb the
# main partmcsl DCMIP build at /scratch/pabosle/e3sm-pclap-dcmip.
wdir=/scratch/pabosle/e3sm-pclap-dcmip-diag-mass

execName=theta-l-nlev64-native

baseNamelist=$homme/partmcsl_dcmip12_transport.nl
nlFile=/tmp/partmcsl_dcmip_diag_mass.nl
outDir=./movies_dcmip_diag_mass/

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFile=batch_run_dcmip_diag_mass.cmd
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
  printf "Configuring standalone Homme (PARTMCSL_DIAG_MASS=ON, SBR_DIAG=OFF)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=OFF \
    -DPARTMCSL_DIAG_MASS=ON $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  cd $wdir
  make -j 24 $execName
fi

# ne = 30 is already the default in the base namelist; override output_dir
# so this run does not stomp the sibling partmcsl DCMIP movies.
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
  printf "Running ${execName} (ne=30, PARTMCSL_DIAG_MASS=ON) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # Machine-specific mpirun layout (see run_homme_partmcsl_dcmip.sh for
  # the reasoning).  fd cap workaround per cee-compute005/046 notes.
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
    | tee homme-out-dcmip-diag-mass.txt || true
  printf "Finished (or timed out) ne=30 diag-mass run.\n"
  printf "\nExtract mass diagnostic lines with:\n"
  printf "  grep PARTMCSL_DIAG_MASS %s/homme-out-dcmip-diag-mass.txt\n" "$wdir"
fi
