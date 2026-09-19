#!/bin/bash

# Build/run driver for the original DCMIP 2012 Test 1-1 (3D deformational
# flow) with PartMCSL Q5 alongside SL Q, at ne = 30, nlev = 64, 12-day
# integration with daily output.
#
# This is the "default" (non-diagnostic) build path: PARTMCSL_SBR_DIAG must
# be OFF so `dcmip2012_test1_1` runs the real deformational winds (not the
# SBR override).  A separate build dir keeps this configuration isolated
# from the SBR/VT builds.
#
# Usage:
#   ./run_homme_partmcsl_dcmip.sh -c    # configure (SBR_DIAG=OFF)
#   ./run_homme_partmcsl_dcmip.sh -b    # build theta-l-nlev64-native
#   ./run_homme_partmcsl_dcmip.sh -r    # run
#   ./run_homme_partmcsl_dcmip.sh -s    # emit batch script
#
# Output: /scratch/pabosle/e3sm-pclap-dcmip/movies_dcmip_ne30/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Separate build tree from the SBR/VT one so PARTMCSL_SBR_DIAG=OFF here does
# not disturb the SBR sweep build.
wdir=/scratch/pabosle/e3sm-pclap-dcmip

execName=theta-l-nlev64-native

baseNamelist=$homme/partmcsl_dcmip12_transport.nl
nlFile=/tmp/partmcsl_dcmip_ne30.nl
outDir=./movies_dcmip_ne30/

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFile=batch_run_dcmip.cmd
nnodes=1
# 12-day run at ne=30, nlev=64, tstep=10 s -> ~1.04e5 dyn steps.  3.3x more
# steps than tstep=33 s; wall time budgeted at ~10 h with headroom.
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
  printf "Configuring standalone Homme for DCMIP deformational (PARTMCSL_SBR_DIAG=OFF)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=OFF $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  cd $wdir
  make -j 24 $execName
fi

# ne = 30 is already the default in the base namelist; only override output_dir
# so this run does not stomp the SBR/VT movies directories.
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
  printf "Running ${execName} (ne=30) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # Machine-specific mpirun layout.  Detect host and pick the right sizing:
  #   cee-compute046: 8 * 60 phys cores = 480 total (Xeon Platinum 8490H)
  #   cee-compute005: 16 * 28 phys cores = 448 total (Xeon Platinum 8180)
  # One rank per physical core, SMT siblings idle, ranks distributed across
  # sockets for full memory bandwidth.
  #
  # OpenMPI opens ~3 pipes per rank for stdout/stderr forwarding, so 448/480
  # ranks want ~1500+ file descriptors; the RHEL default soft limit of
  # 1024 trips iof_base_setup.c:116.  Raise soft to hard, plus env var.
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
  # timeout + `|| true` guards against the SLMM ~g_csl_mpi hang at finalize
  # (documented in partmcsl_compose_hang_handoff.md).  Outputs are already
  # flushed by then.
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by $mpi_map --bind-to core --n $mpi_n \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-dcmip-ne30.txt || true
  printf "Finished (or timed out) ne=30.\n"
fi
