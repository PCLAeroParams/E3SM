#!/bin/bash

# Build/run driver for the Vertical Translation Test (dcmip2012_test1_vt).
# Sweeps nlev = 20, 64, 128, 256 on theta-l-nlev${N}-native targets, running
# partmcsl_vt.nl with a per-nlev output_dir so runs don't stomp each other.
# Adapted from run_homme_partmcsl.sh.
#
# Usage:
#   ./run_homme_partmcsl_vt.sh -c              # configure once
#   ./run_homme_partmcsl_vt.sh -b              # build all nlev targets
#   ./run_homme_partmcsl_vt.sh -r              # run all nlev targets
#   ./run_homme_partmcsl_vt.sh -n "20 64" -br  # subset via -n
#
# The Python analytic-exact reference lives at
# /workspace/vertical_translation_exact.py (path names must stay in sync
# with the ne / output_dir choices below).

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap

# Default nlev sweep — override with -n "20 64 128 256" or a subset.
nlevList="20 64 128 256"

baseNamelist=$homme/partmcsl_vt.nl

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFilePrefix=batch_run_vt
nnodes=1
wtime="01:00:00"
res=flight-cldera
acct=fy210162
# ne=16 gives nelemd = 6*256 = 1536; 448 matches the SBR sweep.
ntasks=448
# HOMME/SLMM's ~g_csl_mpi destructor hangs during finalize (documented in
# partmcsl_compose_hang_handoff.md).  Outputs are already flushed by then,
# so we bound each run with timeout and continue the loop regardless of
# exit status.  Bump if a fine-nlev run legitimately needs more time.
runTimeout="600s"

while getopts 'cbrsn:' OPTION
do
  case $OPTION in
    c) configFlag=1 ;;
    b) buildFlag=1 ;;
    r) runFlag=1 ;;
    s) submitFlag=1 ;;
    n) nlevList=$OPTARG ;;
    ?) printf "Usage: %s: [-cbrs] [-n \"nlev1 nlev2 ...\"]\n" $(basename $0) >&2
       exit 2
       ;;
  esac
done
shift $(($OPTIND -1))

if [ "$configFlag" ]
then
  printf "Configuring standalone Homme\n"
  cd $wdir
  # Keep -DPARTMCSL_SBR_DIAG=ON so the same build dir supports SBR + VT runs;
  # the SBR_DIAG guards only affect dcmip2012_test1_1 (unused by VT).
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=ON $homme
fi

for nlev in $nlevList
do
  execName=theta-l-nlev${nlev}-native
  nlFile=/tmp/partmcsl_vt_nlev${nlev}.nl
  outDir=./movies_vt_nlev${nlev}/

  # Rewrite output_dir per nlev so runs don't stomp each other.  Everything
  # else in the namelist is nlev-independent (nlev itself is compile-time
  # in this build tree).
  sed "s|output_dir *=.*|output_dir        = \"${outDir}\"|" $baseNamelist > $nlFile

  if [ "$buildFlag" ]
  then
    printf "Building ${execName}\n"
    cd $wdir
    make -j 24 $execName
  fi

  if [ "$submitFlag" ]
  then
    jobFile=${jobFilePrefix}_nlev${nlev}.cmd
    printf "creating job command for nlev=${nlev}: ${jobFile}\n"
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
    printf "Running ${execName} (nlev=${nlev}) -> ${outDir}\n"
    mkdir -p $wdir/$outDir
    cd $wdir
    # ne=16 -> nelemd=1536; 448 ranks matches SBR sweep, plenty of headroom.
    # timeout + `|| true` guards against the SLMM ~g_csl_mpi hang so the
    # loop continues to the next nlev.  --kill-after sends SIGKILL 10s
    # after the SIGTERM if the process is still stuck.
    timeout --foreground --kill-after=10s $runTimeout \
      mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n $ntasks \
        $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
      | tee homme-out-vt-nlev${nlev}.txt || true
    printf "Finished (or timed out) nlev=${nlev}; continuing.\n"
  fi
done
