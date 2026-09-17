#!/bin/bash

# Build/run driver for the horizontal SBR sweep (dcmip2012_test1_1 with
# PARTMCSL_SBR_DIAG=ON so the Test S solid-body-rotation wind + Q7
# analytic-exact overrides are compiled in).  Sweeps ne = 16, 30, 60, 120
# on a single fixed-nlev build target, using the pre-generated per-ne
# namelists at $homme/partmcsl_sbr_sweep_ne${ne}.nl (each already carries
# the matched tstep / nmax / output_dir for its ne).
#
# Usage:
#   ./run_homme_partmcsl_sbr.sh -c              # configure once (SBR_DIAG=ON)
#   ./run_homme_partmcsl_sbr.sh -b              # build execName
#   ./run_homme_partmcsl_sbr.sh -r              # run all ne
#   ./run_homme_partmcsl_sbr.sh -n "16 30" -r   # subset via -n
#   ./run_homme_partmcsl_sbr.sh -s              # emit per-ne batch scripts
#
# Outputs land in $wdir/movies_sbr_sweep_ne${ne}/dcmip2012_test1_11.nc,
# ready for mass_conservation.py --horiz on the resulting netCDFs.

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap

# Default ne sweep — override with -n "16 30" or a subset.
neList="16 30 60 120"

# Single build target for the full sweep.  128 levels matches the prior
# horizontal SBR verification (memory: partmcsl_project_sbr_horizontal_done).
execName=theta-l-nlev128-native

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFilePrefix=batch_run_sbr
nnodes=1
wtime="02:00:00"
res=flight-cldera
acct=fy210162
ntasks=448
# ne=120 with nmax=2880 dominates; per-ne timeout generous enough for it.
# Also guards against the SLMM ~g_csl_mpi finalize hang (documented in
# partmcsl_compose_hang_handoff.md); outputs are already flushed by then.
runTimeout="3600s"

while getopts 'cbrsn:' OPTION
do
  case $OPTION in
    c) configFlag=1 ;;
    b) buildFlag=1 ;;
    r) runFlag=1 ;;
    s) submitFlag=1 ;;
    n) neList=$OPTARG ;;
    ?) printf "Usage: %s: [-cbrs] [-n \"ne1 ne2 ...\"]\n" $(basename $0) >&2
       exit 2
       ;;
  esac
done
shift $(($OPTIND -1))

if [ "$configFlag" ]
then
  printf "Configuring standalone Homme (PARTMCSL_SBR_DIAG=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=ON $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  cd $wdir
  make -j 24 $execName
fi

for ne in $neList
do
  nlFile=$homme/partmcsl_sbr_sweep_ne${ne}.nl
  outDir=./movies_sbr_sweep_ne${ne}/

  if [ ! -f "$nlFile" ]; then
    printf "Missing namelist for ne=${ne}: ${nlFile}; skipping.\n" >&2
    continue
  fi

  if [ "$submitFlag" ]
  then
    jobFile=${jobFilePrefix}_ne${ne}.cmd
    printf "creating job command for ne=${ne}: ${jobFile}\n"
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
    printf "Running ${execName} (ne=${ne}) -> ${outDir}\n"
    mkdir -p $wdir/$outDir
    cd $wdir
    # Local box: quad Xeon 8176 = 4 sockets * 28 phys cores = 112 phys,
    # 224 logical w/ SMT.  Good-neighbor cap = half the hyperthreaded
    # total = 112 hardware threads => use all 112 physical cores (one
    # rank per phys core, SMT siblings left idle).  Ranks distributed
    # 28 per socket across all four sockets for full memory bandwidth.
    timeout --kill-after=10s $runTimeout \
      mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 112 \
        $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
      | tee homme-out-sbr-ne${ne}.txt || true
    printf "Finished (or timed out) ne=${ne}; continuing.\n"
  fi
done
