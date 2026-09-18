#!/bin/bash

# Build/run driver for the PARTMCSL-DISABLED SL diagnostic.  Same
# transport switches, ne=30, nlev=64, tstep=33 as the partmcsl DCMIP
# driver, but built with -DHOMME_ENABLE_PARTMCSL=OFF so no partmcsl
# code path (init, forcing, phys_to_dyn, output copy-back) compiles in.
# This gives ground-truth SL Q1 on this config; a healthy day-1 peak
# here proves the remaining deficit in the partmcsl-enabled build is
# partmcsl-adjacent, and a low day-1 peak here proves the deficit is
# purely a transport-config issue independent of partmcsl.
#
# Usage:
#   ./run_homme_sl_only.sh -c    # configure (PARTMCSL=OFF)
#   ./run_homme_sl_only.sh -b    # build theta-l-nlev64-native
#   ./run_homme_sl_only.sh -r    # run 2-day diagnostic
#
# Output: /scratch/pabosle/e3sm-pclap-sl-only/movies_sl_only/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Fresh build tree so we don't disturb the partmcsl build's CMake cache.
wdir=/scratch/pabosle/e3sm-pclap-sl-only

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_sl_only.nl
nlFile=/tmp/dcmip12_sl_only.nl
outDir=./movies_sl_only/

configFlag=
buildFlag=
runFlag=

# 2-day run at ne=30, nlev=64, tstep=33 -> 5232 dyn steps.  Small run,
# generous timeout.
runTimeout="1800s"

while getopts 'cbr' OPTION
do
  case $OPTION in
    c) configFlag=1 ;;
    b) buildFlag=1 ;;
    r) runFlag=1 ;;
    ?) printf "Usage: %s: [-cbr]\n" $(basename $0) >&2
       exit 2
       ;;
  esac
done
shift $(($OPTIND -1))

if [ "$configFlag" ]
then
  printf "Configuring standalone Homme (HOMME_ENABLE_PARTMCSL=OFF)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=4 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=OFF $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  cd $wdir
  make -j 24 $execName
fi

sed "s|output_dir *=.*|output_dir        = \"${outDir}\"|" $baseNamelist > $nlFile

if [ "$runFlag" ]
then
  printf "Running ${execName} (ne=30, no partmcsl) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical
  # w/ SMT).  Good-neighbor cap = half the hyperthreaded total = 448
  # hardware threads => use all 448 physical cores, one rank per phys
  # core, SMT siblings idle.  Ranks distributed 28 per socket across all
  # sixteen sockets for full memory bandwidth.  Bump fd cap for OpenMPI's
  # per-rank pipes; belt-and-braces env var also asks Open MPI to raise
  # limits itself.
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-sl-only.txt || true
  printf "Finished (or timed out) SL-only diagnostic.\n"
fi
