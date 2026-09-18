#!/bin/bash

# Build/run driver for the PARTMCSL-DISABLED SL diagnostic at qsize=8.
# Companion to run_homme_sl_only.sh (qsize=4) -- everything else identical
# (ne, nlev, tstep, transport switches).  Isolates whether SL/CEDR loading
# 8 tracers vs 4 tracers is enough by itself to reproduce the Q1
# degradation seen with partmcsl compiled in.
#
# Requires the wrapper change that sizes q(8) in the sl-only branch and
# mirrors q(5:8)=q(1:4) at t=0 (see dcmip12_wrapper.F90 changes).
#
# Usage:
#   ./run_homme_sl_only_q8.sh -c    # configure once (PARTMCSL=OFF, QSIZE_D=9)
#   ./run_homme_sl_only_q8.sh -b    # build
#   ./run_homme_sl_only_q8.sh -r    # run 1-day diagnostic
#
# Output: /scratch/pabosle/e3sm-pclap-sl-only-q8/movies_sl_only_q8/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Fresh build tree -- QSIZE_D=9 differs from the sl-only q4 tree's =4.
wdir=/scratch/pabosle/e3sm-pclap-sl-only-q8

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_sl_only_q8.nl
nlFile=/tmp/dcmip12_sl_only_q8.nl
outDir=./movies_sl_only_q8/

configFlag=
buildFlag=
runFlag=

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
  printf "Configuring standalone Homme (HOMME_ENABLE_PARTMCSL=OFF, QSIZE_D=9)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
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
  printf "Running ${execName} (ne=30, no partmcsl, qsize=8) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-sl-only-q8.txt || true
  printf "Finished (or timed out) sl-only q8 diagnostic.\n"
fi
