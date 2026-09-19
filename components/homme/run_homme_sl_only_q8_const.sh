#!/bin/bash

# Build/run driver for the PARTMCSL-DISABLED SL diagnostic at qsize=8 with
# q(5:8) seeded as spatial constants (0.1, 0.3, 0.5, 0.7) instead of
# mirroring q(1:4).  Companion to run_homme_sl_only_q8.sh (mirror) --
# everything else identical (ne, nlev, tstep, transport switches).
#
# Purpose: distinguish whether the Q1 fragmentation seen in sl_only_q8 is
# triggered by (a) qsize=8 tracer count alone, or (b) duplicating DCMIP
# tracer content into the upper 4 slots.  Spatial constants are the
# extreme "not the DCMIP tracer content" case -- trivially transported by
# SL, no bounds/limiter action possible.
#
# Requires the wrapper change that adds the PARTMCSL_CONST_MIRROR branch
# in dcmip12_wrapper.F90 (both HOMME_ENABLE_PARTMCSL and sl-only sides).
#
# Usage:
#   ./run_homme_sl_only_q8_const.sh -c    # configure once
#   ./run_homme_sl_only_q8_const.sh -b    # build
#   ./run_homme_sl_only_q8_const.sh -r    # run 1-day diagnostic
#
# Output: /scratch/pabosle/e3sm-pclap-sl-only-q8-const/movies_sl_only_q8_const/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Fresh build tree -- PARTMCSL_CONST_MIRROR is a compile-time flag so we
# cannot share the sl_only_q8 build tree.
wdir=/scratch/pabosle/e3sm-pclap-sl-only-q8-const

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_sl_only_q8_const.nl
nlFile=/tmp/dcmip12_sl_only_q8_const.nl
outDir=./movies_sl_only_q8_const/

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
  printf "Configuring standalone Homme (HOMME_ENABLE_PARTMCSL=OFF, QSIZE_D=9, PARTMCSL_CONST_MIRROR=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=OFF \
    -DPARTMCSL_CONST_MIRROR=ON $homme
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
  printf "Running ${execName} (ne=30, no partmcsl, qsize=8, const q5..q8) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-sl-only-q8-const.txt || true
  printf "Finished (or timed out) sl-only q8 const diagnostic.\n"
fi
