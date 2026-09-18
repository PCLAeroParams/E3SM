#!/bin/bash

# Build/run driver for the PARTMCSL-DISABLED SL diagnostic at qsize=4 but
# QSIZE_D=9 (arrays dimensioned for 9 tracers, only 4 used).  Companion
# to run_homme_sl_only.sh (QSIZE_D=4, qsize=4) and run_homme_sl_only_q8.sh
# (QSIZE_D=9, qsize=8).  This is the only rung that decouples the
# compile-time array bound from the runtime tracer count.
#
# Usage:
#   ./run_homme_sl_only_qsized9_q4.sh -c    # configure once
#   ./run_homme_sl_only_qsized9_q4.sh -b    # build
#   ./run_homme_sl_only_qsized9_q4.sh -r    # run 1-day diagnostic
#
# Output: /scratch/pabosle/e3sm-pclap-sl-only-qsized9-q4/movies_sl_only_qsized9_q4/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Fresh build tree so QSIZE_D=9 doesn't disturb the QSIZE_D=4 baseline tree.
wdir=/scratch/pabosle/e3sm-pclap-sl-only-qsized9-q4

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_sl_only_qsized9_q4.nl
nlFile=/tmp/dcmip12_sl_only_qsized9_q4.nl
outDir=./movies_sl_only_qsized9_q4/

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
  printf "Configuring standalone Homme (HOMME_ENABLE_PARTMCSL=OFF, QSIZE_D=9, namelist qsize=4)\n"
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
  printf "Running ${execName} (ne=30, no partmcsl, QSIZE_D=9, qsize=4) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-sl-only-qsized9-q4.txt || true
  printf "Finished (or timed out) sl-only QSIZE_D=9 qsize=4 diagnostic.\n"
fi
