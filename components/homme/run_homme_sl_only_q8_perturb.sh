#!/bin/bash

# Build/run driver for the PARTMCSL-DISABLED SL diagnostic at qsize=8
# with -DPARTMCSL_PERTURB_MIRROR=ON so q(5:8) = q(1:4) * 0.99999 (not exact).
#
# Direct A/B against sl_only_q8 (exact mirror).  If Q1 becomes smooth like
# sl_only_q4 after this perturbation, CEDR's global mass-consistency solve
# is not robust to bit-duplicate tracer inputs -- corner case not exercised
# in production because real physical tracers are never bit-duplicates.
#
# Usage:
#   ./run_homme_sl_only_q8_perturb.sh -c   # configure once
#   ./run_homme_sl_only_q8_perturb.sh -b   # build
#   ./run_homme_sl_only_q8_perturb.sh -r   # run 1-day diagnostic

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap-sl-only-q8-perturb

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_sl_only_q8_perturb.nl
nlFile=/tmp/dcmip12_sl_only_q8_perturb.nl
outDir=./movies_sl_only_q8_perturb/

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
  printf "Configuring standalone Homme (HOMME_ENABLE_PARTMCSL=OFF, QSIZE_D=9, PERTURB_MIRROR=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=OFF \
    -DPARTMCSL_PERTURB_MIRROR=ON $homme
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
  printf "Running ${execName} (ne=30, no partmcsl, qsize=8, perturbed mirror) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-sl-only-q8-perturb.txt || true
  printf "Finished (or timed out) sl-only q8 perturbed-mirror diagnostic.\n"
fi
