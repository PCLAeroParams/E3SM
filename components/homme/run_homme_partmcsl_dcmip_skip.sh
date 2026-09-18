#!/bin/bash

# Build/run driver for the partmcsl-enabled DCMIP 1-1 test with the
# dcmip2012_test1_1_phys_to_dyn call compiled OUT via
# -DPARTMCSL_SKIP_PHYS_TO_DYN=ON.
#
# A/B against run_homme_partmcsl_dcmip.sh (which has SKIP=OFF, i.e. the
# copy-back is active).  Both configure with -DHOMME_ENABLE_PARTMCSL=ON
# and -DPARTMCSL_SBR_DIAG=OFF; the only difference is the SKIP flag.
# Runs the 1-day dcmip12_skip_p2d.nl (fragmentation shows by day 1).
#
# Usage:
#   ./run_homme_partmcsl_dcmip_skip.sh -c    # configure once
#   ./run_homme_partmcsl_dcmip_skip.sh -b    # build theta-l-nlev64-native
#   ./run_homme_partmcsl_dcmip_skip.sh -r    # run 1-day A/B
#
# Output: /scratch/pabosle/e3sm-pclap-skip-p2d/movies_skip_p2d/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Fresh tree so this build's CMake cache does not disturb the main
# DCMIP or SBR builds.
wdir=/scratch/pabosle/e3sm-pclap-skip-p2d

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_skip_p2d.nl
nlFile=/tmp/dcmip12_skip_p2d.nl
outDir=./movies_skip_p2d/

configFlag=
buildFlag=
runFlag=

# 1-day run: nmax = 86400 / 33 ≈ 2618 dyn steps.  Small run, generous timeout.
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
  printf "Configuring standalone Homme (PARTMCSL=ON, SBR_DIAG=OFF, SKIP_PHYS_TO_DYN=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=ON \
    -DPARTMCSL_SBR_DIAG=OFF -DPARTMCSL_SKIP_PHYS_TO_DYN=ON $homme
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
  printf "Running ${execName} (ne=30, partmcsl-on, skip_p2d) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical
  # w/ SMT).  Half-hyperthreaded cap => 448 ranks, one per phys core,
  # SMT siblings idle.  Same fd bump + env var as the other drivers.
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-skip-p2d.txt || true
  printf "Finished (or timed out) skip-p2d A/B.\n"
fi
