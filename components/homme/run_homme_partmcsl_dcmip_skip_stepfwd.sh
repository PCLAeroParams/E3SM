#!/bin/bash

# Build/run driver for the partmcsl-enabled DCMIP 1-1 test with BOTH the
# runtime transport calls AND the phys_to_dyn copy-back compiled out:
#   -DPARTMCSL_SKIP_STEP_FORWARD=ON -DPARTMCSL_SKIP_PHYS_TO_DYN=ON.
#
# Third rung of the A/B ladder against run_homme_sl_only.sh,
# run_homme_partmcsl_dcmip.sh, and run_homme_partmcsl_dcmip_skip.sh.  With
# both flags ON, partmcsl code compiles and partmcsl_init runs (so
# pg_data%q is allocated and seeded from the GLL IC), but nothing partmcsl
# is called during the timestep loop.  If Q1 now matches SL-only, the
# residual leak is in the runtime partmcsl calls (step_forward, vertical
# step, permute).  If Q1 is still degraded, the leak is in init or in
# CMake-level side-effects of HOMME_ENABLE_PARTMCSL=ON (Kokkos re-init,
# compose reconfigured, extra tracer loading in SL/CEDR, etc.).
#
# Usage:
#   ./run_homme_partmcsl_dcmip_skip_stepfwd.sh -c   # configure once
#   ./run_homme_partmcsl_dcmip_skip_stepfwd.sh -b   # build
#   ./run_homme_partmcsl_dcmip_skip_stepfwd.sh -r   # run 1-day A/B
#
# Output: /scratch/pabosle/e3sm-pclap-skip-stepfwd/movies_skip_stepfwd/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap-skip-stepfwd

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_skip_stepfwd.nl
nlFile=/tmp/dcmip12_skip_stepfwd.nl
outDir=./movies_skip_stepfwd/

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
  printf "Configuring standalone Homme (PARTMCSL=ON, SBR_DIAG=OFF, SKIP_STEP_FORWARD=ON, SKIP_PHYS_TO_DYN=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=ON \
    -DPARTMCSL_SBR_DIAG=OFF \
    -DPARTMCSL_SKIP_STEP_FORWARD=ON -DPARTMCSL_SKIP_PHYS_TO_DYN=ON $homme
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
  printf "Running ${execName} (ne=30, partmcsl-on, skip step_forward AND phys_to_dyn) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  # Same 448-rank layout + fd bump as the other drivers.
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-skip-stepfwd.txt || true
  printf "Finished (or timed out) skip-stepfwd A/B.\n"
fi
