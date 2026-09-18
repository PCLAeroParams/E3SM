#!/bin/bash

# Build/run driver for the partmcsl-enabled DCMIP 1-1 test with EVERY
# partmcsl subroutine compiled out:
#   -DPARTMCSL_SKIP_INIT=ON -DPARTMCSL_SKIP_STEP_FORWARD=ON
#   -DPARTMCSL_SKIP_PHYS_TO_DYN=ON
#
# Fourth rung of the A/B ladder.  With all three skip flags ON,
# partmcsl_init does NOT run at prim_init1 -- no fv_mesh alloc, no
# partmcsl ghost buffers, no C++ src_partition, no init_local_meshes.
# HOMME_ENABLE_PARTMCSL is still ON so the wrapper's init block still
# runs (qsize=8, gfr_init, pg_data alloc, gfr_dyn_to_fv_phys at t=0).
#
# If Q1 in this build matches SL-only quality, the residual leak IS in
# partmcsl_init (Fortran allocations, C++ src_partition_init/init_local_meshes,
# or a side effect on the shared edge/ghost infrastructure).  If Q1 is
# still degraded to the skip-both level, the leak is in the wrapper's
# HOMME_ENABLE_PARTMCSL init block or in compose/CEDR's qsize=8 loading.
#
# Usage:
#   ./run_homme_partmcsl_dcmip_skip_all.sh -c   # configure once
#   ./run_homme_partmcsl_dcmip_skip_all.sh -b   # build
#   ./run_homme_partmcsl_dcmip_skip_all.sh -r   # run 1-day A/B
#
# Output: /scratch/pabosle/e3sm-pclap-skip-all/movies_skip_all/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap-skip-all

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_skip_all.nl
nlFile=/tmp/dcmip12_skip_all.nl
outDir=./movies_skip_all/

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
  printf "Configuring standalone Homme (PARTMCSL=ON, SBR_DIAG=OFF, SKIP_INIT + SKIP_STEP_FORWARD + SKIP_PHYS_TO_DYN all ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=ON \
    -DPARTMCSL_SBR_DIAG=OFF \
    -DPARTMCSL_SKIP_INIT=ON \
    -DPARTMCSL_SKIP_STEP_FORWARD=ON \
    -DPARTMCSL_SKIP_PHYS_TO_DYN=ON $homme
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
  printf "Running ${execName} (ne=30, partmcsl-on, ALL partmcsl subs skipped) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-skip-all.txt || true
  printf "Finished (or timed out) skip-all A/B.\n"
fi
