#!/bin/bash

# Build/run driver for the partmcsl-enabled DCMIP 1-1 test with the FOUR
# skip flags all on:
#   -DPARTMCSL_SKIP_INIT=ON  -DPARTMCSL_SKIP_STEP_FORWARD=ON
#   -DPARTMCSL_SKIP_PHYS_TO_DYN=ON -DPARTMCSL_SKIP_WRAPPER_INIT=ON
#
# Fifth (and hopefully last) rung of the A/B ladder.  With all four flags
# ON, the ONLY partmcsl-related things that survive are:
#   - `use partmcsl_advection_mod, only: ...` Fortran imports in
#     prim_step and prim_finalize (no runtime call happens)
#   - CMake-level: partmcsl C++ library is built and linked into
#     theta-l-nlev64-native
# Everything else -- init, wrapper init (gfr_init, pg_data alloc,
# gfr_dyn_to_fv_phys seed), step_forward/vertical_step/permute,
# phys_to_dyn copy-back -- is compiled out.
#
# Diagnostic:
#   * Q1 matches sl_only_q8 => wrapper init WAS the 40% residual
#     (gfr_init or gfr_dyn_to_fv_phys has a side effect on SL/CEDR path)
#   * Q1 still degraded => residual is at the CMake/link/import level
#     (partmcsl F90 module imports, or something in compose SL that
#     branches on HOMME_ENABLE_PARTMCSL being defined)
#
# Usage:
#   ./run_homme_partmcsl_dcmip_skip_all_wrapper.sh -c   # configure once
#   ./run_homme_partmcsl_dcmip_skip_all_wrapper.sh -b   # build
#   ./run_homme_partmcsl_dcmip_skip_all_wrapper.sh -r   # run 1-day A/B
#
# Output: /scratch/pabosle/e3sm-pclap-skip-all-wrapper/movies_skip_all_wrapper/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap-skip-all-wrapper

execName=theta-l-nlev64-native

baseNamelist=$homme/dcmip12_skip_all_wrapper.nl
nlFile=/tmp/dcmip12_skip_all_wrapper.nl
outDir=./movies_skip_all_wrapper/

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
  printf "Configuring standalone Homme (PARTMCSL=ON, SBR_DIAG=OFF, all four SKIP flags ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DHOMME_ENABLE_PARTMCSL=ON \
    -DPARTMCSL_SBR_DIAG=OFF \
    -DPARTMCSL_SKIP_INIT=ON \
    -DPARTMCSL_SKIP_STEP_FORWARD=ON \
    -DPARTMCSL_SKIP_PHYS_TO_DYN=ON \
    -DPARTMCSL_SKIP_WRAPPER_INIT=ON $homme
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
  printf "Running ${execName} (ne=30, partmcsl-on, ALL FOUR skip flags ON) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  # cee-compute005: 16 sockets * 28 phys cores = 448 phys (896 logical).
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-skip-all-wrapper.txt || true
  printf "Finished (or timed out) skip-all-wrapper A/B.\n"
fi
