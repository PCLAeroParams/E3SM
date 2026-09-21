#!/bin/bash

# Build/run driver for the DCMIP 2012 Test 1-1 arrival-tiling diagnostic.
# Enables PARTMCSL_DIAG_TILING which prints, once per tracer time step,
# statistics of the ratio (Sum_si ov_area(si -> dj) / a_dst(dj) - 1) over
# all destination cells:
#
#   PARTMCSL_DIAG_TILING nstep=<n>  (sum_ov/a_dst - 1) min/max/mean/rms:<...>
#
# Interpretation:
#   - min/max near 0, mean~0, rms~0    => arrival tiling is machine-precision
#                                          exact; the ~5e-4 Q5 mass drift is
#                                          NOT from geometric tiling failure.
#   - mean systematically negative      => halo stencil truncates some parcels
#                                          (contributions from parcels reaching
#                                          non-neighbor cells are lost).
#   - rms grows with time, mean ~ 0     => unbiased forward-Euler drift of
#                                          parcel geometry off the sphere.
#   - mean drifts monotonically         => biased geometric drift under the
#                                          specific flow structure.
#
# Also enables PARTMCSL_DIAG_MASS so the mass-conservation lines are still
# printed at output snapshots -- useful to cross-correlate.
#
# Separate build tree because PARTMCSL_DIAG_TILING is compile-time.
#
# Usage:
#   ./run_homme_partmcsl_diag_tiling.sh -c    # configure
#   ./run_homme_partmcsl_diag_tiling.sh -b    # build
#   ./run_homme_partmcsl_diag_tiling.sh -r    # run
#
# Output:
#   /scratch/pabosle/e3sm-pclap-dcmip-diag-tiling/movies_dcmip_diag_tiling/dcmip2012_test1_11.nc
#   /scratch/pabosle/e3sm-pclap-dcmip-diag-tiling/homme-out-dcmip-diag-tiling.txt

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
wdir=/scratch/pabosle/e3sm-pclap-dcmip-diag-tiling

execName=theta-l-nlev64-native

baseNamelist=$homme/partmcsl_dcmip12_transport.nl
nlFile=/tmp/partmcsl_dcmip_diag_tiling.nl
outDir=./movies_dcmip_diag_tiling/

configFlag=
buildFlag=
runFlag=

runTimeout="36000s"

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
  printf "Configuring standalone Homme (PARTMCSL_DIAG_MASS=ON, PARTMCSL_DIAG_TILING=ON)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=OFF \
    -DPARTMCSL_DIAG_MASS=ON \
    -DPARTMCSL_DIAG_TILING=ON $homme
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
  printf "Running ${execName} (ne=30, tiling diagnostic) -> ${outDir}\n"
  mkdir -p $wdir/$outDir
  cd $wdir
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  host=$(hostname -s)
  case $host in
    cee-compute046*) mpi_map='ppr:60:socket:PE=1'; mpi_n=480 ;;
    cee-compute005*) mpi_map='ppr:28:socket:PE=1'; mpi_n=448 ;;
    *)               mpi_map='ppr:28:socket:PE=1'; mpi_n=448
                     printf "WARN: unknown host %s, defaulting to cee-compute005 layout (28x16=448)\n" "$host" ;;
  esac
  printf "Launching on %s: --map-by %s --n %d\n" "$host" "$mpi_map" "$mpi_n"
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by $mpi_map --bind-to core --n $mpi_n \
      $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
    | tee homme-out-dcmip-diag-tiling.txt || true
  printf "Finished (or timed out) ne=30 tiling diag run.\n"
  printf "\nExtract tiling diagnostic lines with:\n"
  printf "  grep PARTMCSL_DIAG_TILING %s/homme-out-dcmip-diag-tiling.txt\n" "$wdir"
  printf "Extract mass diagnostic lines with:\n"
  printf "  grep PARTMCSL_DIAG_MASS %s/homme-out-dcmip-diag-tiling.txt\n" "$wdir"
fi
