#!/bin/bash

# Build/run driver for the original DCMIP 2012 Test 1-1 (3D deformational
# flow) with PartMCSL Q5 alongside SL Q.  Sweeps ne = {30, 120} on the
# theta-l-nlev64-native target with a 12-day integration and daily output.
#
# This is the "default" (non-diagnostic) build path: PARTMCSL_SBR_DIAG must
# be OFF so `dcmip2012_test1_1` runs the real deformational winds (not the
# SBR override).  Guard your build dir separately from the SBR/VT builds.
#
# Usage:
#   ./run_homme_partmcsl_dcmip.sh -c              # configure once (SBR_DIAG=OFF)
#   ./run_homme_partmcsl_dcmip.sh -b              # build theta-l-nlev64-native
#   ./run_homme_partmcsl_dcmip.sh -r              # run all ne targets
#   ./run_homme_partmcsl_dcmip.sh -n "30" -r      # subset via -n
#
# Output: /workspace/movies_dcmip_ne{NE}/dcmip2012_test1_11.nc  (per ne).

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/cee-compute.cmake
# Separate build tree from the SBR/VT one so we can keep PARTMCSL_SBR_DIAG=OFF
# here without disturbing the SBR sweep build.  Change if you want to share.
wdir=/scratch/pabosle/e3sm-pclap-dcmip

# Default ne sweep — override with -n "30 120" or a subset.
neList="30 120"

# Same theta-l-nlev64-native build target for both ne values (nlev is
# compile-time; ne is namelist).
execName=theta-l-nlev64-native

baseNamelist=$homme/partmcsl_dcmip12_transport.nl

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFilePrefix=batch_run_dcmip
nnodes=1
wtime="03:00:00"
res=flight-cldera
acct=fy210162
ntasks=448
# Full 12-day run at ne=120 is the slow end.  Bump if it times out.
runTimeout="10800s"

while getopts 'cbrsn:' OPTION
do
  case $OPTION in
    c) configFlag=1 ;;
    b) buildFlag=1 ;;
    r) runFlag=1 ;;
    s) submitFlag=1 ;;
    n) neList=$OPTARG ;;
    ?) printf "Usage: %s: [-cbrs] [-n \"ne1 ne2 ...\"]\n" $(basename $0) >&2
       exit 2
       ;;
  esac
done
shift $(($OPTIND -1))

if [ "$configFlag" ]
then
  printf "Configuring standalone Homme for DCMIP deformational (PARTMCSL_SBR_DIAG=OFF)\n"
  mkdir -p $wdir
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG=OFF $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  cd $wdir
  make -j 24 $execName
fi

for ne in $neList
do
  nlFile=/tmp/partmcsl_dcmip_ne${ne}.nl
  outDir=./movies_dcmip_ne${ne}/

  # Per-ne overrides:  ne, output_dir.  tstep stays at the deformational
  # default (33 s -> CFL ~0.03 at ne=120 for tracer step 66 s).  If ne=120
  # ever becomes marginal, drop tstep here.  Everything else is inherited
  # from partmcsl_dcmip12_transport.nl.
  sed -e "s|^ *ne *=.*|  ne                = ${ne}|" \
      -e "s|output_dir *=.*|output_dir        = \"${outDir}\"|" \
      $baseNamelist > $nlFile

  if [ "$submitFlag" ]
  then
    jobFile=${jobFilePrefix}_ne${ne}.cmd
    printf "creating job command for ne=${ne}: ${jobFile}\n"
    cat <<EOF > $jobFile
#!/bin/bash
#SBATCH -N $nnodes
#SBATCH -t $wtime
#SBATCH -A $acct
#SBATCH -n $ntasks
#SBATCH --reservation $res
mkdir -p $wdir/$outDir
cd $wdir
mpirun --map-by core --bind-to core --n $ntasks $wdir/test_execs/$execName/$execName < $nlFile
EOF
    chmod +x $jobFile
    cat $jobFile
  fi

  if [ "$runFlag" ]
  then
    printf "Running ${execName} (ne=${ne}) -> ${outDir}\n"
    mkdir -p $wdir/$outDir
    cd $wdir
    # timeout + `|| true` guards against the SLMM ~g_csl_mpi hang at finalize
    # (documented in partmcsl_compose_hang_handoff.md).  Outputs are already
    # flushed by then, so we bound each run and continue the loop regardless.
    timeout --kill-after=10s $runTimeout \
      mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n $ntasks \
        $wdir/test_execs/$execName/$execName < $nlFile 2>&1 \
      | tee homme-out-dcmip-ne${ne}.txt || true
    printf "Finished (or timed out) ne=${ne}; continuing.\n"
  fi
done
