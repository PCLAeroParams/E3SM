#!/bin/bash

# Run the qsize=5, exact-mirror diagnostic (q(5) = q(1) at t=0).
# Reuses the sl_only_q8 (mirror) build tree -- no rebuild needed.
#
# Purpose: distinguish whether duplicate DCMIP content in slot 5 alone
# triggers the Q1 fragmentation, or whether the qsize=8 result is
# something specific to higher tracer counts.
#
# Prereqs:
#   - run_homme_sl_only_q8.sh has been run with -c -b at least once
#     (build tree at /scratch/pabosle/e3sm-pclap-sl-only-q8/).
#
# Usage:
#   ./run_homme_sl_only_q5_mirror.sh -r
#
# Output: /scratch/pabosle/e3sm-pclap-sl-only-q8/movies_sl_only_q5_mirror/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
wdir=/scratch/pabosle/e3sm-pclap-sl-only-q8

execName=theta-l-nlev64-native
runTimeout="1800s"

baseNamelist=$homme/dcmip12_sl_only_q5_mirror.nl
nlFile=/tmp/dcmip12_sl_only_q5_mirror.nl
outDir=./movies_sl_only_q5_mirror/

if [ ! -x "$wdir/test_execs/$execName/$execName" ]; then
  printf "ERROR: executable not found at %s\n" "$wdir/test_execs/$execName/$execName" >&2
  printf "Run: ./run_homme_sl_only_q8.sh -c -b   first.\n" >&2
  exit 1
fi

runFlag=1
while getopts 'r' OPTION
do
  case $OPTION in
    r) runFlag=1 ;;
    ?) printf "Usage: %s: [-r]\n" $(basename $0) >&2; exit 2 ;;
  esac
done

sed "s|output_dir *=.*|output_dir        = \"${outDir}\"|" "$baseNamelist" > "$nlFile"

if [ "$runFlag" ]; then
  printf "Running qsize=5 exact mirror (q5 = q1)  ->  %s\n" "$outDir"
  mkdir -p "$wdir/$outDir"
  cd "$wdir"
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      "$wdir/test_execs/$execName/$execName" < "$nlFile" 2>&1 \
    | tee homme-out-sl-only-q5-mirror.txt || true
  printf "Finished (or timed out) sl-only q5 mirror diagnostic.\n"
fi
