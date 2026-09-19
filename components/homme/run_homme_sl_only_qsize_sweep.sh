#!/bin/bash

# qsize sweep at qsize={5,6,7} for the sl-only PARTMCSL_CONST_MIRROR
# diagnostic.  Reuses the sl_only_q8_const build tree since the flag
# is compile-time but qsize is runtime.
#
# Purpose: bracket where the Q1 fragmentation kicks in.  q4 baseline is
# clean; q8 fragments (~32% Q1 mass loss).  Does q5 already fragment
# (one extra trivial tracer is enough)?  Does damage grow with qsize?
#
# Prereqs:
#   - run_homme_sl_only_q8_const.sh has been run with -c -b at least once
#     (build tree at /scratch/pabosle/e3sm-pclap-sl-only-q8-const/ with
#     the executable already built).
#
# Usage:
#   ./run_homme_sl_only_qsize_sweep.sh                  # runs all of 5,6,7
#   ./run_homme_sl_only_qsize_sweep.sh 5                # just qsize=5
#   ./run_homme_sl_only_qsize_sweep.sh 5 7              # 5 and 7
#
# Output: /scratch/pabosle/e3sm-pclap-sl-only-q8-const/movies_sl_only_qN_const/dcmip2012_test1_11.nc

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
wdir=/scratch/pabosle/e3sm-pclap-sl-only-q8-const

execName=theta-l-nlev64-native
runTimeout="1800s"

if [ ! -x "$wdir/test_execs/$execName/$execName" ]; then
  printf "ERROR: executable not found at %s\n" "$wdir/test_execs/$execName/$execName" >&2
  printf "Run: ./run_homme_sl_only_q8_const.sh -c -b   first.\n" >&2
  exit 1
fi

# Default sweep points if no args given.
sweep=("$@")
if [ ${#sweep[@]} -eq 0 ]; then
  sweep=(5 6 7)
fi

for qs in "${sweep[@]}"; do
  case $qs in
    5|6|7|8) ;;
    *) printf "Skipping qsize=%s (only 5,6,7,8 are supported by this sweep)\n" "$qs"; continue ;;
  esac

  baseNamelist=$homme/dcmip12_sl_only_q${qs}_const.nl
  if [ ! -f "$baseNamelist" ]; then
    printf "ERROR: namelist missing: %s\n" "$baseNamelist" >&2
    continue
  fi

  outDir=./movies_sl_only_q${qs}_const/
  nlFile=/tmp/dcmip12_sl_only_q${qs}_const.nl
  sed "s|output_dir *=.*|output_dir        = \"${outDir}\"|" "$baseNamelist" > "$nlFile"

  printf "\n=== Running qsize=%s (const) ===\n" "$qs"
  mkdir -p "$wdir/$outDir"
  cd "$wdir"
  ulimit -n $(ulimit -n -H)
  export OMPI_MCA_opal_set_max_sys_limits=1
  timeout --foreground --kill-after=10s $runTimeout \
    mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n 448 \
      "$wdir/test_execs/$execName/$execName" < "$nlFile" 2>&1 \
    | tee "homme-out-sl-only-q${qs}-const.txt" || true
  printf "=== Finished qsize=%s ===\n" "$qs"
done
