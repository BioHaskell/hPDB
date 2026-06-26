#!/bin/bash
# hPDB parallel-parse Rg scaling on 1HTQ.pdb, N1/N2/N4/N8, best-of-TRIALS.
# Parses GHC +RTS -s elapsed (Total = full parse+fold, MUT = less GC).
# Run UNDER benchlock.py (fcntl lock, interlocks with bench-utils).
set -u
RGPAR="$1"; FIX="$2"; TRIALS="${3:-3}"
echo "hPDB parallel Rg: $FIX, best of $TRIALS, nproc=$(nproc)"
echo "correctness: $(env -u GHCRTS "$RGPAR" "$FIX" +RTS -N1 2>/dev/null | grep hPDB)"
echo
printf "%-4s %12s %12s %12s\n" "N" "total_s" "MUT_s" "speedup"
base=""
for N in 1 2 4 8; do
  bt=""; bm=""
  for t in $(seq 1 "$TRIALS"); do
    s=$(env -u GHCRTS "$RGPAR" "$FIX" +RTS "-N$N" -s 2>&1)
    total=$(echo "$s" | awk '/Total   time/ {gsub(/[()s]/,"",$5); print $5}')
    mut=$(  echo "$s" | awk '/MUT     time/  {gsub(/[()s]/,"",$5); print $5}')
    [ -z "$total" ] && continue
    if [ -z "$bt" ] || awk "BEGIN{exit !($total < $bt)}"; then bt="$total"; bm="$mut"; fi
  done
  [ -z "$base" ] && base="$bt"
  spd=$(awk "BEGIN{printf \"%.2f\", $base/$bt}")
  printf "%-4s %12s %12s %11sx\n" "$N" "$bt" "$bm" "$spd"
done
