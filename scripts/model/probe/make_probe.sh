#!/usr/bin/env bash
# Build the ledger's hooked bar loop from the current q/programs/cat_mouse/bar_loop.q (CATCH_RATE_ANALYSIS.md §2).
# Inserts three calls defined in probe_wrap.q: .p.barStart[] after the distance is measured, .p.afterProp[] after
# the brains are stacked again, .p.barEnd[] before the log is appended. Fails if an anchor is missing.
set -eu
cd "$(dirname "$0")/../../.." || exit 2
OUT=results/model/probe; mkdir -p "$OUT"
awk '
  {print}
  /prevDist: \.vision\.toroidalDist/      {print "  .p.barStart[];"; a++}
  /prim: \.mod\.stack \(sightB;touchB;prim\)/ {print "  .p.afterProp[];"; b++}
  /stSeen\[bar-1\]: lastSeenSlice\]/      {print "  .p.barEnd[];"; c++}
  END {if (a!=1||b!=1||c!=1) {print "anchor count wrong: " a " " b " " c > "/dev/stderr"; exit 1}}
' q/programs/cat_mouse/bar_loop.q > "$OUT/bar_loop_probe.q"
echo "wrote $OUT/bar_loop_probe.q"
