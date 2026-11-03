#!/usr/bin/env bash
# OBSTACLES_PLAN.md I.4 lesion maps: freeze each trained brain of an arm and block one cell at a time.
#
# For every seed s<id> of ARM: an intact frozen run, then one frozen run per lesioned cell (no route
# leads into that cell), all with the same seed and flags; then q/programs/analysis/lesion_report.q.
#
# Usage: [BARS=2000] [CELLS="r:c ..."] scripts/probes/lesion_map.sh results/ARM [-- extra cat_mouse flags]
#   CELLS default: the sight brain (stacked rows 0-2) and the central brain (rows 6-8) of the
#   default modular brain; the touch brain (rows 3-5) is silent without walls.
#   Output: results/lesion_<ARM>/s<id>/<cell>/ and results/lesion_<ARM>/report.txt
set -u
cd "$(dirname "$0")/../.." || exit 2
ARM=${1%/}; shift
[ "${1:-}" = "--" ] && shift
EXTRA="$*"
BARS=${BARS:-2000}
CELLS=${CELLS:-$(for r in 0 1 2 6 7 8; do for c in $(seq 0 11); do printf "%s:%s " "$r" "$c"; done; done)}
OUT="results/lesion_$(basename "$ARM")"
mkdir -p "$OUT"
jobs_file=$(mktemp)
for sd in "$ARM"/s*/; do
  id=$(basename "$sd"); n=${id#s}
  w="$sd/weights_seed$n.bin"
  [ -f "$w" ] || continue
  for cell in intact $CELLS; do
    les=""; [ "$cell" != intact ] && les="--lesion $cell"
    echo "env q q/programs/cat_mouse.q --seed $n --bars $BARS --no-learning --load-weights $w $les $EXTRA --output-dir $OUT/$id/${cell/:/_} < /dev/null > /dev/null 2>&1" >> "$jobs_file"
  done
done
xargs -P "$(nproc)" -I{} sh -c '{}' < "$jobs_file"
rm -f "$jobs_file"
q q/programs/analysis/lesion_report.q "$OUT" < /dev/null 2>&1 | tee "$OUT/report.txt"
