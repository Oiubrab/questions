#!/usr/bin/env bash
# Run a named substrate-lab sweep in parallel, then summarise it.
#   scripts/lab_sweep.sh SWEEP          (JOBS=n to limit parallelism)
# Output: results/lab/SWEEP/<config>/{metrics,summary}.csv, results/lab/SWEEP/report.txt
#
# Sweeps:
#   s3  REVIVAL_PLAN.md task S.3 — closed system, no input. 16x16, starting pressure 0.3,
#       3000 steps, decay every 60. Full factorial over wrap x bias x temp x order x hebb,
#       3 seeds each. Plus the current brain's surroundings (6x12, walls, sink, downward bias).
set -u
cd "$(dirname "$0")/.." || exit 2
SWEEP=${1:?usage: $0 SWEEP}
JOBS=${JOBS:-$(nproc)}
DIR=results/lab/$SWEEP
mkdir -p "$DIR"

case $SWEEP in
  s3)
    COMMON="--rows 16 --cols 16 --sink 0 --fill 0.3 --steps 3000 --sample 50 --decay 60"
    for wrap in 0 1; do for bias in down flat; do for temp in 1 0.3 0; do
    for order in rowmajor random; do for hebb in 1000 0; do for seed in 1 2 3; do
      echo "w${wrap}_${bias}_t${temp}_${order}_h${hebb}_s${seed}|$COMMON --wrap $wrap --bias $bias --temp $temp --order $order --hebb $hebb --seed $seed"
    done; done; done; done; done; done
    for seed in 1 2 3; do
      echo "current_s${seed}|--rows 6 --cols 12 --wrap 0 --bias down --temp 1 --order rowmajor --hebb 1000 --sink 1 --fill 0.3 --steps 3000 --sample 50 --decay 60 --seed $seed"
    done
    ;;
  *) echo "unknown sweep: $SWEEP" >&2; exit 2 ;;
esac > "$DIR/jobs.txt"

start=$SECONDS
while IFS='|' read -r name args; do
  while [ "$(jobs -rp | wc -l)" -ge "$JOBS" ]; do wait -n; done
  (
    mkdir -p "$DIR/$name"
    # shellcheck disable=SC2086  # args is a deliberately word-split option list
    q q/programs/substrate_lab.q $args --out "$DIR/$name/" < /dev/null > "$DIR/$name/stdout.txt" 2>&1 \
      || echo "FAILED: $name (see $DIR/$name/stdout.txt)" >&2
  ) &
done < "$DIR/jobs.txt"
wait
echo "$SWEEP: $(wc -l < "$DIR/jobs.txt") runs in $((SECONDS-start))s"
q q/programs/lab_report.q "$DIR" < /dev/null | tee "$DIR/report.txt"
