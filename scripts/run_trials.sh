#!/usr/bin/env bash
# Run q/programs/cat_mouse.q for several brain ids in parallel.
#
# Usage (any cwd):
#   scripts/run_trials.sh -l LABEL -i "101 102 103" [-o SEED_OFFSET] [-j JOBS] [-- cat_mouse flags...]
#
#   -l LABEL        output goes to results/LABEL/s<id>/
#   -i IDS          space-separated brain ids
#   -o OFFSET       run seed = id + OFFSET (default 0); lets several arms reuse ids with fresh RNG
#   -j JOBS         parallel processes (default: nproc)
#   after --        extra flags for cat_mouse.q; the literal {id} is replaced by the brain id
#
# Example (REVIVAL_PLAN.md Phase 3, arm B):
#   scripts/run_trials.sh -l arm_B -i "$(seq -s ' ' 101 116)" -o 100 -- \
#     --bars 10000 --meta-only --load-weights 'results/train/s{id}/weights_seed{id}.bin'
#
# Each run writes summary.csv and epoch_summary.csv; analyse with q/programs/compare.q.
set -u
cd "$(dirname "$0")/.." || exit 2

LABEL=""; IDS=""; OFFSET=0; JOBS=$(nproc)
while getopts "l:i:o:j:" opt; do
  case $opt in
    l) LABEL=$OPTARG ;;
    i) IDS=$OPTARG ;;
    o) OFFSET=$OPTARG ;;
    j) JOBS=$OPTARG ;;
    *) echo "bad option" >&2; exit 2 ;;
  esac
done
shift $((OPTIND-1))
[ "${1:-}" = "--" ] && shift
FLAGS=("$@")
if [ -z "$LABEL" ] || [ -z "$IDS" ]; then
  echo "usage: $0 -l LABEL -i \"IDS\" [-o OFFSET] [-j JOBS] [-- flags]" >&2; exit 2
fi

run_one() {
  local id=$1 out="results/$LABEL/s$1"
  local seed=$((id + OFFSET))
  local args=("${FLAGS[@]//'{id}'/$id}")
  mkdir -p "$out"
  q q/programs/cat_mouse.q --seed "$seed" --output-dir "$out/" "${args[@]}" \
    < /dev/null > "$out/stdout.txt" 2>&1
}

start=$SECONDS
for id in $IDS; do
  while [ "$(jobs -rp | wc -l)" -ge "$JOBS" ]; do wait -n; done
  run_one "$id" &
done
wait

failed=0
for id in $IDS; do
  if [ ! -f "results/$LABEL/s$id/summary.csv" ]; then
    echo "FAILED: id $id (see results/$LABEL/s$id/stdout.txt)" >&2
    failed=1
  fi
done
echo "$LABEL: $(echo $IDS | wc -w) runs in $((SECONDS-start))s"
exit $failed
