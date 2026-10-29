#!/usr/bin/env bash
# REVIVAL_PLAN.md G.4: how well and how fast does the brain re-learn when its world changes?
#
# For each configuration:
#   schedule arm: A:10000, L180, A, E180, A, FLEE, A, L90, A (5000 bars each) = 50,000 bars
#   fresh controls: each world learned from scratch for 5000 bars (same length as a block)
# then q/programs/relearn_report.q (every switch metric, fresh times, re-learning ratio,
# savings across returns to A, paired comparisons against the first configuration).
#
# Usage: [CONFIGS="name=flags;name2=flags2"] scripts/relearn_experiment.sh [K] [TAG]
#   K: seeds 1..K (default 16); TAG: label suffix (default today's date).
#   CONFIGS default: h1000 and h250 with --cos-reward --reward-scale 0.25 (the 2026-10-27 run);
#   the first configuration is the reference for paired comparisons.
# Example (wrapped world, weight-ratio sweep):
#   CONFIGS="r80000=--cos-reward --reward-scale 0.25 --hebb 250 --wrap-world;r1000=--cos-reward --reward-scale 0.25 --hebb 250 --wrap-world --weight-ratio 1000" \
#     scripts/relearn_experiment.sh 16 20261028
set -u
cd "$(dirname "$0")/.." || exit 2
K=${1:-16}; TAG=${2:-$(date +%Y%m%d)}
IDS=$(seq -s ' ' 1 "$K")
CONFIGS=${CONFIGS:-"h1000=--cos-reward --reward-scale 0.25 --hebb 1000;h250=--cos-reward --reward-scale 0.25 --hebb 250"}
SCHED="A:10000,L180:5000,A:5000,E180:5000,A:5000,FLEE:5000,A:5000,L90:5000,A:5000"
WORLDS="A L180 E180 FLEE L90"

REPORT_ARGS=()
IFS=';' read -ra CFGS <<< "$CONFIGS"
for cfg in "${CFGS[@]}"; do
  name=${cfg%%=*}; flags=${cfg#*=}
  # shellcheck disable=SC2086  # flags is a deliberately word-split option list
  scripts/run_trials.sh -l "rl_${name}_sched_${TAG}" -i "$IDS" -- $flags --relearn-schedule "$SCHED" || exit 1
  [ ${#REPORT_ARGS[@]} -gt 0 ] && REPORT_ARGS+=("+")
  REPORT_ARGS+=("results/rl_${name}_sched_${TAG}")
  for W in $WORLDS; do
    # shellcheck disable=SC2086
    scripts/run_trials.sh -l "rl_${name}_fresh${W}_${TAG}" -i "$IDS" -- $flags --relearn-schedule "${W}:5000" || exit 1
    REPORT_ARGS+=("results/rl_${name}_fresh${W}_${TAG}")
  done
done

q q/programs/relearn_report.q "${REPORT_ARGS[@]}" < /dev/null 2>&1 | tee "results/rl_report_${TAG}.txt"
