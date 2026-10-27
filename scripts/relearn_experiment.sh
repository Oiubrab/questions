#!/usr/bin/env bash
# REVIVAL_PLAN.md G.4: how well and how fast does the brain re-learn when its world changes?
#
# For each configuration (hebb 250 = current working value, hebb 1000 = default; both with
# --cos-reward --reward-scale 0.25):
#   schedule arm: A:10000, L180, A, E180, A, FLEE, A, L90, A (5000 bars each) = 50,000 bars
#   fresh controls: each world learned from scratch for 5000 bars (same length as a block)
# then q/programs/relearn_report.q (every switch metric, fresh times, re-learning ratio,
# savings across returns to A, paired hebb 250 vs 1000).
#
# Usage: scripts/relearn_experiment.sh [K] [TAG]     (defaults: 16 seeds, tag = today's date)
set -u
cd "$(dirname "$0")/.." || exit 2
K=${1:-16}; TAG=${2:-$(date +%Y%m%d)}
IDS=$(seq -s ' ' 1 "$K")
BASE="--cos-reward --reward-scale 0.25"
SCHED="A:10000,L180:5000,A:5000,E180:5000,A:5000,FLEE:5000,A:5000,L90:5000,A:5000"
WORLDS="A L180 E180 FLEE L90"

for H in 250 1000; do
  scripts/run_trials.sh -l "rl_h${H}_sched_${TAG}" -i "$IDS" -- $BASE --hebb "$H" --relearn-schedule "$SCHED" || exit 1
  for W in $WORLDS; do
    scripts/run_trials.sh -l "rl_h${H}_fresh${W}_${TAG}" -i "$IDS" -- $BASE --hebb "$H" --relearn-schedule "${W}:5000" || exit 1
  done
done

group() { echo -n "results/rl_h$1_sched_${TAG}"; for W in $WORLDS; do echo -n " results/rl_h$1_fresh${W}_${TAG}"; done; }
# hebb 1000 is the reference group (the default); hebb 250 is compared against it
q q/programs/relearn_report.q $(group 1000) + $(group 250) < /dev/null 2>&1 | tee "results/rl_report_${TAG}.txt"
