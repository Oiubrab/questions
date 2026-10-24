#!/usr/bin/env bash
# REVIVAL_PLAN.md Phase 3: controlled re-test of the reaction-dissociation hypothesis.
#
# 1. Train K brains from scratch (direct + meta rewards), TRAIN_BARS each  -> results/p3_train/
# 2. From each trained brain, run 4 arms for ARM_BARS, same run seed per brain across arms:
#      A  continue training       (direct + meta)                       -> results/p3_A/
#      B  meta-only               --meta-only                           -> results/p3_B/
#      C  no strategic rewards    --meta-only --no-meta-rewards         -> results/p3_C/
#      D  meta-brain ablated      --meta-only --ablate-meta             -> results/p3_D/
# 3. Paired comparison vs arm B (the branch's "meta-only" condition) and vs arm A.
#
# Usage: scripts/phase3_experiment.sh [K] [TRAIN_BARS] [ARM_BARS]   (defaults 16 20000 10000)
# Skips training if results/p3_train already has K summaries (so it can be resumed).
set -u
cd "$(dirname "$0")/.." || exit 2
K=${1:-16}; TRAIN_BARS=${2:-20000}; ARM_BARS=${3:-10000}
IDS=$(seq -s ' ' 101 $((100 + K)))
W='results/p3_train/s{id}/weights_seed{id}.bin'

if [ "$(ls results/p3_train/s*/summary.csv 2>/dev/null | wc -l)" -lt "$K" ]; then
  scripts/run_trials.sh -l p3_train -i "$IDS" -- --bars "$TRAIN_BARS" || exit 1
fi
scripts/run_trials.sh -l p3_A -i "$IDS" -o 100 -- --bars "$ARM_BARS" --load-weights "$W" || exit 1
scripts/run_trials.sh -l p3_B -i "$IDS" -o 100 -- --bars "$ARM_BARS" --load-weights "$W" --meta-only || exit 1
scripts/run_trials.sh -l p3_C -i "$IDS" -o 100 -- --bars "$ARM_BARS" --load-weights "$W" --meta-only --no-meta-rewards || exit 1
scripts/run_trials.sh -l p3_D -i "$IDS" -o 100 -- --bars "$ARM_BARS" --load-weights "$W" --meta-only --ablate-meta || exit 1

{
  echo "##### Reference: arm B (meta-only)"
  q q/programs/compare.q results/p3_B results/p3_A results/p3_C results/p3_D < /dev/null
  echo
  echo "##### Reference: arm A (continue training); training run included for epoch curves"
  q q/programs/compare.q results/p3_A results/p3_B results/p3_train < /dev/null
} 2>&1 | tee results/p3_report.txt
