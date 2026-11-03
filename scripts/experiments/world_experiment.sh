#!/usr/bin/env bash
# OBSTACLES_PLAN.md pilot: the brain against scripted baselines in each harder world.
#
# For each world: the brain (working configuration) and the baselines random, reactive, persist and
# greedy, same seeds. Then q/programs/analysis/world_report.q per world, appended to one report.
#
# Usage: [WIRE=angle] [BARS=10000] [BRAIN_ARMS=...] scripts/experiments/world_experiment.sh [K] [TAG]
#   K: seeds 1..K (default 16); TAG: label suffix (default today's date).
#   WIRE: output wiring for the brain arms (--wiring; default angle, the best in §6).
#   BRAIN_ARMS: "name=extra flags" per brain arm, ;-separated (default below: plain brain,
#   efference copy, meta-brain ablated, and no direct reward while hidden).
set -u
cd "$(dirname "$0")/../.." || exit 2
K=${1:-16}; TAG=${2:-$(date +%Y%m%d)}; WIRE=${WIRE:-angle}; BARS=${BARS:-10000}
IDS=$(seq -s ' ' 1 "$K")
BRAIN="--cos-reward --reward-scale 0.25 --hebb 250 --wiring $WIRE"
IFS=';' read -ra BRAIN_ARMS <<< "${BRAIN_ARMS:-brain=;efference=--efference;noMeta=--ablate-meta;noHidRew=--hidden-reward none}"
# name=world flags (all wrapped)
WORLDS=("open=" "blink=--blink 20:20" "seethru=--world pillars --see-through" "pillars=--world pillars" "rooms=--world rooms")
REPORT="results/w_report_${TAG}.txt"; : > "$REPORT"
for w in "${WORLDS[@]}"; do
  name=${w%%=*}; wf=${w#*=}
  ARMS=()
  for b in "${BRAIN_ARMS[@]}"; do
    bn=${b%%=*}; bf=${b#*=}
    # shellcheck disable=SC2086  # flag lists are deliberately word-split
    scripts/run_trials.sh -l "w_${name}_${bn}_${TAG}" -i "$IDS" -- --bars "$BARS" --wrap-world $wf $BRAIN $bf > /dev/null || exit 1
    ARMS+=("results/w_${name}_${bn}_${TAG}")
  done
  for p in random reactive persist greedy; do
    # shellcheck disable=SC2086
    scripts/run_trials.sh -l "w_${name}_${p}_${TAG}" -i "$IDS" -- --bars "$BARS" --wrap-world $wf --policy $p > /dev/null || exit 1
    ARMS+=("results/w_${name}_${p}_${TAG}")
  done
  { echo "################ WORLD $name ($wf) ################"; q q/programs/analysis/world_report.q "${ARMS[@]}" < /dev/null 2>&1; echo; } >> "$REPORT"
done
cat "$REPORT"
