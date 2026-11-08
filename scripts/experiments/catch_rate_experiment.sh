#!/usr/bin/env bash
# plans/inProgress/CATCH_RATE_PLAN.md tasks E.3-E.4: the catch-rate arms on fresh seeds, then every report.
#
#   open:    each arm, open world, BARS bars          -> target_report.q, compare.q, weights-only spontaneity
#   relearn: each arm, A:10000,L180:5000,A:5000       -> relearn_report.q (paired against the first arm)
#   guard:   GUARD_ARMS + the reactive cat in pillars, rooms and blink 20:20 -> world_report.q, compare.q
#
# Usage: [IDS="17 ... 32"] [BARS=10000] [STAGES="open relearn guard"] [ARMS="name=flags;..."] \
#          scripts/experiments/catch_rate_experiment.sh [TAG]
#   IDS: brain ids = seeds (default 17-32: seeds 1-16 are used up for this question, CATCH_RATE_ANALYSIS Appendix)
#   ARMS: the first is the reference. Default: the plan's §4 arms.
#   RL_SCHED, GUARD_ARMS (names from ARMS), GUARD_BARS can be shrunk for a smoke test.
# Reports go to results/cr_report_TAG/. Runs go to results/cr_<stage>_<arm>_TAG/s<id>/.
set -u
cd "$(dirname "$0")/../.." || exit 2
TAG=${1:-$(date +%Y%m%d)}
IDS=${IDS:-$(seq -s ' ' 17 32)}
BARS=${BARS:-10000}
STAGES=${STAGES:-"open relearn guard"}
ARMS=${ARMS:-"base=;P4=--punish-calls 4;main=--punish-calls 4 --floor 100 --weight-ratio 20000 --decay-mult 0.5;gate=--punish-calls 3 --strategic-gate 20 --floor 100 --weight-ratio 20000 --decay-mult 0.5"}
RL_SCHED=${RL_SCHED:-"A:10000,L180:5000,A:5000"}
GUARD_ARMS=${GUARD_ARMS:-"base main gate"}
GUARD_BARS=${GUARD_BARS:-$BARS}
REP="results/cr_report_${TAG}"; mkdir -p "$REP"

declare -A FLAGS; NAMES=()
IFS=';' read -ra CFGS <<< "$ARMS"
for cfg in "${CFGS[@]}"; do n=${cfg%%=*}; FLAGS[$n]=${cfg#*=}; NAMES+=("$n"); done

for stage in $STAGES; do
  case $stage in
  open)
    DIRS=(); EXP=()
    for n in "${NAMES[@]}"; do
      # shellcheck disable=SC2086  # flag lists are deliberately word-split
      scripts/run_trials.sh -l "cr_open_${n}_${TAG}" -i "$IDS" -- --bars "$BARS" ${FLAGS[$n]} > /dev/null || exit 1
      DIRS+=("results/cr_open_${n}_${TAG}"); EXP+=("cr_open_${n}_${TAG}")
    done
    q q/programs/analysis/target_report.q "${DIRS[@]}" < /dev/null > "$REP/open_target.txt" 2>&1
    q q/programs/analysis/compare.q "${DIRS[@]}" < /dev/null > "$REP/open_compare.txt" 2>&1
    mkdir -p results/model/w
    q scripts/model/export.q results/model/w "${EXP[@]}" < /dev/null > /dev/null 2>&1
    python3 scripts/model/spont_arm.py "${EXP[@]}" > "$REP/open_spontaneity.txt" 2>&1
    cat "$REP/open_target.txt" "$REP/open_spontaneity.txt" ;;
  relearn)
    RA=()
    for n in "${NAMES[@]}"; do
      # shellcheck disable=SC2086
      scripts/run_trials.sh -l "cr_rl_${n}_${TAG}" -i "$IDS" -- ${FLAGS[$n]} --relearn-schedule "$RL_SCHED" > /dev/null || exit 1
      [ ${#RA[@]} -gt 0 ] && RA+=("+")
      RA+=("results/cr_rl_${n}_${TAG}")
    done
    q q/programs/analysis/relearn_report.q "${RA[@]}" < /dev/null > "$REP/relearn.txt" 2>&1
    cat "$REP/relearn.txt" ;;
  guard)
    : > "$REP/guard.txt"
    for w in "pillars=--world pillars" "rooms=--world rooms" "blink=--blink 20:20"; do
      wn=${w%%=*}; wf=${w#*=}; GD=()
      for n in $GUARD_ARMS; do
        # shellcheck disable=SC2086
        scripts/run_trials.sh -l "cr_${wn}_${n}_${TAG}" -i "$IDS" -- --bars "$GUARD_BARS" $wf ${FLAGS[$n]} > /dev/null || exit 1
        GD+=("results/cr_${wn}_${n}_${TAG}")
      done
      # shellcheck disable=SC2086
      scripts/run_trials.sh -l "cr_${wn}_reactive_${TAG}" -i "$IDS" -- --bars "$GUARD_BARS" $wf --policy reactive > /dev/null || exit 1
      { echo "################ $wn ($wf) ################"
        q q/programs/analysis/world_report.q "${GD[@]}" "results/cr_${wn}_reactive_${TAG}" < /dev/null 2>&1
        q q/programs/analysis/compare.q "${GD[@]}" < /dev/null 2>&1 | sed -n '/PAIRED DIFFERENCES/,/CATCHES PER 1000/p'
        echo; } >> "$REP/guard.txt"
    done
    cat "$REP/guard.txt" ;;
  *) echo "unknown stage $stage" >&2; exit 2 ;;
  esac
done
