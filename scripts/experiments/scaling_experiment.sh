#!/usr/bin/env bash
# plans/inProgress/CATCH_RATE_PLAN.md task R.5: derived constants against today's fixed ones at several brain sizes.
# One catch_rate_experiment.sh run per size (open world + re-learning), each paired against its own fixed arm.
# Usage: [IDS="33 ... 48"] [SIZES="3 6 9"] scripts/experiments/scaling_experiment.sh [TAG]
# Reports: results/cr_report_<TAG>_c<rows>/
set -u
cd "$(dirname "$0")/../.." || exit 2
TAG=${1:-$(date +%Y%m%d)}
export IDS=${IDS:-$(seq -s ' ' 33 48)}
SIZES=${SIZES:-"3 6 9"}
FIXED="--steps-per-bar 12 --punish-calls 4 --hebb 250"     # the constants tested on 2026-11-08
for r in $SIZES; do
  C="--central-rows $r"
  if [ "$r" = 3 ]; then
    A="c3_fixed=$C $FIXED;c3_derived=$C"
  else
    A="c${r}_fixed=$C $FIXED;c${r}_steps=$C --punish-calls 4 --hebb 250;c${r}_derived=$C"
  fi
  ARMS="$A" STAGES="open relearn" scripts/experiments/catch_rate_experiment.sh "${TAG}_c$r" || exit 1
done
