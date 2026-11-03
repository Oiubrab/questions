#!/usr/bin/env bash
# OBSTACLES_PLAN.md close-out batch: every world rung against every baseline, ablations, harder goals
# (W.5), generalisation (W.6), curriculum and untrained brains for the structure probes.
# Resumable: an arm whose 16 runs all have summary.csv is skipped. ~2 h on 16 cores (planner arms are slow).
# Don't edit cat_mouse.q while it runs (or run it from a copy of q/ and scripts/).
# Usage: [T=20261103] scripts/obstacles_closeout.sh
set -u
cd "$(dirname "$0")/.." || exit 2
IDS=$(seq -s ' ' 1 16); T=${T:-20261103}; B=10000
rt(){ local label=$1; shift
  [ "$(ls results/${label}_$T/*/summary.csv 2>/dev/null | wc -l)" -eq 16 ] && return 0
  scripts/run_trials.sh -l "${label}_$T" -i "$IDS" "$@" > /dev/null || { echo "FAILED $label"; exit 1; }; }
REP=results/obs_report_$T.txt; : > $REP
# ---- 1. world rungs: brain (default), single brain, scripted baselines ----
WORLDS=("open=" "blink5=--blink 20:5" "blink20=--blink 20:20" "blink50=--blink 20:50" "seethru=--world pillars --see-through" "pillars=--world pillars" "wall=--world wall" "rooms=--world rooms" "random=--world random")
for w in "${WORLDS[@]}"; do
  n=${w%%=*}; f=${w#*=}; LS=""
  [ "$n" = blink20 ] || [ "$n" = pillars ] && LS="--log-state"
  # shellcheck disable=SC2086
  rt "o_${n}_brain" -- --bars $B $f $LS
  # shellcheck disable=SC2086
  rt "o_${n}_single" -- --bars $B $f --single-brain
  for p in random reactive persist greedy planner; do
    # shellcheck disable=SC2086
    rt "o_${n}_$p" -- --bars $B $f --policy $p
  done
  A=(results/o_${n}_brain_$T results/o_${n}_single_$T)
  case $n in seethru|pillars|wall|rooms|random)
    # shellcheck disable=SC2086
    rt "o_${n}_noTouch" -- --bars $B $f --no-touch; A+=(results/o_${n}_noTouch_$T);; esac
  case $n in blink20|pillars|rooms)
    # shellcheck disable=SC2086
    rt "o_${n}_spont" -- --bars $B $f --spontaneous 0.05
    # shellcheck disable=SC2086
    rt "o_${n}_hidCos" -- --bars $B $f --hidden-reward cos
    # shellcheck disable=SC2086
    rt "o_${n}_noMeta" -- --bars $B $f --ablate-meta
    A+=(results/o_${n}_spont_$T results/o_${n}_hidCos_$T results/o_${n}_noMeta_$T);; esac
  for p in random reactive persist greedy planner; do A+=(results/o_${n}_${p}_$T); done
  { echo "################ WORLD $n ($f)"; q q/programs/world_report.q "${A[@]}" < /dev/null 2>&1
    for x in "${A[@]:1}"; do case $x in *_single_*|*_noTouch_*|*_spont_*|*_hidCos_*|*_noMeta_*)
      echo "######## $(basename $x) vs brain"; q q/programs/compare.q results/o_${n}_brain_$T $x < /dev/null 2>&1 | sed -n '/PAIRED DIFF/,/^$/p';; esac; done; echo; } >> $REP
done
# ---- 2. satiety edges (O.4) ----
for w in "open=" "pillars=--world pillars"; do n=${w%%=*}; f=${w#*=}
  # shellcheck disable=SC2086
  rt "o_${n}_sat2" -- --bars $B $f --sat-edges 1,4,10,20
  { echo "######## O.4 $n: sat-edges 1,4,10,20 vs default"; q q/programs/compare.q results/o_${n}_brain_$T results/o_${n}_sat2_$T < /dev/null 2>&1 | sed -n '/PAIRED DIFF/,/^$/p'; } >> $REP
done
# ---- 3. harder goals (W.5) ----
G=("evasive=--evasive" "mice3=--mice 3" "mice3pil=--mice 3 --world pillars" "dog=--distractor" "dogpil=--distractor --world pillars")
for g in "${G[@]}"; do n=${g%%=*}; f=${g#*=}; A=()
  for p in brain reactive greedy planner; do
    pf=""; [ $p != brain ] && pf="--policy $p"
    # shellcheck disable=SC2086
    rt "o_${n}_$p" -- --bars $B $f $pf; A+=(results/o_${n}_${p}_$T); done
  { echo "################ HARDER GOAL $n ($f)"; q q/programs/world_report.q "${A[@]}" < /dev/null 2>&1; echo; } >> $REP
done
# ---- 4. generalisation over layouts (W.6) ----
rt o_genMany -- --bars 20000 --world random --layout-every 2000
rt o_genOne -- --bars 20000 --world random
for tr in genMany genOne; do
  rt "o_${tr}_heldFrozen" -o 100 -- --bars 5000 --world random --no-learning --load-weights "results/o_${tr}_$T/s{id}/weights_seed{id}.bin"
  rt "o_${tr}_heldLearn" -o 100 -- --bars 5000 --world random --load-weights "results/o_${tr}_$T/s{id}/weights_seed{id}.bin"
done
rt o_heldFresh -o 100 -- --bars 5000 --world random
rt o_heldUntrained -o 100 -- --bars 5000 --world random --no-learning
rt o_heldReactive -o 100 -- --bars 5000 --world random --policy reactive
{ echo "################ GENERALISATION (held-out random layouts, 5k bars)"
  q q/programs/world_report.q results/o_genMany_heldFrozen_$T results/o_genMany_heldLearn_$T results/o_genOne_heldFrozen_$T results/o_genOne_heldLearn_$T results/o_heldFresh_$T results/o_heldUntrained_$T results/o_heldReactive_$T < /dev/null 2>&1
  for x in genMany_heldFrozen genOne_heldFrozen genMany_heldLearn genOne_heldLearn; do echo "######## $x vs fresh"; q q/programs/compare.q results/o_heldFresh_$T results/o_${x}_$T < /dev/null 2>&1 | sed -n '/PAIRED DIFF/,/^$/p'; done; echo; } >> $REP
# ---- 5. curriculum: open 5k -> pillars 5k -> rooms 10k, vs rooms from scratch ----
rt o_cur1 -- --bars 5000
rt o_cur2 -- --bars 5000 --world pillars --load-weights "results/o_cur1_$T/s{id}/weights_seed{id}.bin"
rt o_cur3 -- --bars 10000 --world rooms --load-weights "results/o_cur2_$T/s{id}/weights_seed{id}.bin"
{ echo "################ CURRICULUM: rooms 10k after open 5k + pillars 5k, vs rooms 10k from scratch"
  q q/programs/compare.q results/o_rooms_brain_$T results/o_cur3_$T < /dev/null 2>&1 | sed -n '/PER-ARM/,/^$/p;/PAIRED DIFF/,/^$/p'; echo; } >> $REP
# ---- 6. untrained brains for the structure probes ----
rt o_untrained -- --bars 0
rt o_untrainedSingle -- --bars 0 --single-brain
echo done
