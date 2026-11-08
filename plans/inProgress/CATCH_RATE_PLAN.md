# Plan: raise the catch rate without losing spontaneity or re-learning

**Status (2026-11-08): in progress.** Stage 3 of the user's catch-rate course (analysis → assessment → **plan** → execute → test at 0.7). The analysis and assessment are [researchNotes/CATCH_RATE_ANALYSIS.md](../../researchNotes/CATCH_RATE_ANALYSIS.md): read its §1, §3.1–3.4 and §4.2 first.

**The user's choice (2026-11-08):** take the recommended route, raised punishment with the spontaneity controls (**P4 + floor 100 + decay ×0.5**), as the main candidate, with the performance gate (**G20 + P3 + floor 100 + decay ×0.5**) as a second arm. The model scripts go into the repo (`scripts/model/`).

**How to use this document:** tasks are checkboxes (`[ ]` not started, `[~]` partly done, `[x]` done and verified). Tick `[x]` only after the acceptance check passes, with the date and what was verified. Append every session's work to the Progress Log (§8). The decisions in §7 belong to the user: ask, don't choose. Every claim needs a paired multi-seed comparison; never conclude from one seed.

---

## 0. Goal

> **The user (2026-11-07):** "how to increase the catch rate without losing this spontaneity and capacity to relearn."

Three conditions, all on fresh seeds 17–32 (seeds 1–16 are used up for this question; analysis Appendix):

1. **Catch rate and alignment.** The revival plan's §0 target: epoch-5 alignment (bars 8,001–10,000) **≥ 0.7 in ≥ 14 of 16 seeds** by 10,000 bars, in the open world. Catches per 1,000 bars above the baseline, paired.
2. **Spontaneity kept.** On the trained weights, the per-bar chance of leaving the dominant path (median "deviation") and the availability of alternatives ("alt. share") are **not below the baseline's**.
3. **Re-learning kept.** After a leg reversal (G.4, `A:10000,L180:5000,A:5000`), at least as many runs regain 0.6 alignment within the 5,000-bar L180 leg as the baseline, and the return to A is no slower.

Walled and blinking worlds are not part of the 0.7 target, but the north star needs both halves, so they are a **guard** (task E.4): a significant loss there is reported to the user before anything becomes a default.

## 1. Why these changes (summary of the analysis)

- **Separation condition (Derived).** A wrong route only falls if its punishment beats the reinforcement every used route gets regardless of outcome: nP > C. Today nP ≈ 0.21 (one punish call, and none at all before bar 2,001) against C ≈ 0.64 (strategic 0.48 + catch boost 0.16), so even a slot pointing away from the mouse gains weight. Policies lock in by bar 500 and never correct.
- **P4** (4 punish calls for neutral and bad moves, from bar 1) gives nP ≈ 0.84 > C, with strategic and the catch boost untouched.
- **G20 + P3** makes strategic reinforcement conditional on hunting going well (≥ 20 catches in the last 200 bars, against 3 today), so it stops cancelling punishment while the policy is still poor.
- **Separation costs spontaneity**: wrong routes are pushed to the floor and stay there. **Decay ×0.5** restores moment-to-moment deviation; **floor 100** makes floored alternatives more available. The floor must respect the re-learning limit ln(1 + h/F) > nP (h = 250, nP = 0.84: F < 190; F = 100 gives ln 3.5 = 1.25 > 0.84; F = 250 gives ln 2 = 0.69 < 0.84 and fails, as the model confirmed).

## 2. Design intent that applies (revival plan §2)

- The core mechanism is unchanged: usage-weighted probabilistic routing of conserved energy, rewards as multiplicative steps, decay.
- **Unconditional usage reinforcement stays at Hebb 250** (point 7). Nothing here touches it.
- **The meta-brain and the strategic term stay.** The main arm leaves strategic and the catch boost at full strength; the gate arm keeps strategic at full strength when it is on.
- **No hard-coded values** (user, 2026-11-03): every new number is a flag with a documented default that reproduces today's behaviour exactly (golden digest unchanged).
- **Flags are for experiments.** If an arm passes, making it the default is the user's decision (§7 decision 1); until then everything is opt-in.

## 3. Pre-registered predictions (written before any simulator run)

From the model (CATCH_RATE_ANALYSIS §4.2). Each pair is noisy, noise-free; the real brain sat between them at baseline. **Only direction and rank are claimed, not levels.**

| Arm | Alignment | Catches/1k | Seeds ≥ 0.7 | Deviation (median) | Alt. share | L180 missing 0.6 | L180 bars to 0.6 | Back to A, bars |
|---|---|---|---|---|---|---|---|---|
| base (today) | 0.52, 0.77 | 60, 98 | 3.5, 13 | 0.146 | 4.8e-5 | 12, 6 | never, 1,065 | 2,880, 2,660 |
| P4 | 0.82, 0.87 | 107, 118 | 14.5, 16 | 0.054 | 2.4e-5 | 1, 0 | 1,055, 1,145 | 243, 304 |
| **main**: P4 + floor 100 + decay ×0.5 | 0.86, 0.87 | 112, 117 | 16, 16 | 0.203 | 1.5e-4 | 0, 0 | 2,360, 1,860 | 267, 357 |
| **gate**: G20 + P3 + floor 100 + decay ×0.5 | 0.84, 0.87 | 110, 118 | 15.5, 16 | 0.178 | 1.5e-4 | 0, 0 | 1,982, 2,140 | 343, 393 |

Real baseline on seeds 1–16 for reference: alignment 0.661, catches 79, 7/16 seeds ≥ 0.7; L180 missing 0.6 in 13/16 (earlier single-brain system).

**Directional predictions to check:**
- **D1.** main and gate both raise epoch-5 alignment and catches over base (paired CI excluding 0).
- **D2.** main and gate meet the §0 target (≥ 14/16 seeds ≥ 0.7).
- **D3.** P4 alone collapses deviation below base; adding floor 100 + decay ×0.5 (main) restores it to at least base.
- **D4.** main and gate miss 0.6 in the L180 leg in fewer runs than base; main recovers more slowly than P4 alone.
- **D5.** The returns to A are faster than base for all three changed arms.

If a prediction fails, the result stands and goes in §6; the model doesn't get tuned to fit it after the fact.

## 4. Arms and flags

| Arm | Flags (on top of the defaults) |
|---|---|
| base | none |
| P4 | `--punish-calls 4` |
| main | `--punish-calls 4 --floor 100 --weight-ratio 20000 --decay-mult 0.5` |
| gate | `--punish-calls 3 --strategic-gate 20 --floor 100 --weight-ratio 20000 --decay-mult 0.5` |

- `--punish-calls N`: neutral and bad moves both get N punish calls, from bar 1. Unset (default) keeps today's rule: bad moves 1 + 2 × epoch ÷ numEpochs calls, neutral moves 1 call from bar 2,001 (analysis §4.3 C).
- `--strategic-gate N`: strategic reinforcement needs at least N catches in the last 200 bars (default 3, today's rule).
- `--floor F`: the weight floor (default 25). Initial weights stay 25–1000 whatever the floor, as in the model (values below the floor rise to it at the first decay, bar 5).
- `--weight-ratio 20000` keeps the cap at 2,000,000 with floor 100 (cap = ratio × floor), as in the model.
- `--decay-mult D`: multiplies the primary brain's decay loss per period (default 1: 0.5–2% every 5 bars). The meta-brain's decay is unchanged, because the model didn't include it.

## 5. Tasks

### Stage 3: plan and tools
- [x] **T.1** Write this plan, with predictions fixed before any run. Move it to `plans/inProgress/`; update CLAUDE.md's status. *(2026-11-08: done; §3 written before any simulator run with the new flags)*
- [x] **T.2** Move the model into the repo: `scripts/model/` (the mean-field model, the kinematic model, the sweeps and tables behind the analysis, the weight export, the ledger probe), with a README saying what produced which table. Paths are relative to the repo; nothing in `results/`. Acceptance: `model.py` and `kin.py` import and reproduce the kinematic validation row (reactive cat ≈ 126 catches/1k, 0.90) from the repo copy. *(2026-11-08: done. `scripts/model/` with a README mapping each note table to its script; solved tables and exported weights copied to `results/model/` (gitignored). The repo copy gives 125.8 catches/1k and 0.897 for the reactive lookup, as in the note. The buggy `mf.py` was dropped; `model.py` is the fixed version. The ledger is kept as `probe/` hooks plus `make_probe.sh`, which rebuilds the hooked bar loop from the current one, rather than as a second copy of the simulator)*
- [x] **T.3** A weights-only spontaneity measure for any arm: export each seed's weights and compute deviation and alt. share with the model's `spont()` (the same definition as the analysis). Acceptance: on `o_open_brain_20261103` it reproduces the analysis's real-baseline figures, or the difference is explained. *(2026-11-08: done. `scripts/model/spont_arm.py`: mean deviation 0.212, range 0.000–0.545, alt. share 6.2e-5, exactly the note's figures. Note: the note's "real 0.212" is a mean; the real median is 0.198. §0 condition 2 is judged on both)*

### Stage 4: execute
- [x] **E.1** Flags `--punish-calls`, `--strategic-gate`, `--floor`, `--decay-mult` (§4), documented at the top of `cat_mouse.q`, in CLAUDE.md's flag list and in `summary.csv`. Acceptance: all tests pass; the golden digest is unchanged; `test_weights.q` covers each flag's parsing and rejection of bad values; a unit test shows `--decay-mult 0.5` halves the mean decay loss and `--floor` sets the floor. *(2026-11-08: done. All 48 test files pass, golden digest unchanged. test_weights.q: defaults and flags recorded in summary.csv, each flag changes learning, floor-100 weights stay in [100, 2,000,000] and some sit at 100, `--floor` leaves the initial weights unchanged, 9 bad values rejected. test_synapses.q: applyDecay = applyDecayBy with the defaults; mean loss 1.25% per period, halved at D = 0.5; a raised floor holds. Also: `.syn.INIT_MIN` keeps initial weights at 25–1000 whatever the floor; `--weight-ratio` now checks ratio × floor ≥ 1000 (the same as ratio ≥ 40 at floor 25))*
- [x] **E.2** A driver `scripts/experiments/catch_rate_experiment.sh` running the arms of §4 on seeds 17–32: open world 10,000 bars; re-learning `A:10000,L180:5000,A:5000`; walled guard. Acceptance: a smoke run (2 seeds, short bars) completes and the reports parse. *(2026-11-08: done. Seeds 901–902, 600 bars, every stage; target, compare, spontaneity, relearn and guard reports all parse; smoke results deleted. New report `q/programs/analysis/target_report.q` (last-epoch alignment, seeds at the target, paired CIs); on `o_open_brain_20261103` it gives 0.662, 7/16, 80.2 catches/1k, as recorded)*
- [x] **E.3** Open world and re-learning, all four arms, seeds 17–32. *(2026-11-08: done, §6.1–6.3)*
- [x] **E.4** Guard worlds: base, main and gate in pillars, rooms and blink 20:20, seeds 17–32, 10,000 bars, with `reactive` as the reference baseline. *(2026-11-08: done, §6.4: no loss in any world)*

### Stage 5: test
- [x] **S.1** Score §0's three conditions for main and gate; check D1–D5; write §6. Acceptance: every number in §6 traces to a report file in `results/`. *(2026-11-08: done; every §6 number is from `results/cr_report_20261108/`)*
- [ ] **S.2** Report to the user, with the decisions in §7. Update the analysis note's §6 ("not established") and THEORY.md's tags only where a prediction was confirmed or refuted.

### Phase R: constants from the equations (user, 2026-11-08)

> **The user:** "the equations that created what is possible here should be built into any future runs of the brain … if I request a larger brain, some of these constants, like the punish metric there, or the Hebbian number, should scale based on the mathematics automatically, and that should be written into the code." (Revival plan §2 point 10.)

Same course as before: analysis, write-up, plan, execute, test. No simulator runs with new settings before R.3 exists.

- [x] **R.0** Make main the default (§7 decision 1). Acceptance: a default run is byte-identical to the explicit main run; legacy flags reproduce the old golden digest; tests pass; docs updated. *(2026-11-08: done. Seed 1, 20k bars: default and `--punish-calls 4 --floor 100 --weight-ratio 20000 --decay-mult 0.5` give identical weights and simulation logs. `--punish-calls legacy --floor 25 --decay-mult 1` restores the old default and reproduces its golden digest (checked in test_golden.q). Without `--weight-ratio` the cap now stays at 2,000,000 whatever the floor. New golden digest 5af7ecb8…; cat_mouse.q, CLAUDE.md and revival §2 (new point 10) updated; the experiment driver's arms now name all four settings)*
- [ ] **R.1** Analysis: for each constant the theory ties to the configuration (punish calls, floor, decay, cap, Hebb, steps per bar), the formula and what it depends on: steps per bar, strategic scope and magnitude, catch window, the task's catch-rate ceiling, path length and use rate through the brain, number of input slices. Derive the size-dependent quantities (path length, use rate) from the untrained brain's Markov chain (THEORY.md, `.theory.chain`), so the brain can measure its own geometry at start-up. Give decay a derivation instead of a sweep value. Written up in `researchNotes/CONSTANT_SCALING.md`. Measures existing runs and saved brains only.
- [ ] **R.2** Analysis: why the gate arm can't re-learn after the leg reversal (§6.3), from its existing logs and weights (§7 decision 5: investigate).
- [ ] **R.3** Plan: the start-up rules and their margins, chosen so today's configuration gives (or nearly gives) 4 / 100 / ×0.5; predictions for other sizes, fixed before any run.
- [ ] **R.4** Execute: compute the constants at start-up from the configuration, print and record them in summary.csv; explicit flags still override. Tests: the rules give the expected values on today's brain and on hand-worked cases.
- [ ] **R.5** Test on other brain sizes (e.g. `--central-rows 6`, `--single-brain`, and any larger brain the infrastructure allows): derived constants against today's fixed ones, fresh seeds. Success: the derived constants keep the §0 conditions where fixed ones lose them, or match them where nothing should change.

## 6. Results

**Run 2026-11-08**, seeds 17–32, every arm paired by seed. Reports in `results/cr_report_20261108/` (`open_target.txt`, `open_compare.txt`, `open_spontaneity.txt`, `relearn.txt`, `guard.txt`); driver `scripts/experiments/catch_rate_experiment.sh 20261108`.

### 6.1 Open world, 10,000 bars (§0 condition 1)

Epoch 5 (bars 8,001–10,000). Paired differences against base, with 95% bootstrap CIs.

| Arm | Mean alignment | Min | Seeds ≥ 0.7 | Catches/1k | Δ alignment (CI) | Δ catches/1k (CI) | Seeds better than base |
|---|---|---|---|---|---|---|---|
| base | 0.695 | 0.449 | 10/16 | 82.8 | | | |
| P4 | 0.832 | 0.729 | **16/16** | 106.5 | +0.137 (+0.091, +0.185) | +23.6 (+14.0, +33.7) | 15 |
| **main** | **0.853** | 0.790 | **16/16** | **114.1** | **+0.158 (+0.112, +0.209)** | **+31.3 (+21.3, +42.1)** | **16** |
| gate | 0.854 | 0.813 | **16/16** | 115.2 | +0.159 (+0.120, +0.205) | +32.3 (+24.4, +41.4) | 16 |

**The §0 target is met by all three changed arms: 16 of 16 seeds at ≥ 0.7, against 14 needed.** The worst main seed (0.790) is above base's mean. Base did better on these seeds than on seeds 1–16 (10/16 and 0.695, against 7/16 and 0.662); the comparison is paired, so that doesn't affect the differences.

The levels land inside the model's brackets (§3): alignment 0.82–0.87 predicted, 0.83–0.85 measured; catches 107–118 predicted, 106–115 measured.

### 6.2 Spontaneity, weights-only, at bar 10,000 (§0 condition 2)

| Arm | Deviation, median | Deviation, mean | Alt. share (geomean) | Δ mean deviation (CI) | Alt. share vs base |
|---|---|---|---|---|---|
| base | 0.207 | 0.219 | 6.4e-5 | | |
| P4 | 0.213 | 0.215 | 2.3e-4 | −0.004 (−0.093, +0.082) | ×3.7 (CI ×1.2 to ×12) |
| **main** | **0.329** | **0.316** | **4.5e-4** | **+0.097 (+0.009, +0.179)** | **×7.1 (CI ×3.5 to ×16)** |
| gate | 0.211 | 0.208 | 2.4e-4 | −0.011 (−0.073, +0.052) | ×3.8 (CI ×1.9 to ×9) |

**Main raises both measures of spontaneity, and the others keep deviation at base and raise alt. share.** All arms pass condition 2.

### 6.3 Re-learning, A:10,000 → L180:5,000 → A:5,000 (§0 condition 3)

| Arm | L180: runs missing 0.6 | L180: t06 median | L180: recovered level | Back to A: t06 median | Back to A: Δ t06 vs base (CI) | Back to A: recovered |
|---|---|---|---|---|---|---|
| base | 8/16 | 1,797 | 0.497 | 1,843 | | 0.678 |
| P4 | 8/16 | 2,582 | 0.405 | 769 | −433 (−969, +178) | 0.790 |
| **main** | **7/16** | 1,890 | 0.456 | **1,145** | −283 (−718, +192) | 0.715 |
| gate | **16/16** | never | **0.124** | 836 | −903 (−1,149, −638) | 0.872 |

(t06 medians include censored runs as not reached; L180 recovered level paired vs base: P4 −0.09 (−0.25, +0.08), main −0.04 (−0.18, +0.11), gate **−0.37 (−0.47, −0.27)**.)

- **Main passes condition 3**: as many runs regain 0.6 as base (9 against 8), the recovered level isn't significantly different, and the return to A is no slower (median 1,145 against 1,843 bars; CI includes 0).
- **Gate fails condition 3**: no run regains 0.6 after the leg reversal, and its recovered level is 0.37 below base. The cause is not established. It is not the floor condition (ln(1 + 250/100) = 1.25 > 3 × 0.21).
- **Re-learning did not improve**, which the model predicted it would (0–1 runs missing). The note said the model was optimistic on re-learning levels; this is larger than that caveat suggested.

### 6.4 Guard worlds, 10,000 bars (E.4)

Catches per 1,000 bars; paired difference in catches per run against base (CI).

| World | base | main | gate | reactive | main vs base | gate vs base |
|---|---|---|---|---|---|---|
| Pillars | 11.0 | 16.1 | 19.1 | 88.0 | **+46%** (+22, +78 per run) | **+74%** (+58, +105) |
| Rooms | 2.4 | 2.5 | 4.1 | 19.9 | +3%, ns (−10, +10) | **+71%** (+3, +28) |
| Blink 20:20 | 43.1 | 51.8 | 52.0 | 59.2 | **+20%** (+48, +124) | **+21%** (+43, +131) |

**No loss in any guard world; both arms gain in pillars and blink, and gate in rooms too.** Both still trail the memoryless reactive cat in all three, as before (OBSTACLES_RESULTS); visible alignment in blink rises (0.71 → 0.81) but hidden-bar behaviour doesn't change.

### 6.5 Predictions (§3)

| | Prediction | Outcome |
|---|---|---|
| D1 | main and gate raise alignment and catches, CI excluding 0 | **Confirmed** (both) |
| D2 | main and gate meet ≥ 14/16 at ≥ 0.7 | **Confirmed** (16/16 each; P4 too) |
| D3 | P4 collapses deviation; main restores it to at least base | **Half refuted.** P4 leaves deviation unchanged (0.213 vs 0.207). Main raises it above base: confirmed |
| D4 | main and gate miss 0.6 after L180 in fewer runs than base; main slower than P4 | **Refuted.** Main 7 vs 8 (no real change); gate 16 vs 8 (worse); main is not slower than P4 |
| D5 | returns to A faster than base for all three | **Direction confirmed** for all three medians; significant only for gate |

**What the model got right:** the separation condition's prediction that raising punishment lifts alignment and catches, at the size predicted, without weakening strategic; and that decay ×0.5 with floor 100 adds spontaneity. **What it got wrong:** re-learning (it predicted a large improvement; there is none for main and a failure for gate), and P4's cost to deviation (there is none on the real brain).

### 6.6 Verdict against §0

| Condition | main | gate | P4 |
|---|---|---|---|
| 1. ≥ 14/16 at ≥ 0.7, catches up | **Pass** (16/16, +38% catches) | Pass (16/16, +39%) | Pass (16/16, +29%) |
| 2. Spontaneity not below base | **Pass** (both measures up) | Pass (deviation equal, alt. share up) | Pass (deviation equal, alt. share up) |
| 3. Re-learning not worse | **Pass** (9 vs 8 recover; return to A no slower) | **Fail** (0 recover) | Pass (8 vs 8) |
| Guard worlds | No loss; +46% pillars, +20% blink | No loss; +21–74% | not run |

**Main meets all three conditions.** Whether it becomes the default is §7 decision 1.

## 7. Decisions for the user

1. ~~If main or gate passes, does it become the default?~~ **Answered (user, 2026-11-08): yes, main is the default (R.0), and its constants should come from the equations and scale with the brain (Phase R).**
2. **Today's punishment timing** (neutral off until bar 2,001; bad-move calls depending on the run length through `numEpochs`): `--punish-calls` replaces both. If a default changes, should the old timing go entirely?
3. **Decay ×0.5 on the primary brain only.** I applied it to the primary brain only, to match the model, which has no meta-brain. Should the meta-brain's decay follow?
4. ~~If the guard worlds show a loss~~: moot, none did (§6.4).
5. ~~The gate arm's re-learning failure~~ **Answered (user, 2026-11-08): investigate (R.2).**

## 8. Progress log

- **2026-11-08.** The user chose the main route (P4 + floor 100 + decay ×0.5) with the gate as a second arm, and asked for the model scripts in the repo. Plan written with predictions fixed (§3) before any simulator run.
- **2026-11-08.** T.2, T.3, E.1, E.2 done (details on each task). Also, at the user's request, defined every symbol in the analysis note (a notation table at the top, and inline at each equation). E.3 and E.4 started: `scripts/experiments/catch_rate_experiment.sh 20261108` (log `results/cr_run_20261108.log`, reports `results/cr_report_20261108/`).
- **2026-11-08.** E.3, E.4, S.1 done (§6). Main meets all three §0 conditions: 16/16 seeds ≥ 0.7 (mean 0.853 against base 0.695), catches +38%, spontaneity up, re-learning no worse, no loss in pillars, rooms or blink. Gate fails re-learning after the leg reversal (0/16), cause unknown. The model was right on the open world and wrong on re-learning. Waiting on the user for §7 decisions 1 and 5.
- **2026-11-08.** The user approved main as the default and asked for the constants to be derived from the equations so they scale with the brain (revival §2 point 10), and for the gate failure to be investigated. R.0 done (byte-identical default; legacy flags reproduce the old digest). Phase R added.
