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
- [ ] **E.3** Open world and re-learning, all four arms, seeds 17–32.
- [ ] **E.4** Guard worlds: base, main and gate in pillars, rooms and blink 20:20, seeds 17–32, 10,000 bars, with `reactive` as the reference baseline.

### Stage 5: test
- [ ] **S.1** Score §0's three conditions for main and gate; check D1–D5; write §6. Acceptance: every number in §6 traces to a report file in `results/`.
- [ ] **S.2** Report to the user, with the decisions in §7. Update the analysis note's §6 ("not established") and THEORY.md's tags only where a prediction was confirmed or refuted.

## 6. Results

*(none yet)*

## 7. Decisions for the user

1. **If main or gate passes, does it become the default?** That changes the golden digest and every default run, as the 2026-10-29 defaults milestone did. Not done without the user's answer.
2. **Today's punishment timing** (neutral off until bar 2,001; bad-move calls depending on the run length through `numEpochs`): `--punish-calls` replaces both. If a default changes, should the old timing go entirely?
3. **Decay ×0.5 on the primary brain only.** I applied it to the primary brain only, to match the model, which has no meta-brain. Should the meta-brain's decay follow?
4. **If the guard worlds show a loss**, which takes priority: the open-world target or the walled worlds?

## 8. Progress log

- **2026-11-08.** The user chose the main route (P4 + floor 100 + decay ×0.5) with the gate as a second arm, and asked for the model scripts in the repo. Plan written with predictions fixed (§3) before any simulator run.
- **2026-11-08.** T.2, T.3, E.1, E.2 done (details on each task). Also, at the user's request, defined every symbol in the analysis note (a notation table at the top, and inline at each equation). E.3 and E.4 started: `scripts/experiments/catch_rate_experiment.sh 20261108` (log `results/cr_run_20261108.log`, reports `results/cr_report_20261108/`).
