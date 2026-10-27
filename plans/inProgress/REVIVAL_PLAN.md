# Revival Plan — ForWhoseAdvantage / Questions

**Status as of 2026-10-25:**
- **North star (§0):** the cat learns to head towards the mouse. **Phase G is the top priority.**
- **Done:** Phases 0, 1, 2, 3 (except 3.4) and 6; S.1–S.3; M.1–M.2; G.0–G.2 (including 3.5 and P.0–P.4); G.3(a) and G.3(b) (both kept, opt-in flags). **Planned:** Phase G.R (G.R0–G.R6).
- **Next:** G.4 reversal learning. It's needed to judge G.R3's ratio on the user's retooling criterion, and it's where G.R4/G.R5 apply. Working configuration: `--cos-reward --reward-scale 0.25 --hebb 250`. On 32 fresh seeds: alignment 0.652, 16/32 seeds ≥ 0.7 by 10k bars; the §0 target (≥ 14/16, i.e. ≥ 28/32) is not met. Weights-only ceiling ≈ 0.72; execution loss is now ≈ 0.02.

See the checkboxes and the Progress Log at the bottom.

This is a working document. It is written so that any LLM session, or a human, can pick it up cold and keep going. Read sections 1–4 before touching code.

---

## 0. North star (set by the user, 2026-10-24) — read this first

> **The cat must actually learn to head towards the mouse.** All other work (the lab, the theory, speed-ups, the meta-brain) is worth doing only insofar as it moves the cat towards this goal. A system that can't reliably learn this simple mapping can't be generalised to richer inputs, outputs and rewards.

**How it's measured** (Phase G defines and implements these):

| Metric | Definition | Reference points |
|---|---|---|
| **Alignment** | mean cosine between the cat's move and the true direction to the mouse, over moves | random 0; untrained −0.22 (predicted, THEORY.md §3.1); best possible with 8 slices × 8 directions ≈ **0.90** (mean of cos over a uniform 0–45° offset) |
| Acquisition time | bars until the rolling 500-bar alignment first exceeds 0.6 | measured per seed |
| Catch rate | catches per 1000 bars | trained brains currently ~26–31 |
| Reversal | after the output wiring is permuted mid-run, bars until alignment recovers to 0.6 | the test of re-learning, i.e. "structures break down and reorganise" |
| Reliability | fraction of seeds meeting a target | n ≥ 16, paired comparisons |

"Towards %" (cos > 0) is too lenient to be the goal metric: about half of all directions qualify.

**Proposed targets** (the user can adjust them): alignment ≥ 0.7 in ≥ 14/16 seeds by 10k bars; acquisition faster than the current baseline, with paired CI excluding 0; reversal recovery in ≤ 5k bars.

**Priority rule:** Phase G comes first. Phase P (speed) is worked as G's enabler. Phase S and Phase M continue only where they feed G, e.g. a lab or theory result that suggests a reward or decay change to try on the cat.

## 1. How to use this document

- Tasks are checkboxes: `[ ]` not started, `[~]` in progress or partially done, `[x]` done and verified.
- **Only tick `[x]` after the acceptance check listed under the task has actually passed.** Add a short note: the date, plus what was verified or how.
- Every working session appends an entry to the **Progress Log** (section 8): what was done, what was found, what's next.
- Never delete findings (section 3). If one turns out to be wrong, strike it through (`~~...~~`) and say why.
- Decisions that belong to the user are listed in section 7. Don't silently pick an answer to one of them. Ask, or leave the task blocked.
- Phases are ordered by dependency. Phase 1 must be finished before any experimental result is trusted.
- **Do not trust performance claims in README.md, CLAUDE.md or `scripts/META_EXPERIMENT_README.md`.** They come from the buggy Fortran-era experiment, or from the buggy q port (see F6–F9). Phase 6 rewrites them.

---

## 2. Project summary (what this actually is)

A q/kdb-x simulation of a "cat" learning to hunt a "mouse" on a 100×100 field.

- **Primary brain:** 6×12 grid of trinary neurons (states 0/1/2). Energy units move between neighbours. The direction of each move is a weighted random choice using 4D synapses `syns[r][c][inDir][outDir]`, multiplied by a fixed downward bias (`.dirs.vBias`). Each successful move adds +1000 to the synapse it used (Hebbian), capped at 2,000,000.
- **Input:** the vision slice (1 of 8, by angle from the cat to the mouse) injects MEDIUM into the top row, cols `IN_OFF..IN_OFF+7` (currently `IN_OFF:2i`).
- **Output:** energy leaving the bottom row, cols `OUT_OFF..OUT_OFF+7`, goes into an 8-slot outputter. The slot with the maximum value is the move direction, and its value (1 or 2) × 5 is the move distance.
- **Bar:** one decision. A bar runs 12 primary propagation steps (and 24 meta steps), then the cat moves, then rewards are applied.
- **Meta-brain:** a 3×5 grid. Input is a positional encoding of the catch count over the last 200 bars.
  - `out[0]` → `temporalScope` (20/40/80 bars)
  - `out[1]` → `magnitude` (1 or 2)
  - The whole output vector is also injected into the primary brain's right column ("cross-flow").
- **Rewards** ([q/programs/cat_mouse.q](../../q/programs/cat_mouse.q)):
  - *Direct (tactical):* every bar, based on the dot product of the move and the direction to the mouse. Multiplicative ×≈2.2 reinforce, or ×≈0.43 punish, on synapses used this bar (`su`).
  - *Strategic:* if `rateCounter>=3` (or `>=1` in `--meta-only`), multiply the synapses used in each of the last `temporalScope` bars by `1.1*magnitude`, using the 100-deep history buffer `hist`.
  - *Catch boost:* on a catch, ×1.5 on synapses used in the last 20 bars.
  - *Meta-brain:* reinforced or punished based on `rateCounter`. Note this does **not** depend on what the meta-brain output.
  - *Decay:* every 5 bars.
- **Branch hypothesis** (`reaction-dissaccociation`; note that CLAUDE.md spells it differently): tactical and strategic rewards interfere, and removing tactical rewards after training improves performance ("+15%").

### Design intent (from the user, 2026-10-24) — the goal everything else serves

1. **Usage-strengthened probabilities build structure.** Routes that get used become more likely to be used again, which creates reinforced systems.
2. **The system also tends towards equilibrium** (decay), so built structures can break down and reorganise. That's a feature, not a flaw.
3. **The aim is self-propelling activity that stays in the brain.** Input churns through it and perturbs it, and beneficial structures self-organise, Game-of-Life style, in the user's words.
4. **A motivating factor (reward) steers which structures form.**
5. **Hierarchy:** the reward for one system is set by another system *of the same kind*, which gets its own, more basic reward. Structure on structure on structure. The primary brain/meta-brain pair is the first instance of this.
6. **Constraint:** keep the core mechanism, i.e. energy moving by usage-weighted probabilistic choice. Replacing it with a cellular-automaton state rule ("option A") was **rejected** by the user.
7. **Unconditional usage reinforcement must stay** (user, 2026-10-25). Strengthening a route simply because it was used, whatever the reward, is part of the design, not a flaw. It's what lets the system **retool and reweight itself when conditions change**, and what lets it be **creative**: it keeps routes alive that reward hasn't (yet) singled out. Don't remove it or gate it entirely behind reward. The tunable quantity is the **ratio** between unconditional (usage) reinforcement and directed (reward/goal) reinforcement.

History: Fortran → OpenMP and GPU → meta-brain experiments (Jan 2026, commit `c6f1c17`) → q port by an LLM (Mar 2026, `aab6bff`). The Fortran source is only in git history: `git show c6f1c17:src/programs/cat_mouse_learning.f90`.

---

## 3. Audit findings (reference — tasks cite these IDs)

Line numbers refer to the code **as of commit `380a227`**, before any fixes.

**F1 — Right-to-left precedence bugs (q evaluates `a*b+c*d` as `a*(b+(c*d))`).** Each one was checked in a q session.

| Where | Code | Effect |
|---|---|---|
| `vision.q:54` toroidalDist | `sqrt[dx*dx + dy*dy]` | (0,0)→(3,4) gives 7.55, not 5 |
| `vision.q:62` pathDist | `sqrt[pdx*pdx + pdy*pdy]` | wrong or NaN path length |
| `vision.q:63,69` pathDist | `(a) xexp 2 + (b) xexp 2` | means `a xexp (2+b²)`. A mouse 30 units away gives distance **0**, so there are **phantom catches** whenever \|Δx\|<1 |
| `vision.q:65` pathDist | `(..)*pdx + (..)*pdy` | wrong projection |
| `vision.q:30` visionSlice | `abs dx < 0.0001` | means `abs(dx<0.0001)`, so slice is −1 (no vision) for the **whole SW quadrant** |
| `cat_mouse.q:128` | `sqrt[dx*dx+dy*dy]` | NaN for some seeds, so the cat starts at (0,0) |
| `cat_mouse.q:367` | `sqrt[rdx*rdx+rdy*rdy]` | wrong respawn normalisation |
| `cat_mouse.q:381` | `sqrt[desX*desX+desY*desY]` | wrong unit vector to the mouse |
| `cat_mouse.q:384` | `catDx*desX + catDy*desY` | a NW move towards a NE target scores +14 (should be −7), so moving away is rewarded |
| `cat_mouse.q:278` | `` `int$bar-1i - til n`` | gives bars `bar-1, bar, bar+1…`. The strategic reinforcement hits the **oldest** history slots, not recent ones |

**F2 — `visionSlice` rounds instead of flooring** (`vision.q:39`, `` `int$ angle % sliceAngle``).
- An angle near 2π gives slice 8, which is out of range. The `'length` error **crashes the run** (seed 1 crashes at about bar 3000).
- It also shifts every slice boundary by 22.5°.

**F3 — `0n < 2.0` is true in q.** A NaN distance from F1 counts as a catch. Any comparison of a possibly-null float must account for this.

**F4 — Tests were never committed.**
- The `.gitignore` line `test_*` (left over from Fortran binaries) silently ignored `q/tests/test_*.q`. The port commit message claims "5/5 pass".
- Also, q's `set` on a nested list writes a companion file `weights_seedN.bin#`, which `*.bin` does not match.

**F5 — `--load-weights` is a stub** (`cat_mouse.q:112-118`, it prints a warning). The branch's core experiment (warm start + `--meta-only`) cannot run in q.

**F6 — In Fortran, the strategic reinforcement was dead after bar 100.**
- The loop is `do k = max(1, bar - brain_step), bar - 1` with the guard `if (k > 0 .and. k <= success_history_size)` (=100). Here `k` is an *absolute* bar number.
- So meta-controlled reinforcement only ran during bars 1–100 of every run.
- The Fortran catch boost also used fixed buffer slots 80–100 rather than "the last 20 bars".

**F7 — The "+15% retention" figure is confounded.** `run_meta_experiment.sh` computed `meta_avg / training_avg`:
- Meta-only arms started from the **best-of-30** weights, already warm (no acquisition phase).
- The baseline was the **average of all 30** runs from scratch, including the epochs before they had learned anything.

  Selection plus the head start would produce a gain even with no real effect. The more likely real mechanism: Hebbian +1000-per-use keeps whatever pathways the brain is using strong, and meta-only mode removes all punishment, so a good trained habit keeps itself going.

**F8 — The meta-brain cannot learn what it's described as learning.**
- ~~**Its reward is a function of its input** (`rateCounter`), not of the consequences of its output.~~ *Corrected 2026-10-24: there IS a causal loop. Meta output → the primary's reward → its behaviour → catch rate → the meta's reward. That loop is the user's intended design (§2 design intent, point 5). It's just slow and delayed, with no per-decision credit. A likelier cause of the null ablation (F9) is the substrate problem in F19 plus the saturation below. The meta-brain drains and saturates like the primary, so it can't hold structure.*
- **It doesn't decide when reinforcement fires:** the threshold `rateCounter>=3` is hard-coded.
- **It is saturated:** `do[rateCounter; applyReinforce]` (up to 200 calls of ×2.2) pins its weights at the cap almost immediately.

**F9 — Ablation (with the F1/F2 fixes, scratch copy, 8 seeds × 10k bars, direct+meta mode):**
- **Meta on:** mean 265 catches (range 185–392).
- **Meta outputs zeroed:** cross-flow off, scope=20, mag=1. Mean 274 (range 156–368).
- **Paired t ≈ −0.45**, so there's no detectable effect. The seed-to-seed spread is far larger than the claimed effect.
- Raw numbers (seeds 11–18): on = 330 198 185 207 392 271 311 223; ablated = 256 222 206 156 368 368 307 312.

**F10 — Reward magnitudes are effectively all-or-nothing.**
- `dotP` uses the raw move vector (length 5–14), not a unit vector. So `adaptThresh` (0.6–0.85) passes nearly any move with a positive component, which is a ~170° "narrow cone".
- `dirMult = 0.5+1.5*dotP` can then reach about 21, so `do[dirMult*2]` gives up to about 42 calls of ×2.2. Weights jump to the cap in one bar.
- Fortran had the same unnormalised dot product. Fortran also had a reinforce cap of 200,000 vs 2,000,000 elsewhere. The q port uses 2M everywhere.

**F11 — Tie-break bias.** `first where out=max out` always picks the lowest index (direction 0 = NW) on ties, and ties are common because the values are only 0/1/2.

**F12 — The world is inconsistent.**
- The cat is clamped to [0,100], while the mouse wraps around the edges (toroidally).
- Distance is toroidal, but vision uses raw dx/dy (not toroidal).
- The mouse respawn position is clamped, not wrapped.

**F13 — Docs describe things that don't exist.**
- **Unused code presented as active:** "valve 2/3" (`.brain.leftwardPull` and `.dirs.hBias` are never called), and the drain is described as "leftward" when it's actually uniform random.
- **Wrong details:**
  - `simLog` columns in the docs (`epochN`, `catchRate`, `metaScope`, `metaMag`, `towardPct`) don't exist
  - CLAUDE.md says `inOff:3i` but the code has `2i`
  - weights are said to be `weights.q` but are actually `weights_seedN.bin`
  - the stated `load.q` order doesn't match the file
- **Wrong comments:** `inc0` direction comments are wrong — 7 is S, not SW, and 8 is SE, not W. It works anyway because the value is only used as a label.

**F14 — Analytics bugs in `cat_mouse.q`.**
- `select catches:max catches - prev catches` gives the per-bar max delta (always 1), not the per-epoch count.
- `earlyAvg`/`lateAvg` divide by zero when there are fewer than 3 epochs.
- `calcRateCounter` is unused and has its own precedence bug.

**F15 — The default output dir is the cwd**, so a run from the repo root litters the root with CSVs and weight files.

**F16 (note, not necessarily a bug) — Input and meta cross-flow *overwrite* cells (`:`) rather than adding to them.** So they can create *or destroy* energy. Conservation only holds inside `.brain.propagate`.
- `test_conservation.q` shows that propagation conserves energy at 3×5, 6×12 *and* 8×16.
- So the old claim that "a meta-brain larger than 3×5 causes energy accumulation" can't come from propagation. If it's real, the overwriting injections are the likely source.

**F17 — Strategic magnitude was mis-ported.**
- Fortran applied ×1.1 `ni` times (`ni` = magnitude, 1 or 2), giving ×1.1 or ×1.21.
- The q port used `magnitude*1.1`, giving ×1.1 or **×2.2**. Restored to Fortran in task 2.2.
- Found 2026-10-24 while wiring the ablation flags. It was not part of the original audit.

**F19 — The brain can't hold self-sustaining activity** (measured 2026-10-24). Setup: a 6×12 brain was filled to about 50% pressure, given no input, and `.brain.propagate` was run alone.
- **Output sink on (normal):** the energy is gone within about 50 steps (about 4 bars), with trained and fresh weights alike. That's why measured pressure stays below 0.06, and why the pressure system never engages.
- **Sealed (no sink):** energy is conserved, but it settles into a saturated HIGH mass, bottom-heavy and piled at the *left*. About 12 of 72 cells churn at its edge, and no state repeats in 200 steps. So it's random churn, not structure.
  - Row energy, top→bottom: 10.2 10.7 11.5 12.6 14.4 15.6 (max 24).
  - The left-side pile isn't explained by `vBias`, which is left/right symmetric. A likely cause is the row-major fold order. Unverified.
- **Why:** the rules form a stochastic, conserved particle system with a downward drift. Systems like that relax towards equilibrium rather than forming persistent moving structures. Game-of-Life-style emergence needs low-noise local rules plus creation and destruction of activity, which strict energy conservation forbids.
- The scratch scripts used are not in the repo; re-create them from this description if needed.

**F20 — `acquisitionTime` was never recorded** (found 2026-10-25). The check read `(not null acquisitionTime)`, but the value starts null, so it could never be set and every run reported no acquisition. Fixed to `(null acquisitionTime)`: it now records the first crossing only. Reporting only — catches and alignment are unchanged, and the golden test still passes. The G.1 baseline was recomputed from saved logs (§6).

**F18 — The final epoch's move counts were dropped when that epoch had 0 catches** (the save was guarded by `if[catchesThisEpoch>0i; …]`). Fixed in task 2.4.

---

## 4. Ground rules for working in this repo

- **q precedence:** always parenthesise the left operand: `(a*b)+c*d`. For vector norms, use `.vision.norm` (added in Phase 1). Watch out for `f x < y`, which means `f (x<y)`.
- **Nulls:** `0n<x` is true for any x, so guard float comparisons that might be null.
- **`mod` is floored in q** (`-3 mod 100` is `97`). The history-slot formula relies on this.
- **Running headless:** `q q/programs/cat_mouse.q --seed 42 --bars 5000 --output-dir results/x/ < /dev/null`. Without the `/dev/null` redirect, q stays in the REPL afterwards (which is intentional, so you can query `simLog`).
- **Cost:** about 2.6 s per 500 bars on a single core. 16 parallel 10k-bar runs took about 6 min on this 16-core box. Learning results vary a lot between seeds, so **never compare single runs. Use ≥8 seeds, paired.**
- **Subprocesses from q:** write `system "env q …"`, never `system "q …"`. q can parse the latter as its own `\q` command and silently do nothing.
- **Symbol literals can't contain `-`:** use `hsym `$"path"` for paths, especially scratch paths, which contain `-mnt-`.
- **Tests:** `q/tests/run_all.sh` must pass after any change to `q/modules/` or `q/programs/`. It runs every `q/tests/test_*.q`; the shared assertions are in `q/tests/harness.q`. `test_weights.q` runs the simulation as a subprocess. The whole suite takes about 0.4 s.
- **Experiments:** use `scripts/run_trials.sh` (parallel seeds → `results/<label>/s<id>/`), then `q q/programs/compare.q results/<ref> results/<other> … < /dev/null` (paired by brain id, with a bootstrap CI). An n=3 dry run showed a spurious "+87.5%" with a CI of −19…+42 catches. That's why small-n comparisons are worthless here.
- **Linting:** use `/qlint-snippet` on single functions only (see CLAUDE.md, Tooling).
- **Commits:** only when the user asks. `results/` and weight files are never committed.

---

## 5. Phases and tasks

### Phase 0 — Handoff hygiene
- [x] **0.1** Write this plan. *(2026-10-24)*
- [x] **0.2** Add a banner at the top of CLAUDE.md pointing here, and warning that its performance claims and the existence of tests are unverified or stale. Acceptance: a new session sees the warning before anything else. *(2026-10-24)*

### Phase 1 — Correctness: restore the *intended* behaviour, with no design changes
- [x] **1.1** Add `.vision.norm:{sqrt sum x*x}` and `.vision.dot:{sum x*y}`. Rewrite `toroidalDist` and `pathDist` with them (F1). Acceptance: `test_geometry.q` cases — 3-4-5 triangle; a far mouse gives 30; a near mouse gives 1; toroidal wrap. *(2026-10-24: done; the geometry tests pass)*
- [x] **1.2** Fix `visionSlice`: the null-distance guard (F1) and floor rather than round (F2). Acceptance: all 8 slices reachable; an angle just below 2π gives 7; the SW quadrant gives 4 or 5, not −1. *(2026-10-24: done. It now uses `(floor angle%sliceAngle) mod numSlices`, so an angle of exactly 2π wraps to 0. Tests cover octant centres and boundaries, 2π−ε, 1000 random points, and identical positions.)*
- [x] **1.3** Fix `cat_mouse.q` precedence bugs (F1): initial position, respawn, `desLen`, `dotP`. Move the lookback-bar computation and the history-slot formula into `.syn.lookbackBars` / `.syn.histSlot` so they can be tested. Acceptance: test cases — lookback for bar 500, scope 5 is `499 498 497 496 495`; slot maths for the wrap case. *(2026-10-24: done; tested. The catch-boost slot comment was also corrected: j=1 is the current bar, because history is written before the boost.)*
- [x] **1.4** Sweep every `.q` file for other precedence or null bugs. Record the result in the Progress Log, even if nothing was found. *(2026-10-24: grep sweep plus a manual read. No new bugs. See the log for what's correct only by accident.)*
- [x] **1.5** Fix the analytics (F14): correct per-epoch catches; guard the early/late averages; delete `calcRateCounter`. *(2026-10-24: done. Per-epoch catches are now `sum deltas catches` by `1+(bar-1) div EPOCH_SIZE`; verified that 4+33+35 = 72 total on seed 1.)*
- [x] **1.6** `.gitignore`: drop the `test_*` rule and add `*.bin#` (F4). *(2026-10-24)*
- [x] **1.7** Recreate the tests: `q/tests/test_trinary.q`, `q/tests/test_geometry.q`, `q/tests/test_conservation.q`, plus `q/tests/run_all.sh`. Each exits non-zero on failure. The conservation test checks `sum raze br + sum out` is unchanged across `.brain.propagate` on random brains and synapses, and that the `inc0`/`inc1` bookkeeping stays consistent. Acceptance: `run_all.sh` exits 0. *(2026-10-24: done, plus `harness.q` and `test_weights.q` (Phase 2). 59 assertions, ALL TESTS PASSED.)*
- [x] **1.8** Smoke test: 4 seeds × 5000 bars, no crashes. Record catches and towards% in the Progress Log. (Expect roughly 50–120 catches each, based on the scratch-copy results.) *(2026-10-24: catches 72/123/74/50, towards 80/94/86/78%. Identical to the scratch prototype, so the refactor didn't change behaviour. This was before the F17 change.)*

### Phase 2 — Experiment infrastructure
- [x] **2.1** Implement `--load-weights FILE` (F5). Restore exactly what is saved: `(prim syns; meta syns; prim br; inc0; inc1; meta br; meta inc0; meta inc1; hist; histIdx)`. Validate shapes and fail loudly on a mismatch. Acceptance: a save → load → save round trip gives identical synapses. *(2026-10-24: done. The shape check uses a recursive `dims`. `test_weights.q` checks the whole saved state round-trips identically, not just synapses. Catch timestamps are not saved, so `rateCounter` starts at 0 on a warm start, as in Fortran.)*
- [x] **2.2** Ablation and control flags, printed in the run header:
  - `--ablate-meta`: meta-brain outputs are treated as zero, so there's no cross-flow and scope/magnitude are at their minimum
  - `--no-meta-rewards`: disables the strategic primary reinforcement block

  *(2026-10-24: done. `--no-meta-rewards` empties the lookback list, but the meta-brain's own rewards still run. Unknown `--` flags now abort with exit 1, which is tested. The F17 magnitude fix was also applied here.)*
- [x] **2.3** Default output dir is `results/seed<N>/` instead of the cwd (F15). Acceptance: a run from the repo root leaves `git status` clean. *(2026-10-24: `results/` is gitignored; verified clean.)*
- [x] **2.4** Machine-readable outputs per run:
  - `summary.csv`: one row — seed, bars, flags, weight file, total catches, towards/away/perp, mean pressure
  - `epoch_summary.csv`

  *(2026-10-24: done. `epoch_summary.csv` has a `bars` column, because the last epoch may be partial, and drops the phantom empty trailing slot. The F18 fix was also applied here.)*
- [x] **2.5** `scripts/run_trials.sh`: runs N seeds in parallel with pass-through args into `results/<label>/s<seed>/`. *(2026-10-24: done. It uses brain ids plus `-o` seed offset, and a `{id}` placeholder in flags for per-brain weight files. Exits 1 if any run lacks `summary.csv`.)*
- [x] **2.6** `q/programs/compare.q`: loads `summary.csv` / `epoch_summary.csv` across labels, pairs them by seed, and reports means, paired differences, the t statistic and a bootstrap 95% CI. *(2026-10-24: done. Pairs by brain id; bootstrap uses 10k resamples with a fixed seed. Dry run with n=3 succeeded.)*

### Phase 3 — Controlled re-test of the branch hypothesis (needs Phases 1–2)
Protocol:
1. Train K=16 brains from scratch: seeds 101–116, 20,000 bars, default (direct+meta) rewards.
2. Starting from each trained brain, run 4 arms × 10,000 bars. Use the same run seed across arms for a given brain (seed = 200 + brain index), so arms differ only by flag:
   - **A** continue training: `--load-weights W`
   - **B** meta-only: `--load-weights W --meta-only`
   - **C** no strategic rewards: `--load-weights W --meta-only --no-meta-rewards` (only Hebbian, catch boost and decay remain)
   - **D** meta-only with the meta-brain ablated: `--load-weights W --meta-only --ablate-meta`
3. Metric: catches per 1000 bars. Use the arm totals, and also the last training epoch as a "pre" reference.

How to read the results:
- B > A means removing tactical rewards helps (the branch hypothesis, now with a fair comparison).
- B ≈ C means strategic reinforcement adds nothing.
- B ≈ D means the meta-brain's outputs add nothing.

The whole protocol is scripted: `scripts/phase3_experiment.sh [K] [TRAIN_BARS] [ARM_BARS]`. Training is skipped if `results/p3_train` is already complete. The report goes to `results/p3_report.txt`.

- [x] **3.1** Train the 16 brains. *(2026-10-24: 16 × 20k bars in 781 s; all completed)*
- [x] **3.2** Run arms A–D. *(2026-10-24: 4 × 16 × 10k bars; all completed. Wall time: A 430 s, B 234 s, C 47 s, D 238 s. C being 5× faster is unexplained, and worth a look: presumably much less brain activity.)*
- [x] **3.3** Analyse with `compare.q`. Write a results table into section 6 of this document. *(2026-10-24: see section 6. Also fixed `compare.q` to pivot epochs when arms have different epoch counts, and widened its console.)*
- [~] **3.4** Report to the user. The interpretation and next research direction are the user's call (see section 7). *(2026-10-24: reported; decision 4 is awaiting the user's answer.)*
- [x] **3.5** No-learning baseline. Run a fresh brain with all learning disabled (no rewards, no Hebbian +1000, no decay) for 16 seeds × 20k bars. This gives the catch rate and towards % of an untrained cat. The README's learning-curve numbers (8 → 37 catches per 1000 bars) have no chance-level reference yet. It needs a new `--no-learning` flag. *(2026-10-25: all 16 seeds validated at 20k bars; see §6, Phase G.1.)*

### Phase G — The goal track: the cat learns to head towards the mouse (top priority; §0)
Work in order. Each change must beat the current baseline in a paired experiment (≥16 seeds, `compare.q`) before it's kept.
- [x] **G.0** Measure the goal. *(2026-10-25: alignment helper passes the +1/−1/0 acceptance cases; alignment is logged per bar and summarized per run/epoch; `compare.q` reports arm means and paired differences. Acquisition time is the first 500-bar window with mean alignment >0.6; null means not reached.)*
  - Add per-move alignment, the cosine between the normalised move vector and the true mouse direction, to `simLog`, `summary.csv` and `epoch_summary.csv`.
  - Add acquisition time to `summary.csv`.
  - Extend `compare.q` to report alignment.
  - Acceptance: a test checks alignment = +1 for a move straight at the mouse, −1 for straight away, 0 for perpendicular.
- [x] **G.1** Baselines (absorbs task 3.5): *(2026-10-25: both arms complete and validated; see §6.)*
  - [x] (a) **no learning**: a `--no-learning` flag that turns off all synapse updates. *(2026-10-25: implemented; same-seed test verifies primary and meta synapses remain identical to initialization through 200 bars, while the ordinary run changes primary weights.)*
  - [x] (b) **current code**: 16 seeds × 20k bars, recording alignment, acquisition time and catches. *(2026-10-25: all seeds and epoch totals validated; results in §6.)*
  - These are the numbers every later change must beat.
- [x] **G.2** Speed, so iteration is fast enough: Phase P tasks P.0–P.4, in that order. The aim is a 20k-bar run in under 1 minute instead of ~6.5; P.0's golden test guards behaviour. *(2026-10-25: target met. A fresh 20,000-bar run (seed 7, single process) takes 20.8 s.)*
- [~] **G.3** Reward fixes suggested by the audit and the theory. Try them one at a time, measured against G.1 (from (b) on: against the best configuration so far):
  - [x] (a) normalise `dotP` so the reward tracks real alignment (F10, Phase 4.1); *(2026-10-25: `--cos-reward`, opt-in. Paired 16 seeds × 20k vs a same-day baseline: catches **+24%** (CI +44.7 to +213.3 per run), alignment +0.035 (CI −0.005 to +0.074), acquired 14/16 vs 11/16. **Kept**: later G.3 steps run with `--cos-reward` on; making it the default is deferred to the end of G.3 because it changes the golden digest. Results in §6.)*
  - [x] (b) bounded log-space reward increments instead of ×2.22 repeated up to ~43 times (THEORY.md §8, P7, Phase 4.3); *(2026-10-25: `--reward-scale L` multiplies every primary-brain reward log-step by L with identical RNG use. Sweep on top of `--cos-reward`, paired vs L=1: L=0.25 gives catches **+49%** (CI +228 to +451 per run), alignment **+0.149** (CI +0.106 to +0.195), **16/16** acquired, median acquisition 1,200 bars (vs 5,543). L=0.1 and L=0.5 are also significantly better than L=1; L=0.1 vs L=0.25 is indistinguishable (plateau). **Kept at L=0.25.** THEORY.md P7 refuted: weights got sparser, not graded. Results in §6.)*
  - (c) random tie-break in the action choice (F11, Phase 4.2);
  - (d) a meta-brain saturation fix (Phase 4.4);
  - (e) **only with user approval:** angle-ordered output wiring (THEORY.md §3.1, P4, M.4).
- [ ] **G.4** Reversal learning. Add `--permute-outputs-at BAR` and measure recovery. If recovery is slow or absent, tune decay memory τ_d and the reinforcement number 𝓡 (THEORY.md §7), informed by lab task S.3b.
- [ ] **G.5** Generalisation checks for the final configuration: other mouse speeds, field sizes and seeds. Then rerun the Phase 3-style meta-brain controls on the improved cat.
- [ ] **G.6** Use the policy predictor (THEORY.md §3.2, task M.3) to screen candidate configurations cheaply before running full paired experiments.

### Phase G.R — A principled learning rule (from the literature; added 2026-10-25 at the user's request)

**Why this phase exists.** The ceiling diagnostic (§6, "Phase G — where the 0.6 ceiling lives") showed that, at the best configuration so far, the weights *alone* predict alignment ≈ 0.71 against a possible ≈ 0.90, and execution loses another ≈ 0.06. So the main gap is in what the learning rule writes into the weights. The user asked for literature-backed changes that (a) improve results, (b) stay general enough to **re-learn** when the reward or world changes, (c) focus learning on the rewards we define, and (d) **keep the core concept**: usage-weighted probabilistic routing of conserved energy, strengthened by experience, plus a hierarchy of same-kind systems (§2 design intent). Everything here satisfies (d): none of it replaces the energy rule. It changes only *how weights are updated* (and, in G.R6, what the meta-brain does).

**The key derivation** (THEORY.md §1/§6 notation; record it in THEORY.md when G.R2 lands):
- Route choice is $p(o) \propto (b_o W_o)^{\alpha}$. In log-weights $\theta=\ln W$ this is a **softmax policy**: $p(o) = e^{\alpha(\theta_o+\ln b_o)} / \sum_{o'} e^{\alpha(\theta_{o'}+\ln b_{o'})}$, with $\partial \ln p(c)/\partial\theta_o = \alpha(\mathbb 1[o=c]-p(o))$.
- **REINFORCE** (Williams 1992) for a softmax policy: $\Delta\theta_{s,o} = \eta\,(R-\bar R)\,\alpha\,(\mathbb 1[o=c_s]-p_s(o))$, for every directed state $s$ = (cell, inDir) the energy passed through, with $c_s$ its chosen route. The `su` route record is already that path, so **the existing credit assignment has the right shape**.
- Compared with that rule, the current update is missing:
  1. a **baseline** $\bar R$: it rewards absolute success, not better-than-expected;
  2. the **(1−p) / −p terms**: it pushes the chosen route equally however certain it already is, i.e. rich-get-richer, which is why routes saturate and why gentler rewards (G.3b) helped;
  3. its **unconditional Hebbian +1000 per use** has equilibrium $W^*=h u\tau_d$, which tracks *usage*, not *reward*. That term is **intentional and stays** (§2, point 7): it enables retooling and creativity. What may be wrong is its *ratio* to the directed reinforcement. G.3(b) supports this: shrinking the directed rewards by λ=0.25, which raised the unconditional share, was the biggest gain so far. G.R3 tunes that ratio.
- Each directed state choosing among 8 routes is a textbook **stochastic learning automaton**. The brain is a **team of learning automata sharing one reward** (Narendra & Thathachar 1989; Thathachar & Sastry 2004, *Networks of Learning Automata*). Williams (1992) showed the classic linear reward–inaction scheme is a special case of REINFORCE, so convergence results from that literature apply here.
- **Possibly novel** (check under M.5 before claiming): a team of learning automata *physically coupled through a conserved energy lattice* (direction memory, capacity-2 exclusion), whose credit assignment falls out of the energy path; and an actor–critic built from two systems of the same kind (G.R6).

**Rules for every G.R task:**
- Opt-in flag; the default stays bit-identical (golden test).
- Unit tests for the update arithmetic.
- Paired 16 seeds × 20k bars against the **best configuration so far**, currently `--cos-reward --reward-scale 0.25`.
- Report alignment, acquired count and median bars, seeds with alignment ≥ 0.7 by 10k bars (the §0 target), catches, and the **weights-only predicted E[cos]** from G.R0, to see whether the change lifts the ceiling in the weights.
- Keep a change only if alignment or the target count improves with a CI excluding 0, and nothing else gets significantly worse.

**Order:** G.R0 → G.R1+G.R2 (one combined arm, plus each alone if the combination wins) → G.R3 → G.3(c)/(d) (cheap execution fixes for the ≈0.06 loss) → G.4 reversal, with G.R4/G.R5 as the relearning fixes → G.R6.

- [x] **G.R0** Promote the ceiling diagnostic to a tool: `q/programs/policy_ceiling.q results/<arm> [...]`. *(2026-10-25: done. Shared maths factored into `q/modules/theory.q` (`.theory`); `theory_checks.q` now loads it and its full output is byte-identical to before. `policy_ceiling.q` reproduces the acceptance numbers exactly (L=1: 0.5400605 vs 0.5103204; L=0.25: 0.7092251 vs 0.6458473) in under 2 s. `-seeds` prints the per-seed table.)*
  - Per seed, it computes the weights-only policy (`chain`/`policy`/`stats` from `theory_checks.q`: the vision unit only, a 12-step bar, T=1, saturation ignored) from `weights_seed<N>.bin`. It prints predicted E[cos], P(exit within the bar) and MI next to the measured last-epoch alignment, plus their per-arm means and correlation.
  - Factor the shared chain/policy code into one place (e.g. `q/modules/theory.q`) rather than copy it.
  - Acceptance: it reproduces §6's numbers (L=1: predicted 0.540 vs measured 0.510; L=0.25: 0.709 vs 0.646).
  - Note: `chain` uses `.dirs.vBias` and T=1. If a task changes either, pass them in.
- [x] **G.R1** **Reward-prediction error** *(2026-10-25: tested inside G.R2 as PG with vs without `--rpe`. **No effect:** at η=1, +0.008 alignment, CI −0.128 to +0.140. Not kept; flag retained. §6.)* (REINFORCE baseline; the dopamine reward-prediction-error account, Schultz, Dayan & Montague 1997).
  - Define the per-move reward $R$ = the move's cosine to the mouse (the `--cos-reward` dotP). Keep a running average $\bar R \leftarrow \bar R + \beta(R-\bar R)$ (flag `--rpe`, rate `--rpe-rate β`, default ≈ 0.01, i.e. ~100 moves).
  - The direct reward becomes $\delta = R-\bar R$: reinforce if δ > 0 and punish if δ < 0, with log-step ∝ |δ| (replacing the adaptive threshold and the count-of-calls rule).
  - Why it should help **re-learning**: when the world or the reward changes, $\bar R$ is suddenly too high, δ goes negative for the old habits, and unlearning starts by itself.
  - Tests: δ arithmetic; $\bar R$ converges to a constant input; flag off is bit-identical.
- [x] **G.R2** **Policy-gradient-shaped update in log space** *(2026-10-25: tested at η ∈ {0.1, 0.3, 1, 2, 4}. **Not kept:** best at η≈1–2, which ties the reference (alignment −0.035/−0.037, CIs include 0) and is never better, while some seeds collapse (min catches 5–23 vs the reference's 644). η=4 is significantly worse on catches. Flags retained for later. §6.)* (REINFORCE; learning-automata reward–penalty schemes).
  - For each used directed state $s$ with chosen route $c$: $\Delta\theta_{s,o} = \eta\,\delta\,\alpha\,(\mathbb 1[o=c]-p_s(o))$ for **all 8** routes $o$. Here $p_s$ comes from the current row of `syns` × `vBias` (unmasked, T=1). For a HIGH cell, use the row of the label that was used. Apply $W \leftarrow \mathrm{clamp}(W e^{\Delta\theta})$.
  - Flag `--pg-update`, step `--pg-eta η` (sweep e.g. 0.05, 0.1, 0.2, 0.4). Updates then shrink automatically as a route becomes certain, which is the principled version of `--reward-scale`.
  - Scope of the first arm: replace the **direct** reinforce/punish only. Keep the catch boost and strategic lookback at the current best scale, then test removing them.
  - Tests:
    - $\sum_o \Delta\theta_{s,o}$ weighted correctly. Unweighted $\sum_o(\mathbb 1[o=c]-p_o)=0$, so the row's total log-change is zero when the clamps aren't hit.
    - The gradient matches a finite-difference derivative of $\ln p_s(c)$.
    - Unused states are untouched.
    - Flag off is bit-identical.
  - Run G.R1+G.R2 as one arm (δ is the third factor in G.R2), then each alone if the combination wins.
- [~] **G.R3** **Ratio of unconditional (usage) to directed (reward) reinforcement.** *(2026-10-25: `--hebb N` implemented as `.brain.HEBB`, default 1000 (golden unchanged); exact-increment unit test; `--hebb 0` aborts. Steady-state result: an exploratory sweep (seeds 1–16) favoured h=250, and a **pre-registered confirmation on fresh seeds 17–48 (n=32) holds**: alignment +0.043 (CI +0.002 to +0.083); catches +8.6% (CI −3 to +183); target count 16/32 vs 9/32 (McNemar p=0.17). **Working value h=250 for steady state.** Still open: the retooling side. h=250 lowers the unconditional share, so it must be re-judged in G.4 (reversal) before G.R3 is ticked.)* *(Revised 2026-10-25 at the user's direction: the unconditional term stays, §2 point 7. An earlier draft proposed removing it.)*
  - **Parametrise both terms.** The unconditional term is the per-use Hebbian amount $h$, currently a hard-coded `rAmt:1000i` in `.brain.stepNeuron`. Make it a module setting `.brain.HEBB`, defaulting to 1000 (bit-identical), set by a flag `--hebb N`. The directed term is already `--reward-scale λ` (and `--pg-eta` once G.R2 lands).
  - **The meaningful knob is their ratio.** In log space:
    - one use adds $\ln(1+h/W)$: large for weak routes, tiny for strong ones;
    - one reward event adds $\lambda\cdot 0.80\cdot k$, with $k$ the number of reinforce calls.
    - So $h$ mainly controls how fast *unrewarded* routes can grow back, i.e. retooling and exploration; λ controls how hard the goal pulls.
  - **Sweep** $h \in \{2000, 1000, 500, 250, 100\}$ at the current best λ = 0.25, then a small grid of (h, λ) around the best point. **h = 0 is not a candidate configuration** (§2, point 7).
  - Measure the usual G.R metrics, plus the G.R0 weights-only ceiling. Also measure **reversal recovery (G.4)**: the user's rationale is that the unconditional term enables retooling, so the ratio should be judged on re-learning as well as on steady-state alignment. Expect a trade-off; record it rather than optimising alignment alone.
  - **Optional refinement** (Izhikevich 2007; Frémaux & Gerstner 2016, three-factor rules): keep the unconditional term, and *add* a reward-gated eligibility trace $e_{s,o}$ (decay $\tau_e$, flag `--elig-tau`) as the vehicle for directed reinforcement. A catch is then a large δ applied to current eligibilities, which could subsume the 100-bar history buffer, the 20-bar catch boost and the strategic lookback. Keep those behind the flag until the trace version is shown equal or better.
  - Tests: `--hebb 1000` (and no flag) is bit-identical; `--hebb N` changes the per-use increment to exactly N; flag value ≤ 0 aborts; `.sub` keeps its own `hebb` setting, with `test_substrate.q` still passing.
  - **The substrate lab is untouched:** its channels come from unconditional strengthening (S.3), which is consistent with the user's view that usage reinforcement builds structure.
- [ ] **G.R4** **Bounded probabilities for re-learning** (MAX–MIN Ant System, Stützle & Hoos 2000).
  - The current cap/floor ratio is 2,000,000 / 25 = 80,000. A floor route next to a capped one has p ≈ 25/(7·25+2·10⁶) ≈ 1.2×10⁻⁵, so it's never tried again. **Prediction:** reversal (G.4) will be very slow with the current bounds. Undoing a capped route at reward scale 0.25 needs ≈ 11.3/0.21 ≈ 54 punish calls *per route on the path*.
  - Add `--weight-ratio ρ` (W_max/W_min, e.g. 100, 1000, 10⁴) so every route keeps p ≥ p_min.
  - Measure the trade-off: steady-state alignment (it may drop slightly from forced exploration) vs reversal recovery time (G.4). Test as part of G.4.
- [ ] **G.R5** **Two-timescale synapses** (synaptic consolidation; Fusi, Drew & Abbott 2005; Benna & Fusi 2016).
  - Each weight is $\theta = \theta_{fast} + \theta_{slow}$. Rewards update $\theta_{fast}$, which decays with a short $\tau_f$. A fraction κ of $\theta_{fast}$ consolidates into $\theta_{slow}$ each period, and $\theta_{slow}$ decays with a long $\tau_s$.
  - Rationale (THEORY.md §7): a single forgetting time $\tau_d$ can't be both short for re-learning and long for retention. Two can.
  - Flags `--two-timescale`, `--tau-fast`, `--tau-slow`, `--consolidate κ`. Weight files must save both parts; extend `test_weights.q`.
  - Test in G.4: retention, and recovery after reversal.
- [ ] **G.R6** **Hierarchy as actor–critic plus neuromodulation** (Barto, Sutton & Anderson 1983, adaptive critic; Doya 2002, "Metalearning and neuromodulation").
  - The meta-brain becomes the **critic**. It learns to *predict* the primary's reward from its own input (catch-rate code, optionally the vision slice). That prediction replaces G.R1's running average as $\bar R$, and the critic's own prediction error is its **basic reward**.
  - Optionally, the critic also sets the primary's inverse temperature α (exploration) and learning rate η, which are Doya's noradrenaline and acetylcholine roles. Today's scope and magnitude outputs are the existing hook.
  - This is exactly the user's hierarchy (§2, point 5): a system of the same kind sets the other's reward, and has its own more basic reward. It realises Phase 5 in literature-backed form.
  - Needs G.3(d) (meta saturation fix) first.
  - **Control:** it must beat the plain running-average baseline (G.R1) and its own ablation (`--ablate-meta`), paired, with ≥ 16 seeds. Otherwise report a null.

**References** (check details before citing externally):
- Williams 1992, *Machine Learning* 8 (REINFORCE)
- Narendra & Thathachar 1989, *Learning Automata: An Introduction*
- Thathachar & Sastry 2004, *Networks of Learning Automata*
- Schultz, Dayan & Montague 1997, *Science* (reward prediction error)
- Izhikevich 2007, *Cerebral Cortex* (distal reward and eligibility)
- Frémaux & Gerstner 2016, *Frontiers in Neural Circuits* (three-factor rules)
- Stützle & Hoos 2000, *Future Generation Computer Systems* (MAX–MIN Ant System)
- Fusi, Drew & Abbott 2005, *Neuron*; Benna & Fusi 2016, *Nature Neuroscience* (synaptic consolidation)
- Barto, Sutton & Anderson 1983, *IEEE Trans. SMC* (actor–critic)
- Doya 2002, *Neural Networks* (metalearning and neuromodulation)

### Phase 4 — Reward-dynamics hygiene
These are behavioural changes. Measure each one against the Phase 3 arm A baseline, with ≥8 seeds.
- [ ] **4.1** Normalise `dotP` by the move length, so `adaptThresh` is a real cosine threshold (F10).
- [ ] **4.2** Random tie-break in the output action (F11).
- [ ] **4.3** Replace repeated ×2.2 multiplications (`do[k; applyReinforce]`) with a single bounded update per bar (e.g. additive in the log domain, scaled by k). Unify caps (F10).
- [ ] **4.4** Stop meta-brain saturation: remove `do[rateCounter; …]`, or scale it to the log of the rate (F8).
- [ ] **4.5** Make the world consistent (F12): either fully toroidal (cat wraps; vision uses toroidal dx/dy) or fully bounded. **User decision (section 7).**

### Phase 5 — Meta-brain redesign so it *can* learn (needs Phase 4)
*Superseded in approach by G.R6 (actor–critic). Keep these tasks as the fallback if G.R6 doesn't beat its controls.*
- [ ] **5.1** Output-contingent credit. Record the meta-brain's synapse usage when it emits a decision. Some bars later, reward or punish those synapses by `Δrate` = (catch rate after) − (running-average baseline).
- [ ] **5.2** Gate output. One meta output decides *whether* strategic reinforcement fires, replacing the hard-coded `rateCounter>=3`.
- [ ] **5.3** Evaluation. The redesigned meta-brain must beat its own ablation (arm D style), paired, with ≥16 seeds and a CI excluding 0. Otherwise report a null result.

### Phase S — Substrate lab: get self-sustaining activity *before* reconnecting the cat
This phase was chosen by the user (§7 decision 5). It doesn't modify `cat_mouse.q`. The lab uses its own module, `q/modules/substrate.q` (`.sub`): a configurable copy of the propagation rule. **The core rule stays the same** (usage-weighted probabilistic energy moves, Hebbian strengthening, conservation). Only its surroundings change:

| Setting | Current brain | Lab can try |
|---|---|---|
| edges | walls, plus a sink at the bottom | wrap-around (toroidal), no sink |
| direction bias | downward `vBias` | flat (no drift) |
| randomness (`temp`) | 1 = weighted random | <1 sharper, 0 = always the strongest route |
| processing order | row-major | random each step (tests the F19 left-pile hypothesis) |
| Hebbian per use | +1000 | 0 = no learning |
| decay | every 60 steps (5 bars) | any interval or none |

- [x] **S.1** `.sub` module. Acceptance: with `.sub.defaults` it reproduces `.brain.propagate` **bit for bit** on seeded random states; energy is conserved with wrap on; test file `q/tests/test_substrate.q`. *(2026-10-24: done, `q/modules/substrate.q`, not in `load.q`.)*
  - 8 tests pass: bit-for-bit over 50 steps; conservation with wrap and sealing at temp 1, 0.3 and 0, random order, flat bias; wrap geometry; hebb 0.
  - `temp=1` keeps exactly the same RNG use as the original.
- [x] **S.2** *(2026-10-24: done.)* `q/programs/substrate_lab.q` (CLI options documented in its header), `q/programs/lab_report.q`, and `scripts/lab_sweep.sh SWEEP` (named sweeps; `s3` defined). About 2.5 ms per step on 16×16 at pressure 0.3. Original task spec: lab runner `q/programs/substrate_lab.q` (CLI config → `metrics.csv` + `summary.csv`), plus a sweep script and a report. Metrics, sampled every 50 steps:
  - energy;
  - fraction of cells changing per step (activity);
  - fraction saturated / empty (jam vs spread);
  - row and column spread of energy (banding/sedimentation);
  - **persistence** = correlation between the state now and 10 steps ago (≈1 frozen, ≈0 random churn, in between = moving structure);
  - **route sharpness** = average max choice probability over all (cell, inDir);
  - **route turnover** = fraction of (cell, inDir) whose strongest route changed since the last sample (reorganisation).
- [x] **S.3** Sweep 1 — closed system with no input. Which settings give activity that persists, isn't jammed, and builds routes that also turn over? Is F19's left pile caused by processing order? *(2026-10-24: done. 147 runs in 171 s; `results/lab/s3/report.txt`. Results are in §6 under "Phase S". Two report bugs were fixed: names with dots were skipped, and `cols` as a column name.)*
- [ ] **S.3b** Reorganisation. In S.3, route turnover in the structured configs (≈0.003–0.008 per 50 steps) sits at the noise floor that decay alone produces (hebb-0 runs: ≈0.004–0.014). So structures form but **don't break down and reorganise** (design intent, point 2).
  - Sweep decay strength and interval against `hebb` (e.g. decay every 5–60 steps; hebb 100–1000) on the two middle-regime configs.
  - Target: persistence still well above churn (~0.3–0.8) *and* turnover clearly above the noise floor.
  - Needs a `--decay-rate` option. Decay is currently fixed at ×0.98–0.995.
- [ ] **S.4** Perturbation response for promising configs. Run twin copies with identical randomness, poke one, and track how the difference evolves: it dies out (forgets), stays bounded, or spreads. This is the "input churns through it" property.
- [ ] **S.5** Open system: input injection plus a thermostat (set-point drain), and a non-destructive output readout.
- [ ] **S.6** Two coupled substrates: one sets the other's reward, and gets a basic reward itself (design intent, point 5).
- [ ] **S.7** **User decision:** what to port back into `cat_mouse.q`, then re-run Phase 3-style controls.

### Phase M — Mathematical theory (user request, 2026-10-24)
[THEORY.md](../../researchNotes/THEORY.md) is the reference. Every claim in it is tagged Derived / Verified / Consistent / Prediction / Conjecture. `q/programs/theory_checks.q` reproduces every Verified number.
- [x] **M.1** Write the theory. *(2026-10-24.)*
  - It models the brain as an absorbing Markov chain on (cell, inDir) states, with the policy as the within-bar absorption matrix. It covers drift, diffusion and residence time; crowding as a capacity-2 exclusion process; Little's law; learning as a generalised Pólya urn with exponent α=1/T; decay memory τ_d and the reinforcement number 𝓡; log-space reward arithmetic; hierarchy as control of 𝓡; and predictions P1–P7.
  - **Key verified results:**
    - the linear chain matches the real code within ~1 pt at low density;
    - the policy computed from weights alone predicts measured towards-rate (r=0.82) and catches (r=0.80) across the 16 trained brains;
    - residence time is 15.4 steps, longer than the 12-step bar;
    - the untrained policy is anti-aligned (E[cos] −0.22) because of the output wiring;
    - trained weights are bimodal (96% at the floor, ~3% ≥1M).
- [x] **M.2** Reproducible checks: `q/programs/theory_checks.q`. *(2026-10-24: runs in ~2 s; output matches THEORY.md.)*
- [ ] **M.3** Test P5: does the weights-only predictor also predict catches in arms B–D? If so, promote it to a tool (e.g. `q/programs/policy.q WEIGHTS`) and consider it as a cheap fitness or reward signal.
- [ ] **M.4** Test P4: angle-ordered output wiring (slot s → direction at 45s°). Add a `--wiring angle` flag to `cat_mouse.q` and run paired training (16 seeds). Compare acquisition speed and catches. **User decision first:** a built-in prior may or may not fit the project's goals.
- [ ] **M.5** Literature check before claiming novelty. The combination in THEORY.md §6.1 (many walkers + capacity-2 exclusion + direction-conditioned reinforcement + decay + tunable superlinearity) should be checked against the ACO, Physarum, reinforced-random-walk and exclusion-process literature.
- [ ] **M.6** Verify the mean-field fundamental diagram (P1) and Little's law with injection (P6), once S.5 adds a flux metric and an input. Then update THEORY.md's status tags.

Phase S tasks should use the theory's predictions: P2 and P3 belong in S.3b's design.

### Phase 6 — Documentation truth pass
- [x] **6.1** Rewrite CLAUDE.md: correct parameters, tests and flags; remove claims Phase 3 didn't support; keep the coding conventions; add the precedence and null rules from section 4. *(2026-10-24: done. The user's Tooling/qlint section is kept verbatim, and a "q pitfalls" section was added.)*
- [x] **6.2** README: replace the performance claims with Phase 3 results, or remove them. *(2026-10-24: rewritten for repo visitors. It covers what the simulation does, the honest research status with the Phase 3 table, quick start, flags, outputs and a docs index.)*
- [x] **6.3** BAR_STRUCTURE / CROSS_FLOW_ARCHITECTURE / OPTIMIZATION_NOTES / PROJECT_STRUCTURE: fix F13 items. Either implement or delete the "valve 2/3" text (**user decision**). *(2026-10-24: all four rewritten to match the code.)*
  - BAR_STRUCTURE is now an exact per-bar sequence covering every reward rule and its magnitude, with ⚠ markers linking to findings.
  - **Valves 2/3 are documented as "not wired in / not implemented"** rather than deleted or implemented, so §7 decision 2 is still open.
  - New measured fact, now documented: pressure peaks at about 0.056 over a 20k-bar run, so **the drain and valve 1 never engage**. That makes the whole pressure system inert in practice.
  - OPTIMIZATION_NOTES now holds the measured runtimes.
- [x] **6.4** `scripts/META_EXPERIMENT_README.md`: replace it with the Phase 3 protocol and results. *(2026-10-24: now a guide to running controlled experiments — rules of thumb, tools, the Phase 3 arms and results — plus a short history of why the old claim failed.)*

### Phase P — Speed up the existing simulation (user request, 2026-10-24)

**Profile.** I wrapped timers around each main-loop section of a scratch copy of `cat_mouse.q` and ran 1000 bars. Numbers are ms per bar:

| Section | fresh brain | trained brain (s103) |
|---|---|---|
| Strategic (meta-controlled) reinforcement | 1.71 | **11.63** |
| Move cat (includes catch boost + direct-reward reinforce/punish loops) | 2.49 | **6.40** |
| Propagation (12 primary + 24 meta steps) | 0.75 | 0.80 |
| Meta input-based reward | 0.11 | 0.19 |
| Decay, reset `su`, everything else | ~0.17 | ~0.17 |
| **Total** | ~5.2 | **~19.2** (20k bars ≈ 6.4 min single-threaded) |

**So brain propagation is only ~4% of the time.** About 95% goes to *applying rewards*: repeated whole-array passes over all 4,608 synapses through nested lambdas.

**Micro-benchmarks** (trained weights):

| Operation | nested (now) | flat vector | sparse (used synapses only) |
|---|---|---|---|
| `applyReinforce`, primary | 0.36 ms | 0.04 ms | 0.001 ms |
| one strategic-lookback bar | 0.385 ms | — | 0.001 ms |
| `applyDecay`, primary | 0.175 ms | — | — |
| `.brain.propagate`, one step, typical state | 0.022 ms | — | — |

Other facts from the benchmark:
- A typical bar uses **13 of 4,608 synapses (0.3%)**, so sparse updates win massively.
- **RNG fact:** 72 calls of `64?1.0` give exactly the same numbers as one `4608?1.0`. So flattening can stay **bit-identical**. Sparse updates draw fewer random numbers, so they are only **statistically identical**: the RNG stream diverges and runs differ trajectory by trajectory.
- **q gotcha:** `@[x;indices;f]` applies `f` to *each item separately*. For a whole-sub-list function, use `x[i]: f x i`.

**Techniques, in order** (estimated total: ~19 ms → ~1–1.5 ms per bar, **~15×**, for P.1–P.4):
- [x] **P.0** Golden-run regression test. Store the md5 of `-8!` (the serialised bytes) of the final state of a seeded 300-bar run. Tier-1 changes must keep it identical. Tier-2 changes update it *and* must pass a statistical equivalence check: ≥16 seeds, paired `compare.q`, with a CI on catches that includes 0. *(2026-10-25: `test_golden.q` reruns seed 42 for 300 bars and verifies MD5 `1148f06d98253588dc05ffa60b97c918`. This q build lacks a built-in md5 function, so it hashes the exact `-8!` byte vector with system `md5sum`.)*
- [x] **P.1** *(tier 1, bit-identical)* Flat synapse updates. Reward/decay updates flatten the nested synapse state to one int vector per operation, apply one vector expression, then restore the original nested shape; public brain/weight interfaces remain unchanged. *(2026-10-25: exact `test_golden.q` digest preserved; `test_synapses.q` verifies all three operations match the nested reference, including identical RNG continuation. A 20-call microbenchmark on 6×12 measured 4.5× faster reinforcement and 2.9× faster decay.)*
- [x] **P.2** *(tier 2)* Sparse usage. Record `su` as a list of used flat indices per bar, not a 4,608-boolean array, and make `hist` a 100-slot list of index lists. Reinforce/punish then touch only the used indices. *(2026-10-25: integrated sorted sparse usage/history in the simulation; dense propagation remains available to the substrate lab and saved weight files retain dense history. Dense/sparse propagation is equivalent in a seeded step; save/load with populated history and 2,000-bar reward/catch smoke tests pass. Updated tier-2 golden MD5: `4e7d460ae07a59feb626e8dcdcf615f5`. Full suite: 117 assertions pass. Matched 16-seed 20k comparison vs G.1: catches +38.19/run (95% CI −66.5 to +127), alignment −0.00612 (95% CI −0.04653 to +0.02823), both CIs include 0. A 200-call 6×12 reinforcement microbenchmark measured 18.20 ms dense vs 8.16 ms sparse (2.23×).)*
- [x] **P.3** *(tier 2)* Strategic lookback in one shot: count occurrences of each sparse synapse index across selected history slots, then update each weight once by `mag xexp occurrenceCount`, instead of repeated whole-array passes. Integer truncation may differ slightly from sequential scaling. *(2026-10-25: focused reference/cap tests pass; 2,000-bar smoke and P.0 digest pass; full suite: 112 assertions. Benchmark: 80 slots × 13 used indices, 20 repetitions: 51.40 ms sequential vs 1.11 ms counted (46.2×). Matched 16-seed 20k comparison vs P.2: catches +13.25/run (95% CI −1.5 to +41.25); alignment +0.00742 (95% CI −0.00365 to +0.02590); both intervals include zero.)*
- [x] **P.4** *(tier 2)* Collapse repeated sparse reward loops into one per-synapse update with multiplier `m^k` times the product of k independent noise draws. *(2026-10-25: batched helpers preserve RNG continuation in focused tests; full suite: 118 assertions. 100-call microbenchmark for eight repeats on 13 used synapses: reinforce 29.39 ms → 4.20 ms (6.99×); punish 28.08 ms → 4.25 ms (6.61×). Matched 16-seed 20k comparison vs P.3: catches −12.125/run (95% CI −30.375 to +1.3125), alignment −0.00020 (95% CI −0.01042 to +0.01004); both intervals include zero.)*
- [ ] **P.5** Propagation, once it's the bottleneck after P.1–P.4. In order:
  - (a) *tier 1:* avoid copy-on-write in the fold. Unpacking `acc` into locals and repacking a 6-tuple per neuron forces copies. Keep state in globals and amend in place (`.[`.brain.br;…]`), or fold over a flat state.
  - (b) *tier 1:* precompute per-cell neighbour indices and validity masks once, instead of recomputing `nrs/ncs/inBrain8/safeR/safeC` for every neuron at every step.
  - (c) *a **semantic change**, needs user approval:* a synchronous, vectorised step. Every active neuron chooses at once (n×8 weight matrix, row-wise `sums`, n random draws), then conflicts are resolved by capacity, with conservation still exact. This could be 10×+ on large grids, but it changes the dynamics from sequential to simultaneous, so test it in the Phase S lab first.
**Lab profile (Phase S), 2026-10-24.** In the substrate lab the picture is reversed: there are no reward passes, so **propagation is ~95–97% of the cost**.

| Config | active neurons/step | propagate | decay | metrics |
|---|---|---|---|---|
| 16×16, middle regime | 139 | 2.9 ms/step | 0.01 | 0.06 ms/step (3.0 ms per sample) |
| 16×16, gas regime | 126 | 2.5 ms/step | 0.01 | 0.06 |
| 32×32, middle regime | 548 | 12.0 ms/step | 0.04 | 0.23 |

- The cost is a constant **~21 µs per active neuron per step**: interpreter overhead of scalar q inside the fold. So it scales with grid area × fill. A 64×64 grid would be ~50 ms/step, ~2.5 min per 3000-step run.
- Under 16-way parallel sweeps, runs were ~2× slower per step from contention: S.3 runs took 12–24 s for 3000 steps.
- **Priority for Phase S is therefore P.5, not P.1–P.4.** Estimated gains:
  - P.5a/b (in-place state, precomputed neighbours) probably 2–3×, staying bit-identical.
  - P.5c (synchronous vectorised step) could reach ~1 µs/neuron, i.e. 20×+.
  - P.5c is a semantic change. Validate it *in the lab* by checking that the S.3 regimes (frozen / gas / channels) and their metrics reproduce. The random-order setting, already preferred because it removes the F19 artefact, is the closest sequential analogue to a synchronous update with random conflict resolution.
- The metrics code (`bestR`/`peakP`, nested lambdas over every cell × inDir) costs ~3 ms per sample at 16×16. Vectorise it too if sampling more often.

- [ ] **P.6** Minor: preallocate `simLog` columns rather than appending one row per bar, and build `resetSU` once and reuse it.
- [ ] **P.7** Re-profile after each step and record the before/after ms per bar here.

### Phase 7 — Optional
- [ ] **7.1** ~~Performance: profile `.brain.stepNeuron`.~~ Superseded by Phase P. The profile shows propagation isn't the hot path.
- [x] **7.2** Visualisation of `simLog` trajectories (the old Python tools were deleted in `aab6bff`). *(2026-10-25: superseded by the KX Dashboards viewer, see plans/inProgress/KX_DASHBOARDS_PLAN.md. Phases V0-V2 done.)*

---

## 6. Results

### Phase 3 — controlled re-test (2026-10-24)

**Setup:**
- Code as of the end of Phase 2: Phase 1 fixes plus the F17 magnitude restore.
- Training: 16 brains (ids 101–116) trained from scratch for 20k bars, with direct + meta rewards.
- Arms: 4 arms × 10k bars from each brain's saved state, with run seed = id+100 in every arm.
- Reproduce with `scripts/phase3_experiment.sh`. Raw outputs are in `results/p3_*` (gitignored); the full report is in `results/p3_report.txt`.

**Per-arm (catches per 10k-bar run, n=16):**

| Arm | Condition | Mean | SD | Min–Max | per 1k bars | towards % |
|---|---|---|---|---|---|---|
| A | continue training (direct+meta) | 310.5 | 80.4 | 192–494 | 31.1 | 83.8 |
| B | meta-only (`--meta-only`) | 156.8 | 171.4 | 1–570 | 15.7 | 63.6 |
| C | meta-only, no strategic rewards | 227.8 | 142.0 | 1–535 | 22.8 | 69.7 |
| D | meta-only, meta-brain ablated | 159.6 | 145.7 | 1–447 | 16.0 | 67.5 |
| train | from scratch, 20k bars (for reference) | 518.9 | 106.3 | 368–826 | 25.9 | 82.3 |

**Paired differences (matched by brain; bootstrap 95% CI of the mean difference):**

| Comparison | Mean diff | % | t | 95% CI | Reading |
|---|---|---|---|---|---|
| B − A | −153.7 | **−49.5%** | −5.27 | −206.7 … −97.1 | Removing tactical rewards **halves** performance. The branch hypothesis is **rejected** under a fair comparison. |
| D − B | +2.8 | +1.8% | 0.14 | −33.3 … +40.1 | Zeroing the meta-brain's outputs changes **nothing**. The meta-brain contributes nothing measurable. |
| C − B | +71.0 | +45.3% | 1.88 | −5.4 … +136.1 | Suggestive (but not significant) that strategic reinforcement *hurts* without tactical rewards. |

**Catches per 1000 bars by epoch (mean over the 16 brains):**

| | e1 | e2 | e3 | e4 | e5 |
|---|---|---|---|---|---|
| A | 29.9 | 27.6 | 31.8 | 31.3 | 34.6 |
| B | 20.4 | 17.4 | 16.3 | 13.2 | 11.2 |
| D | 19.2 | 17.0 | 16.1 | 14.0 | 13.5 |
| train (e6–e10) | 31.7 | 30.6 | 27.6 | 30.9 | 36.6 |

Meta-only doesn't *maintain* behaviour: it degrades steadily (20 → 11 per 1k bars). Continuing training keeps improving.

**Per-brain heterogeneity, which explains the old "+15%":**
- In arm B, outcomes are bimodal. Some brains collapse (ids 105, 109, 111 → 8, 9 and 1 catches), while **3 of 16 beat arm A** (103: 570 vs 494; 106: 370 vs 364; 115: 428 vs 404).
- Two of those three are among the strongest trained brains: 103 and 115 had the highest late-training rates, along with 112, which collapsed.
- The old experiment tested only the **best** trained brain, over 5 meta-only runs, against the all-brain training average (F7). That design would pick out exactly this tail.
- B and D track each other brain by brain (e.g. 111: 1/1; 109: 9/8; 115: 428/447). The outcome is set by the trained brain's state, not by the meta-brain.

**Caveats:**
- This is the fixed q code, not Fortran.
- `rateCounter` resets to 0 on a warm start, because catch timestamps aren't saved.
- Arms are 10k bars long.
- `dotP` is still unnormalised (F10), so "towards %" is a lenient metric.

### Phase G.1 — baselines (2026-10-25)

**Validation:** both arms contain exactly seeds 1–16, with one 20,000-bar summary per seed. Each epoch file has 10 rows totaling 20,000 bars. The no-learning summaries record `noLearning=1`, `directRewards=0`, and `metaRewards=0`.

| Arm | Mean catches | Catches/1k bars | Towards % | Mean alignment | Acquired (fraction) |
|---|---:|---:|---:|---:|---:|
| Current code | 513.44 | 25.6719 | 83.76 | 0.4113 | **11/16** (corrected, see F20) |
| No learning | 1.38 | 0.06875 | 21.57 | 0.01807 | 0/16 (0%) |

Acquisition is the first 500-bar rolling alignment above 0.6. ~~Neither arm reached it in these runs.~~ *Corrected 2026-10-25 (F20): that was a bug — `acquisitionTime` could never be set. Recomputed from the saved per-bar logs with the simulator's exact rule (the method reproduces the fixed simulator on seed 7: bar 18,045): current code acquired in **11/16** seeds, at bars 1619, 2908, 3382, 5704, 6410, 7908, 10250, 13177, 14188, 17478, 17561 (median 7,908 among those that acquired); no learning 0/16.*

**Paired difference (no learning − current, n=16; bootstrap 95% CI):** catches −512.06 (−99.73%, t=−12.88, CI −593.06 to −441.88); mean alignment −0.39319 (t=−23.29, CI −0.42588 to −0.36180).

**Epoch means, catches per 1,000 bars:**

| Arm | e1 | e2 | e3 | e4 | e5 | e6 | e7 | e8 | e9 | e10 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Current code | 11.4688 | 19.8125 | 21.1875 | 24.9688 | 26.0625 | 28.1875 | 25.8125 | 30.4063 | 33.5000 | 35.3125 |
| No learning | 0 | 0 | 0.125 | 0.0625 | 0.1875 | 0.03125 | 0.03125 | 0.09375 | 0.03125 | 0.125 |

**Epoch means, alignment:**

| Arm | e1 | e2 | e3 | e4 | e5 | e6 | e7 | e8 | e9 | e10 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Current code | 0.26998 | 0.36124 | 0.35427 | 0.41409 | 0.41104 | 0.43078 | 0.42540 | 0.44553 | 0.48307 | 0.49847 |
| No learning | 0.01733 | 0.01954 | 0.01508 | 0.01450 | 0.01669 | 0.01835 | 0.01856 | 0.02327 | 0.02374 | 0.01414 |

### Phase G.3(a) — cosine direction reward (2026-10-25)

Both arms are seeds 1–16 × 20,000 bars on the same code (after P.4 and the F20 fix); the only difference is `--cos-reward`. Report: `results/g3a_report.txt`; arms `results/g3a_base_20261025`, `results/g3a_cos_20261025`.

| Arm | Catches/run (sd) | Catches/1k bars | Towards % | Mean alignment | Acquired | Median bars to acquire |
|---|---|---|---|---|---|---|
| Baseline | 552.8 (152.7) | 27.6 | 80.4 | 0.412 | 11/16 | 6,927 |
| `--cos-reward` | **685.6** (105.4) | **34.3** | 73.5 | **0.447** | **14/16** | **5,543** |

**Paired (cos − baseline, bootstrap 95% CI):** catches **+132.8 (+24.0%)**, t = 2.96, CI **+44.7 to +213.3**; alignment +0.0349, t = 1.67, CI −0.0049 to +0.0741.

**By epoch:** the cosine reward helps most early. Epoch 1 catches per 1k bars are 24.1 vs 13.1, and epoch-1 alignment is 0.348 vs 0.254. By epoch 10 the alignment gap is small (0.510 vs 0.501).

**Unexplained:** the lenient towards % falls (80.4 → 73.5) while alignment and catches rise, so the cosine reward produces more strongly aligned moves but also more non-towards ones. Not investigated; a per-move cosine histogram would show it.

**Reading against §0:** the alignment target (≥ 0.7 in ≥ 14/16 seeds by 10k bars) is still far off: mean alignment at epoch 5 is 0.457. The cosine reward makes learning faster and more reliable, but doesn't raise the ceiling much. That points at the saturating ×2.22 reward arithmetic (G.3b) as the next lever.

### Phase G.3(b) — log-scaled rewards (2026-10-25)

All arms: `--cos-reward`, seeds 1–16 × 20,000 bars, differing only in `--reward-scale L`. Report: `results/g3b_report.txt`. The L=1 arm reproduces the G.3(a) cosine arm exactly (685.5625 catches, 0.4472066 alignment), confirming L=1 is bit-identical.

| L | Catches/run (sd) | Catches/1k | Towards % | Mean alignment | Acquired | Median bars to acquire | Seeds with alignment ≥ 0.7 by 10k bars (epoch 5) / by 20k (epoch 10) |
|---|---|---|---|---|---|---|---|
| 1 | 685.6 (105.4) | 34.3 | 73.5 | 0.447 | 14/16 | 5,543 | 0/16 / 1/16 |
| 0.5 | 982.9 (248.0) | 49.1 | 79.7 | 0.549 | 15/16 | 1,935 | 3/16 / 4/16 |
| **0.25** | **1,021.6** (228.7) | **51.1** | 84.0 | **0.596** | **16/16** | **1,200** | 2/16 / 5/16 |
| 0.1 | 1,012.9 (336.3) | 50.6 | 80.5 | 0.581 | 15/16 | 1,741 | 6/16 / 7/16 |

**Paired vs L=1 (bootstrap 95% CI):**

| L | Catches | Alignment |
|---|---|---|
| 0.5 | +297.3 (+43%), CI +176.3 to +419.1 | +0.102, CI +0.051 to +0.148 |
| 0.25 | **+336.1 (+49%), CI +228.4 to +451.3** | **+0.149, CI +0.106 to +0.195** |
| 0.1 | +327.3 (+48%), CI +156.8 to +498.4 | +0.134, CI +0.062 to +0.204 |

L=0.1 vs L=0.25 directly: catches −8.8 (CI −223.7 to +213.7), alignment −0.015 (CI −0.105 to +0.071). A plateau.

**Learning curve (mean alignment by epoch):** L=0.25 reaches 0.568 by epoch 2 and 0.611 by epoch 4; L=1 is at 0.358 and 0.471. Most of the gain is **faster acquisition**. The late plateau is ~0.61–0.65.

**P7 (THEORY.md) refuted.** Final primary weights, as fractions of 4,608, mean over seeds:

| L | ≤ 100 | 100 – 10⁶ (graded) | ≥ 10⁶ (strong) |
|---|---|---|---|
| 1 | 0.941 | 0.031 | 0.027 |
| 0.5 | 0.965 | 0.009 | 0.025 |
| 0.25 | 0.977 | 0.005 | 0.019 |
| 0.1 | 0.983 | 0.003 | 0.014 |

Gentler rewards give a sparser skeleton of strong routes, not graded weights, yet much better hunting.

**Against §0:** relative to the original G.1-style baseline (552.8 catches, 0.412 alignment, 11/16 acquired), the G.3(a)+(b) configuration gives **1,021.6 catches (+85%), 0.596 alignment, 16/16 acquired, median 1,200 bars**. The target (alignment ≥ 0.7 in ≥ 14/16 seeds by 10k bars) is **not met**: best is 6/16 (L=0.1). The ceiling of ~0.6–0.65 is now the main gap.

### Phase G.R3 — ratio of unconditional to directed reinforcement (2026-10-25)

**Exploratory sweep.** All arms: `--cos-reward --reward-scale 0.25`, seeds 1–16 × 20,000 bars, varying only `--hebb` (the per-use unconditional reinforcement; reference = 1000, arm `g3b_L0.25_20261025`). Report: `results/gr3_report.txt`.

| `--hebb` | Mean alignment | Δ vs 1000 (95% CI) | Catches/run | Δ catches (95% CI) | ≥0.7 at epoch 5 / 10 | Median epoch-10 alignment | Weights-only predicted E[cos] |
|---|---|---|---|---|---|---|---|
| 2000 | 0.577 | −0.020 (−0.078, +0.035) | 1,025.9 | +0.4% (−140, +133) | 4/16 / 6/16 | 0.663 | 0.690 |
| **1000** (ref) | 0.596 | — | 1,021.6 | — | 2/16 / 5/16 | 0.642 | 0.709 |
| 500 | 0.597 | +0.001 (−0.041, +0.046) | 1,093.3 | +7% (−35, +182) | 3/16 / 5/16 | 0.643 | 0.744 |
| **250** | **0.651** | **+0.054 (−0.019, +0.126)** | **1,126.6** | +10% (−51, +252) | **8/16 / 9/16** | **0.731** | 0.703 |
| 100 | 0.614 | +0.018 (−0.048, +0.082) | 1,042.3 | +2% (−119, +162) | 3/16 / 6/16 | 0.653 | 0.707 |

All arms acquire 16/16. The target count at epoch 5 for hebb 250 vs 1000 is 8 discordant seeds vs 2, giving exact McNemar p = 0.11: suggestive, not significant. **Reading:**
- The steady-state effect of the ratio is **modest**, with a possible optimum near h = 250 (about a quarter of the default).
- Raising the unconditional share further (h = 2000) doesn't help, and neither does shrinking it too far (h = 100).
- With effects this size, n = 16 is under-powered, so a confirmation on fresh seeds was set up before running it (below).

**Pre-registered confirmation** (set before running): hebb 250 vs 1000 on **new seeds 17–48 (n = 32)**, same flags. Primary measures: paired mean alignment, and the target count at epoch 5 (exact McNemar). Arms `gr3c_h1000_20261025`, `gr3c_h250_20261025`; report `results/gr3c_report.txt`.

| n=32, seeds 17–48 | hebb 1000 | hebb 250 | Paired (95% CI) |
|---|---|---|---|
| Mean alignment | 0.610 | **0.652** | **+0.0427 (+0.0019, +0.0834)**, t = 2.03 |
| Catches/run (sd) | 1,070.2 (210.2) | 1,162.5 (208.2) | +92.3, +8.6% (−2.8, +183.1) |
| Acquired / median bars | 31/32 / 1,210 | 32/32 / 1,420 | — |
| ≥ 0.7 at epoch 5 (10k bars) | 9/32 | 16/32 | discordant 13 vs 6, exact McNemar p = 0.17 |
| ≥ 0.7 at epoch 10 (20k bars) | 11/32 | 16/32 | discordant 11 vs 6, p = 0.33 |
| Weights-only predicted E[cos] | 0.723 | 0.722 | — |
| Measured last-epoch alignment | 0.671 | 0.706 | — |
| Execution loss (predicted − measured) | 0.053 | **0.016** | — |

**Confirmed on the primary measure** (small effect). The target-count improvement is in the same direction but not significant. The gain comes from a **smaller execution loss** rather than better weights (the weights-only ceiling is unchanged at ≈ 0.72). One untested reading: with less unconditional reinforcement, routes reinforced only by stray energy (for example meta cross-flow) carry less weight, so the vision signal dominates the action more.

**Not yet measured:** the user's other criterion for the ratio, retooling after conditions change. That needs G.4 (reversal).

### Phase G.R1/G.R2 — policy-gradient update with reward-prediction-error baseline (2026-10-25) — negative result

All arms: `--cos-reward --reward-scale 0.25`, seeds 1–16 × 20,000 bars. The only difference is the direct reward, which is `--pg-update --pg-eta η` (± `--rpe`, rate 0.01) instead of the threshold/count rule. The catch boost and strategic lookback are unchanged. Reference: `g3b_L0.25_20261025`, verified byte-identical under the new code (seed 1, log and weights). Reports: `results/gr12_report.txt`, `results/gr12b_report.txt`.

| Direct reward | Mean alignment | Δ alignment vs ref (95% CI) | Catches/run | Δ catches (95% CI) | Min catches | Acquired | ≥0.7 at epoch 5 / 10 | Weights-only predicted E[cos] |
|---|---|---|---|---|---|---|---|---|
| **Reference** (threshold/count rule) | **0.596** | — | **1,021.6** | — | 644 | 16/16 | 2/16 / 5/16 | 0.709 |
| PG + RPE, η=0.1 | 0.275 | −0.321 (−0.381, −0.258) | 297.7 | −71% (−825, −600) | 9 | 6/16 | 0 / 0 | 0.320 |
| PG + RPE, η=0.3 | 0.393 | −0.204 (−0.319, −0.091) | 562.3 | −45% (−688, −220) | 6 | 12/16 | 1 / 1 | 0.445 |
| PG + RPE, η=1 | 0.562 | −0.035 (−0.120, +0.049) | 887.9 | −13% (−330, +72) | 23 | 15/16 | 4 / 3 | 0.649 |
| PG + RPE, η=2 | 0.559 | −0.037 (−0.134, +0.053) | 904.2 | −11% (−357, +135) | 332 | 15/16 | 5 / 4 | 0.538 |
| PG + RPE, η=4 | 0.512 | −0.084 (−0.208, +0.020) | 777.6 | **−24% (−492, −22)** | 5 | 15/16 | 1 / 4 | 0.488 |
| PG alone, η=0.3 | 0.432 | −0.165 (−0.268, −0.067) | 675.0 | −34% (−564, −135) | 5 | 9/16 | 0 / 1 | 0.530 |
| PG alone, η=1 | 0.554 | −0.043 (−0.132, +0.040) | 877.0 | −14% (−361, +61) | 9 | 13/16 | 4 / 3 | 0.586 |

**Baseline (G.R1) effect at η=1:** PG+RPE minus PG alone gives alignment +0.008 (CI −0.128 to +0.140) and catches +10.9 (CI −292 to +313). No effect.

**Reading:**
1. **The ratio matters** (§2 point 7). Performance rises steeply with η up to ≈1–2, where PG's per-move log-step (≤ |δ|(1−p) · η) becomes comparable to the old rule's (0.2 per call × 1–7 calls at λ=0.25) and to the unconditional +1000-per-use term on weak routes. Below that, usage dominates and the goal pulls too weakly. That motivates G.R3.
2. **The principled rule doesn't beat the tuned heuristic here,** and it adds a collapse risk: several PG seeds end with < 25 catches, while the reference never goes below 644. Hypothesis, not tested: **interference through shared states.** Every directed state (cell, inDir) is traversed by paths from many vision slices, and PG also *lowers sibling routes* (−p) that other slices may need. The old rule only ever touches routes actually used.
3. **The reward-prediction-error baseline didn't help.** The move cosine is already roughly centred, near 0 for random moves, so a running average adds little.
4. **The weights-only predictor's validity is limited.** For PG at η ≥ 2, the measured last-epoch alignment *exceeds* the prediction (gaps −0.07, −0.12, and −0.015 for PG alone at η=1), so its low-density, vision-unit-only assumptions don't hold for those weights. Treat G.R0 readings for very different update rules with caution.

### Phase G — where the 0.6 ceiling lives (diagnostic, 2026-10-25)

**Method:** for each seed of the G.3(b) arms, the policy is computed from the final saved weights alone (THEORY.md §2–3 linear chain: the vision unit only, a 12-step bar, T=1, saturation ignored). It's compared with the measured mean alignment in the last epoch (bars 18,001–20,000). This was done with a scratch script; G.R0 makes it a tool.

| Arm (with `--cos-reward`) | Predicted E[cos] from weights | Measured last-epoch alignment | Predicted P(vision unit exits within the bar) | corr(predicted, measured), n=16 |
|---|---|---|---|---|
| `--reward-scale 1` | 0.540 | 0.510 | 0.875 | 0.69 |
| `--reward-scale 0.25` | **0.709** | **0.646** | 0.974 | 0.07 |

Best achievable with 8 slices × 8 directions ≈ 0.90.

**Reading:**
- **Most of the gap is in the weights** (0.71 vs 0.90): that's the learning rule's job, and Phase G.R exists for it.
- About 0.06 is lost in **execution** (weights → actual moves). Likely sources, untested: meta-brain energy entering the right column and driving moves, the NW tie-break (G.3c), and several units in flight per bar.
- At the best arm, the weights-only prediction no longer explains seed-to-seed differences (r = 0.07), so at the top end, execution noise dominates between-seed variation.
- Caveat: predicted E[cos] uses slice-centre angles, so it reads ~2–3% optimistic against true-angle alignment.

### Phase S — substrate lab, sweep S.3 (2026-10-24)

**Setup:**
- Closed 16×16 brain with no input and no sink, starting pressure 0.3, 3000 steps, decay every 60 steps.
- Full factorial over: wrap {0,1} × bias {down, flat} × temp {1, 0.3, 0} × order {rowmajor, random} × hebb {1000, 0}, 3 seeds each.
- Plus the current brain's surroundings (6×12, walls, sink).
- Reference: a well-mixed random gas at this pressure is ~9% full and ~49% empty, with persistence ≈0.
- Means over the second half of each run; full table in `results/lab/s3/report.txt`.

**Three regimes:**

| Regime | Settings | activity | persist10 | Picture |
|---|---|---|---|---|
| **Frozen** | temp 0 (always the strongest route); also temp 0.3 with row-major order + hebb | 0.3–5% | 0.92–1.0 | energy locks into fixed cells; almost nothing moves |
| **Churn / gas** | temp 1 with wrap; or *any* temp ≥0.3 with hebb 0 and wrap | ~50% | 0.01–0.06 | uniform gas, the equilibrium; no structure |
| **Middle** | wrap, temp 0.3, hebb 1000, random order (down *or* flat bias) | 15–18% | 0.68–0.74 | self-organised **channels** with ongoing flow |

**The design-intent result:**
- *Usage strengthening is what creates lasting structure* (§2, point 1). The same settings with hebb 0 give churn (persistence 0.04–0.06, all 256 cells active). With hebb 1000 they give channels (persistence 0.68–0.74).
- In a 500-step time-average of the middle regime (seed 1), about 61–63 cells are *never* occupied, forming corridors. About 82–93 cells are active more than 20% of the time, forming flowing lanes.
- With downward bias plus wrap, the lanes run vertically and circulate around the torus. That's **self-sustaining activity that stays in the brain.**

**Other findings:**
- **F19's left-side pile is caused by row-major processing order.** Confirmed: with walls and downward bias, column spread is 0.67–1.09 with row-major order vs 0.13–0.17 with random order.
- **Walls with downward bias always sediment** (bottom-heavy rows, row spread 0.67–1.48; 22–25% of cells full). Wrap removes the jam.
- **The current brain's surroundings** (6×12, sink) drain to 0 energy, as in F19.
- **Structures don't reorganise yet:** turnover in the middle regime is at the decay-noise floor. That's task S.3b.
- **Caveat:** these are channels or highways, not Game-of-Life-style moving objects. `persist10` can't tell a static lane network carrying a flow apart from slowly moving structures; S.4's perturbation test and better spatial metrics will help.

**Bottom line for the branch:** with fair controls, the reaction-dissociation hypothesis fails, and the meta-brain in its current form does nothing. The interesting, real phenomenon is that a minority of trained brains sustain or improve their hunting without any tactical reward, while most decay. *What distinguishes the self-sustaining brains?* That's a better-posed research question than the original one (see section 7, decision 4).

---

## 7. Open decisions for the user

1. **World geometry (4.5):** fully toroidal or bounded box?
2. **Unimplemented "valves" (6.3):** implement pressure-dependent leftward drain and bias steepening, or delete them from the docs?
3. **Fortran fidelity:** Phase 1 restores what the code *intended*, not what Fortran actually did (dead strategic path, fixed-slot catch boost). Is there any reason to reproduce the Fortran behaviour exactly, e.g. to replicate the old numbers? Current assumption: no.
4. **After Phase 3** (now answered: the meta-brain contributes nothing, and meta-only halves performance). Which direction next?
   - (a) Phase 4 then Phase 5: make the meta-brain able to learn, then re-test.
   - (b) Reframe around *why some trained brains self-sustain without tactical reward while most decay* (e.g. compare weight saturation, pathway sparsity and brain energy of ids 103/106/115 vs 105/109/111 at the end of training).
   - (c) Both, with (b) first, because it's cheap: the data is already in `results/p3_train/`.

   **Blocking Phase 4/5 until the user picks.**
5. **The user's actual goal** (stated 2026-10-24): self-propelling activity that lives in the brain, is perturbed by input, and develops emergent "machines", like Conway's Game of Life. F19 shows the current substrate can't do this. Open choices:
   - Is strict energy conservation a principle to keep, or was it just a means to an end? GoL-style emergence and strict conservation are in tension.
   - Which substrate direction to take? Proposed options:
     - **(A)** a 3-state cellular-automaton rule, e.g. Brian's Brain or Greenberg–Hastings excitable media, with the 4D synapses weighting neighbour influence;
     - **(B)** keep the conserved energy rule but seal the brain, read output without removing energy, add a homeostatic set point and a randomness ("temperature") control;
     - **(C)** first build a cat-free "substrate lab" that compares A and B on a bigger toroidal grid using persistence and emergence metrics.

   This may supersede decision 4.

   **ANSWERED 2026-10-24:** option A is rejected (see §2 design intent). Go with **B inside the lab (C)** → Phase S. The meta-brain is to stay a system of the same kind as the primary. That makes Phase 5's "output-contingent credit" redesign *optional*: revisit it only if the substrate fix alone doesn't make the meta-brain matter.

---

## 8. Progress Log

### 2026-10-24 — Session 1 (audit + plan + start of execution)
- Audit performed; findings F1–F16 recorded. Evidence came from q REPL checks, Fortran source in git history, and scratch-copy experiments (F9).
- Phase 0 done.
- **Phase 1 done.** Files changed: `q/modules/vision.q`, `q/modules/synapses.q`, `q/programs/cat_mouse.q`, `.gitignore`; added `q/tests/`.
  - The visionSlice fix took two attempts: `x mod y` is x modulo y, and I first had the arguments reversed. The test caught it.
  - **Sweep result (1.4):** no further precedence bugs.
    - Correct only by accident: the reinforcement lambdas `sc + hc*(min(MAX,sc*m) - hc*sc)`, which only work because `hc` is 0 or 1, and the slot formula `(hi - j + 100) mod 100`, which only works because q's `mod` is floored.
    - Left as-is but worth simplifying in Phase 4.
- **Phase 2 done.** Two new findings: F17 (strategic magnitude mis-ported, ×2.2 vs Fortran ×1.21; restored) and F18 (final-epoch move counts dropped).
  - **Behaviour changes in Phase 2:** F17 alters learning dynamics, so Phase 3 numbers won't match the F9 scratch numbers or the 1.8 smoke test. Everything else in Phases 1–2 is a bug fix, plumbing or output.
- Pitfall hit: `system "q …"` silently did nothing from inside a q script. Fixed with `env q`, and added to section 4.
- **Phase 3 launched:** `scripts/phase3_experiment.sh` with defaults (16 brains × 20k training bars, then 4 arms × 10k bars).
- **Phase 3 done** (3.1–3.3). The results are in section 6: B−A = −49.5% (CI −207…−97 catches); D−B ≈ 0. 3.4 (report to the user and get their decision) is pending.
- Nothing has been committed. All changes are in the working tree on `reaction-dissaccociation`.
- **Phase 6 done** (on the user's request, ahead of Phases 4/5). All seven docs now describe the current code and results.
  - Also corrected my own earlier estimates of test runtime (~5–10 s claimed; actually ~0.4 s for the whole suite).
  - Added task 3.5: a no-learning baseline, because the README's learning numbers lack a chance-level reference.
  - **When code changes in Phases 4/5, update BAR_STRUCTURE.md**, which documents every reward rule and magnitude, **and CLAUDE.md's quick reference.**
- **The user clarified the project's real goal:** self-sustaining, emergent, GoL-like activity, perturbed by input.
  - I measured activity persistence and recorded it as F19: the activity drains in about 4 bars, and a sealed brain jams into random churn instead.
  - Added §7 decision 5, the substrate direction, which probably takes priority over decision 4.
- **The user chose: no option A; B inside a substrate lab** (Phase S). Their design intent is recorded in §2. F8 was corrected: the meta-brain loop exists, it's just slow, and the substrate is the likelier problem.
- **The user asked for speed-up techniques in the plan → Phase P**, written from a real profile.
  - Key result: rewards, not propagation, are ~95% of runtime, and a sparse approach could be ~15× faster overall.
  - OPTIMIZATION_NOTES.md "where the time goes" was wrong (it named propagation) and has been corrected.
  - Profiling and benchmark scripts were scratch only. The method: insert `.prof.mark[`section]` before each `  / ---- X ----` loop comment, accumulating `.z.p` differences.
- **Phase S progress:** S.1 (`.sub`, bit-identical to `.brain` by default) and S.2 (lab runner, report, sweep script) are done.
  - S.3 is done: usage strengthening turns a uniform gas into self-organised, circulating channels (wrap, temp 0.3, random order). F19's left pile is explained by processing order.
  - Next is S.3b: make the structures reorganise (decay vs hebb balance), then S.4: perturbation.
- **Lab profiled** (the user asked for speed notes): propagation ≈ all of the lab's cost, ~21 µs per active neuron per step. Phase P now says P.5 is the priority for Phase S, and P.1–P.4 for the cat simulation. OPTIMIZATION_NOTES.md updated.
- **The user asked for mathematical rigour → Phase M.** THEORY.md and `q/programs/theory_checks.q` were written; M.1 and M.2 are done.
  - Two hypotheses of mine were wrong and were corrected before writing: the untrained policy isn't "straight down" but a blurred diagonal; and weights aren't "pinned at the cap" but bimodal, with decay pulling them back down after saturation.
- **The user set the north star:** the cat must actually learn to head towards the mouse, and all other work serves that. Added §0 (goal metrics and targets) and **Phase G** (the goal track), and reprioritised: G first, P as its enabler, S and M only where they feed G. The user also approved committing all work so far.
- **At that point, the next session should:**
  0. **Start Phase G at G.0**, unless the user has redirected.
  1. Check section 7 for the user's answers.
  2. If none, ask decision 4 before starting Phase 4/5.
  3. Task 3.5 can proceed regardless.

### 2026-10-25 — Session 2 (G.0 alignment measurement)
- Added `.vision.alignment`, with tests for straight toward (+1), straight away (−1), and perpendicular (0).
- Added actual, clamped movement alignment against the mouse position from before the move/catch respawn to `simLog`; non-moves and undefined zero-vector cases are null.
- Added mean alignment to run and epoch CSVs. Acquisition time records the first bar where the rolling 500-bar mean over valid moves exceeds 0.6; it remains null when not reached.
- Extended `compare.q` with per-arm means, paired alignment differences and epoch means. A two-seed, 600-bar smoke comparison loaded both new CSV schemas successfully; acquisition was not reached in either arm.
- Validation: `q/tests/run_all.sh` passed (73 assertions); `q/tests/test_weights.q` passed; geometry acceptance cases passed. Two matched 600-bar arms loaded and compared with the expanded summary schema; this was plumbing validation, not a performance experiment.
- G.1(a): added `--no-learning`; propagation remains active, but Hebbian changes, all reward/catch updates and decay are disabled. Focused same-seed weight-freeze checks pass for both brains.

### 2026-10-25 — G.1 baseline batch
- Resumed the already-started no-learning batch after confirming no trial processes were active and no completed summaries existed. The runner completed seeds 1–16 in 33 seconds; no duplicate completed runs were launched.
- Validated both arms: each has one 20,000-bar summary per seed and 10 epoch rows totaling 20,000 bars per seed. No-learning flags were verified in every baseline summary.
- Ran `compare.q` for `g1_current_20261025` vs `g1_no_learning_20261025`. The no-learning arm averaged 1.375 catches and 0.01807 alignment, versus 513.4375 catches and 0.41126 alignment for current code; paired CIs and epoch means are recorded in §6. Neither arm acquired (0/16).
- `q/tests/run_all.sh`: all tests passed. No later phase was started.

### 2026-10-25 — P.0 golden regression
- Added `q/tests/test_golden.q`, which reruns seed 42 for 300 bars, writes the exact `-8!` final-state bytes, and checks their MD5 against `1148f06d98253588dc05ffa60b97c918`.
- The installed q runtime does not expose a built-in `md5` or `.Q.md5`, so the test hashes the unchanged serialized byte vector with `md5sum`.
- Focused acceptance: 3/3 passed (byte-for-byte serialization round-trip, md5sum output, golden digest). Next: P.1 flat synapse storage.

### 2026-10-25 — P.1 vectorized synapse updates
- Updated `.syn.applyReinforce`, `.syn.applyPunish`, and `.syn.applyDecay` to flatten each nested synapse array once, perform the arithmetic over a single vector, and reshape to the original 4D structure.
- Preserved bit identity: P.0's 300-bar digest is unchanged. Added `test_synapses.q` to compare old nested reference implementations and post-operation RNG values; all seven assertions pass.
- Microbenchmark (20 calls, 6×12): reinforcement 7.49 ms → 1.68 ms (4.5×); decay 3.59 ms → 1.24 ms (2.9×). Timing includes flatten/reshape overhead.
- Full test suite: 83 assertions pass. Next: P.2 sparse usage/history; it is tier 2 and needs the planned ≥16-seed paired equivalence check.
- User reminder: keep the plan checkboxes and progress log current as each acceptance passes. The open copy under `plans/inProgress/REVIVAL_PLAN.md` was behind; its G.0, G.1, 3.5, P.0 and P.1 status/results are now synced. P.2 prototype is still in progress, not complete.
- P.2 complete: integrated sparse usage/history into the simulation; dense propagation remains available to the substrate lab. One-step dense/sparse equivalence, populated-history save/load, and a 2,000-bar strategic/catch smoke run pass. Tier-2 golden MD5 is `4e7d460ae07a59feb626e8dcdcf615f5`. Full suite: 117 assertions pass. Paired 16-seed results: catches +38.19/run (CI −66.5 to +127); alignment −0.00612 (CI −0.04653 to +0.02823); both intervals include zero. Sparse reinforcement is 2.23× faster in the 6×12 microbenchmark (200 calls, 18.20 ms dense vs 8.16 ms sparse). Next: P.3 strategic lookback batching.
- P.3 implementation milestone: strategic lookback now combines sparse route counts and scales each used weight once. Focused sequential-reference/cap tests pass, the 2,000-bar seed-42 smoke and golden digest are unchanged, and the full suite passes 112 assertions. An 80-slot / 13-index microbenchmark measured 51.40 ms sequential vs 1.11 ms counted over 20 repetitions (46.2×). P.3 remains in progress until paired 16-seed equivalence completes.
- P.3 complete: paired 16-seed 20k comparison vs P.2 gives catches +13.25/run (95% CI −1.5 to +41.25) and alignment +0.00742 (95% CI −0.00365 to +0.02590); both include zero. G.2's next item is P.4 reward-loop batching.
- P.4 implementation milestone: repeated sparse reinforce/punish calls now batch per-synapse noise products. Focused RNG-stream and bounded-rounding checks pass; full suite: 118 assertions. 100-call benchmark (8 repeats, 13 used indices): reinforce 29.39 ms → 4.20 ms (6.99×), punish 28.08 ms → 4.25 ms (6.61×). Paired equivalence vs P.3 is pending; P.4 remains in progress.
- P.4 complete: batched repeated sparse reinforce/punish loops using per-synapse products of independent noise draws. Focused tests verify RNG continuation and bounded rounding; the full suite passes 118 assertions. Microbenchmark (100 repetitions of eight calls, 13 used indices): reinforce 29.39 ms → 4.20 ms (6.99×); punish 28.08 ms → 4.25 ms (6.61×). Paired 16-seed 20k comparison vs P.3 passes: catches −12.125/run (CI −30.375 to +1.3125), alignment −0.00020 (CI −0.01042 to +0.01004); both include zero. No later task started.

### 2026-10-25 — KX Dashboards viewer (7.2)
- Added opt-in `--viz-port`/`--viz-delay-ms`, `q/modules/viz.q`, `q/programs/viz_server.q`, `dashboards/` and `q/tests/test_viz.q` (18 assertions). With viz on, seed 42 x 2000 bars gives a byte-identical `simulation_log.csv`; publishing costs about 11% wall time.
- `scripts/e2e_dashboards.sh` drives the dashboard in Firefox with Playwright against the real sim. No learning or performance claims were made.

### 2026-10-25 — Session 3 (doc consolidation, G.2 close-out, F20)
- **Docs consolidated.** The user's restructure left two copies of the plan and of five research notes; the plan copies had diverged (root had the G.1 tables and session logs, `plans/inProgress/` had the current P.3/P.4 status). Merged into one canonical `plans/inProgress/REVIVAL_PLAN.md`, kept the newer BAR_STRUCTURE (root, with the `--no-learning` and alignment notes), moved all research notes to `researchNotes/`, removed the root duplicates, and rewrote every relative markdown link for the new locations (script-checked: no broken links). CLAUDE.md now documents the layout and that work happens on `main`.
- **G.2 done:** a fresh 20,000-bar run takes 20.8 s (target < 60 s).
- **F20 fixed:** acquisition time was never recorded. Corrected G.1 baseline: current code acquires in 11/16 seeds (median 7,908 bars among those), not 0/16.
- Next: G.3(a), normalised direction reward.
- **G.3(a) done and kept:** `--cos-reward` (opt-in; default unchanged, golden test passes; `test_weights.q` checks the flag is recorded and changes learning). Paired 16 seeds × 20k: catches +24% (CI +44.7 to +213.3), alignment +0.035 (CI includes 0), acquired 14/16 vs 11/16. Results in §6. Supporting changes: `compare.q` now types summary columns by name (old and new summaries load together; arms are union-joined) and reports seeds acquired and median bars to acquire; `summary.csv` gains a `cosReward` column. BAR_STRUCTURE.md and CLAUDE.md document the flag. Full suite: 129 assertions pass.
- Next: G.3(b), bounded log-space reward increments, on top of `--cos-reward`. Needs a choice of step size η (THEORY.md §8 suggests η ≪ 11.3, the floor-to-cap range in log space); plan to sweep a few values in the lab-style paired setup.
- **G.3(b) done and kept:** `--reward-scale L` (opt-in, default 1, bit-identical; RNG use unchanged). Sweep with `--cos-reward`: L=0.25 gives catches +49% and alignment +0.149 vs L=1 (both CIs exclude 0), 16/16 acquired, median 1,200 bars. 0.1–0.5 all beat L=1; 0.1 vs 0.25 is a plateau. Kept at 0.25. THEORY.md P7 was refuted (weights got sparser, not graded) and is re-tagged there. Tests: `test_sparse_synapses.q` +7 (L=1 identity, same RNG draws, log-change scales by L, unused untouched, scaleMult); `test_weights.q` +4 (recorded in summary, changes learning, L ≤ 0 aborts); full suite 140 assertions pass, golden unchanged. `compare.q` and `test_weights.q` now type summary columns by name.
- North star: best configuration is now `--cos-reward --reward-scale 0.25`: alignment 0.596 (from 0.412), catches +85%, 16/16 acquire. Target not met (≥ 0.7 in ≥ 14/16 by 10k: best 6/16). Next: G.3(c) random tie-break, then (d) meta saturation, each on top of the best configuration.

### 2026-10-25 — Session 3 (continued): literature-backed learning-rule plan
- The user asked whether the maths points to existing theory that could improve results while staying general (able to re-learn), reward-focused and true to the core concept.
- Ran a ceiling diagnostic (§6): at the best configuration the weights alone predict alignment 0.71 (max ≈ 0.90), and execution loses ≈ 0.06. So the learning rule is the main bottleneck.
- Derived that the route choice is a softmax policy in log-weights, so REINFORCE gives the principled update, and the existing `su` path credit already has its shape. Missing: a reward-prediction-error baseline, the (1−p)/−p terms, and reward-gating of the Hebbian term.
- Added **Phase G.R** (G.R0–G.R6) with designs, flags, tests, acceptance rules and references. Phase 5 is marked as superseded in approach by G.R6.
- The user approved adding these to the plan. Nothing implemented yet. Nothing committed yet: the working tree holds this session's and the other chat's work.
- Next: G.R0, then G.R1+G.R2.
- **User direction:** unconditional usage reinforcement must stay, because it's what lets the system retool and be creative; tune its ratio to directed reinforcement instead. Recorded as §2 point 7 and in CLAUDE.md's "What NOT to do". G.R3 was rewritten from "remove/gate the Hebbian term" to "sweep the unconditional:directed ratio" (`--hebb N` with λ; h = 0 is not a candidate), judged on reversal too.
- **G.R0 done:** `q/modules/theory.q` and `q/programs/policy_ceiling.q`; acceptance numbers reproduced exactly.
- **G.R1/G.R2 implemented** (in progress until the sweep is analysed): `.syn.applySparsePG` plus the flags `--pg-update --pg-eta --rpe --rpe-rate`. `--rpe` alone aborts: the baseline is applied only inside the PG update, so "G.R1 alone" is measured as PG with vs without `--rpe`. Unit tests: chosen +(1−p) / siblings −p, row sum ≈ 0, finite-difference gradient, unused states untouched, δ=0 no-op, no RNG. Full suite 153 assertions pass; golden unchanged. With the new flags off, a seed-1 20k run is byte-identical to the `g3b_L0.25` reference (log and weights), so that arm is reused as the reference. Sweep running: PG+RPE η ∈ {0.1, 0.3, 1.0} and PG-only η=0.3, all with `--cos-reward --reward-scale 0.25`.
- **G.R1/G.R2 analysed: not kept** (negative result, §6). Swept PG η ∈ {0.1, 0.3, 1, 2, 4} with RPE, plus PG alone at η 0.3 and 1. Best (η≈1–2) ties the reference and is never better; some seeds collapse; RPE has no effect. The η trend supports the user's ratio principle. The flags and `.syn.applySparsePG` stay in the code. Next: G.R3, the ratio sweep, on the reference rule.
- **G.R3 steady state:** an exploratory `--hebb` sweep (seeds 1–16) favoured h=250, then a pre-registered 32-seed confirmation on fresh seeds held on alignment (+0.043, CI +0.002 to +0.083). The gain is a smaller execution loss (0.053 → 0.016); the weights ceiling is unchanged (≈0.72). Working configuration: `--cos-reward --reward-scale 0.25 --hebb 250`. Because this lowers the unconditional share the user values for retooling, G.R3 stays open until G.4 measures reversal. Tests: exact-increment test in `test_conservation.q`; `--hebb` recorded, changes learning, rejects 0; full suite 160 assertions pass, golden unchanged.
- Next: G.4 reversal (`--permute-outputs-at BAR`), then judge h ∈ {250, 1000} and G.R4/G.R5 on recovery.
