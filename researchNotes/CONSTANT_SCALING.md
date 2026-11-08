# Constants from the equations: how they scale with the brain

**What this is.** Tasks R.1 and R.2 of [CATCH_RATE_PLAN.md](../plans/inProgress/CATCH_RATE_PLAN.md) Phase R (2026-11-08): the analysis behind the user's principle that "the equations that created what is possible here should be built into any future runs of the brain … if I request a larger brain, some of these constants … should scale based on the mathematics automatically" (revival plan §2 point 10).

**What this is not.** Not a test. Nothing here ran the simulator with a new setting. The only runs are existing configurations with learning off (`--central-rows N --no-learning`), to check the geometry calculation in §2.

Notation is as in [CATCH_RATE_ANALYSIS.md](CATCH_RATE_ANALYSIS.md) (its table at the top). New here:
- **S:** propagation steps per bar (12 today).
- **F(t):** the share of a vision unit's energy that has reached the outputter within t steps of entering.
- **K:** number of vision slices (8).
- **c:** catches per bar.

---

## 1. Which constants depend on what

Each reward is a multiplicative step, so the conditions are all in log-weight per route use. From `synapses.q` and `bar_loop.q` (Derived):

| Quantity | Per route use | Depends on |
|---|---|---|
| One punish call, P | λ (ln 1.25 + S ln(1/0.95)) | **S**, λ |
| One reinforce call | λ (ln 1.2 + S ln(1/0.95)) | **S**, λ |
| Strategic, C_strat | λ · ln 1.1 · scope · magnitude (when on) | meta-brain outputs: scope ≥ 20 bars, magnitude ≥ 1 |
| Catch boost, C_catch | λ · ln 1.5 · 20 · c | catch window (20 bars), catch rate |
| Hebb | ln(1 + h/W) | h, W; **not** λ |
| Decay | δ per bar; δK per slice visit | K |

**Brain size enters only through S, and S is fixed today.** Rows, columns and path length don't appear in any per-use condition. They matter through what a bar *is*: how much of the input's energy reaches the output within the bar's S steps. With S fixed, a bigger brain changes P, and therefore the separation condition and the re-learning limit, only indirectly, by changing what a bar's move responds to (§2).

λ cancels from the separation condition nP > C (both sides scale with it), but not from the re-learning limit ln(1 + h/F) > nP, because Hebb is additive and doesn't scale with λ.

## 2. What a bar is: steps per bar from the brain's geometry

An untrained brain's weights are all equal on average (initial 25–1000, uniform), so a unit's routing follows the direction bias alone. That chain (`.theory.chainG`, `chainMod`) gives the exit-time distribution of a vision unit exactly. In the modular brain, a unit leaving the sight brain enters the central brain and moves again in the same step, so one step is subtracted.

| Brain | Mean steps to exit | Median | F(12) | F(24) | Steps for 50% / 80% / 90% |
|---|---|---|---|---|---|
| Modular, central rows 2 | 10.0 | 8 | 0.76 | 0.97 | 8 / 13 / 17 |
| **Modular, central rows 3 (today)** | **12.7** | **11** | **0.60** | 0.94 | 11 / 17 / 21 |
| Modular, central rows 4 | 15.4 | 14 | 0.43 | 0.89 | 14 / 20 / 25 |
| Modular, central rows 6 | 20.6 | 19 | 0.15 | 0.74 | 19 / 27 / 32 |
| Modular, central rows 9 | 28.1 | 26 | 0.006 | 0.42 | 26 / 36 / 42 |
| Modular, central rows 12 | 35.5 | 34 | 0.000 | 0.15 | 34 / 44 / 51 |
| Single 6×12 | 14.7 | 13 | 0.48 | 0.90 | 13 / 19 / 24 |

**Check against the simulator (Measured, learning off, 4 seeds × 400 bars).** The untrained cat moves on 78%, 81%, 83% and 82% of bars at central rows 3, 4, 6 and 9, and the energy held in the brain at the end of a bar rises from 0.6 to 0.8, 1.4 and 2.3 units. The chain's mean residence at 9 rows is 28.1 steps = 2.3 bars, the same figure.

**What that means.** A deeper brain doesn't stop moving: energy is conserved, so in steady state one unit leaves per bar on average, whatever S is. What changes is **which input the move answers**. At 9 rows, under 1% of a bar's move comes from that bar's vision; the rest is input from one to three bars earlier. Rewards, though, go to the routes used *this* bar, and the reward is judged against the mouse's direction *now*. So credit is assigned to the wrong input. Today's brain is at F(12) = 0.60.

**Rule (Derived; calibrated to today).** Choose S so that a bar passes the same share of the input's energy as today's tested brain:

$$S = \min\{\,t : F(t) \ge F_{\text{today}}(12) = 0.599\,\}$$

It gives exactly 12 for today's brain, by construction. Other sizes: central rows 2 → 10, 4 → 15, 6 → 21, 9 → 29, 12 → 37; the single 6×12 brain → 15.

The meta-brain's 24 steps per bar and the step count in its rewards belong to its own (unchanged) 3×5 geometry, so they stay fixed when S changes.

## 3. Punish calls from the separation condition

A wrong route only falls if nP > C (CATCH_RATE_ANALYSIS §3.1). C must be bounded for every state the cat can reach without the meta-brain's say-so:
- **Strategic at its floor:** scope 20, magnitude 1, which is what the meta-brain outputs on about 94% of bars. If the meta-brain raises scope or magnitude, strategic can beat punishment; that is the meta-brain exercising its authority (revival §2 point 5), not a failure of the rule.
- **Catch boost at the best memoryless catch rate:** the scripted reactive cat's 126.5 per 1,000 bars in the open world (Measured, OBSTACLES_RESULTS). The brain is memoryless (§3.6 of the analysis), so it can't collect more catch boost than that for long. Walled worlds catch less, so the rule is conservative there.

So C_ref = λ (20 ln 1.1 + 20 × 0.1265 × ln 1.5) = 0.733 at λ = 0.25, and

$$n = \min\{\,n : nP \ge m_{\text{sep}}\, C_{\text{ref}}\,\}$$

The margin m_sep is needed because n is an integer: with no margin, some sizes land on nP/C = 1.004 (central rows 2). **m_sep = 1.1** keeps every size at least 10% clear and gives today's 4.

| Brain | S | P | n | nP / C_ref |
|---|---|---|---|---|
| Central rows 2 | 10 | 0.184 | 5 | 1.26 |
| **Today (3)** | **12** | **0.210** | **4** | **1.14** |
| Central rows 4 | 15 | 0.248 | 4 | 1.35 |
| Central rows 6 | 21 | 0.325 | 3 | 1.33 |
| Central rows 9 | 29 | 0.428 | 2 | 1.17 |
| Central rows 12 | 37 | 0.530 | 2 | 1.45 |
| Single 6×12 | 15 | 0.248 | 4 | 1.35 |

Deeper brains need fewer calls, because each call is larger at larger S.

## 4. Hebb from the re-learning limit

Hebb must lift a punished route off the floor: ln(1 + h/F) > nP (analysis §3.2). The tested configuration (h = 250, F = 100, nP = 0.839) clears it by a factor ln 3.5 / 0.839 = 1.49. Keeping that margin, **m_h = 1.5**:

$$h = F\,\big(e^{\,m_h\, nP} - 1\big)$$

Today: h = 252 (against 250 tested). At other sizes: central rows 2 → 289, 4 → 342, 6 → 332, 9 → 261, 12 → 391; single brain → 343. This is where **the Hebbian number scales with the brain**, as the user asked. It is h rather than the floor that's derived, because the floor and the cap set the weight scale (cap ÷ floor = 20,000, which fixes the wall-trap escape time, analysis §3.7).

Hebb also depends on λ through P: at reward scale 1, h would be about 15,000. The two are no longer independent settings.

## 5. Decay

**No derivation yet for its value.** Decay ×0.5 was chosen from the model's sweeps because it restores deviation (analysis §3.4); the mechanism wasn't derived, and deviation's dependence on decay is steep (×0.5 → 0.33, ×2 → 0.005 in the model) with no closed form.

**What does scale:** a route serving one slice is used about once every K bars, so what decay does to it is δK per visit. Holding that at the tested value gives decay multiplier = 0.5 × 8/K. With today's 8 slices it stays 0.5 at every brain size; it would change with the vision plan's richer input.

**Not compensated:** longer paths in deeper brains have more decision points, so deviation per bar should rise with depth for the same per-step statistics (deviation = 1 − Π of the per-step shares). There's no rule for that; R.5 measures it.

## 6. What stays fixed, and why

- **Floor 100 and cap 2,000,000:** the weight scale; only ratios matter, and h is derived against them.
- **Strategic gate (3 catches in 200 bars), scope range, catch window, history depth:** time constants in bars, set by the task, not the brain's size.
- **Columns:** the brain is 12 columns wide with vision and outputs at columns 2–9; `--central-rows` is the only size control today. A wider brain would need new infrastructure; the rules above take any shape.

## 7. R.2: why the gate arm can't re-learn

From the existing re-learning runs (`results/cr_rl_*_20261108`), Measured:

| Arm | Catches/1k after the leg reversal | Bars with strategic on | Alignment, last 1,500 bars of L180 |
|---|---|---|---|
| base | 49.3 | 90% | +0.50 |
| main | 43.9 | 77% (one seed 7%) | +0.49 |
| gate | 13.7 | **1%** | +0.13 |

**The gate locks itself shut.** Strategic needs 20 catches in 200 bars; after the reversal the cat catches about 3 in 200, so strategic stays off for the whole block, and hunting never gets good enough to switch it back on. Gate's slices settle at mixed values (+0.1 to +0.5) instead of resolving. It snaps back to 0.87 the moment the original world returns, so the old policy is kept, not lost.

**Why strategic being off stops re-learning isn't derived.** In the separation arithmetic, gate should un-learn old routes faster (net −0.63 per use against main's −0.36). One candidate, not tested: strategic is the only credit that reaches routes whose own bar wasn't rewarded, so newly found good routes grow more slowly without it. **Conclusion for Phase R:** a fixed catch-count gate is a trap during re-learning; the rules above don't use it.

**Also found:** in both main and gate, **slices 0 and 1 stay wrong after the reversal** in most seeds (main: 12% of slices below −0.3, nearly all slices 0–1). It is systematic, not seed noise, and it is what limits main's re-learning. The cause isn't established (a candidate: under L180 those slices' correct outputs are on the far side of the output row). Not addressed by this phase.

## 8. Summary of the rules

| Constant | Rule | Today gives | Setting (margin) |
|---|---|---|---|
| Steps per bar S | smallest S with F(S) ≥ today's F(12) | 12 | none: calibrated to the tested brain |
| Punish calls n | smallest n with nP ≥ m_sep C_ref | 4 | m_sep = 1.1; c_ref = 0.1265 |
| Hebb h | F (e^(m_h nP) − 1) | 252 (tested 250) | m_h = 1.5 |
| Decay multiplier | 0.5 × 8/K | 0.5 | the tested per-visit value |
| Floor, cap | fixed weight scale | 100, 2,000,000 | |

**Predictions (for R.3 to fix):** at today's size the derived brain is the tested brain except h = 252, so it should perform the same. At central rows 6 and 9 the fixed constants (S = 12) leave the move answering stale input (F(12) = 0.15 and 0.006), so derived constants should beat fixed ones there, increasingly with depth.
