# Bar Structure — one decision cycle, step by step

A **bar** is one decision by the cat. Everything below happens once per bar, in this order, in the main loop of [q/programs/cat_mouse.q](q/programs/cat_mouse.q). An **epoch** is 2000 bars and is used only for reporting.

Parameter values are the current defaults in `cat_mouse.q`. Known oddities are marked ⚠, with the finding ID from [REVIVAL_PLAN.md](REVIVAL_PLAN.md).

---

## 1. Bookkeeping and meta-brain input
- At an epoch boundary, store that epoch's catch and direction counts.
- **Catch rate** `rateCounter` = the number of catches in the last 200 bars (`RATE_WINDOW`).
- Encode it into the meta-brain's 5-cell input:
  - rate 1–5 → MEDIUM at position `5−rate`
  - rate 6–10 → HIGH at position `10−rate`
  - rate >10 → HIGH at position 0
  - rate 0 → no input

## 2. World update and vision
- Clear synapse-usage flags (`su`) for both brains. They record which synapses fire during *this* bar, for credit assignment.
- Every 10 bars, the mouse takes a 2-unit random step and wraps around the field edges.
- **Vision:** the angle from cat to mouse, split into 8 slices of 45°. Slice k covers angles [45k°, 45(k+1)°), measured from the +x axis turning towards +y. The active slice becomes MEDIUM in the primary brain's input vector.
  - ⚠ Vision uses raw dx/dy, not toroidal (wrap-around) distance (F12).
  - The brain has to *learn* which output direction matches which slice; nothing pre-wires them.

## 3. Meta-brain cross-flow into the primary brain
- The **previous** bar's meta-brain output (5 values) is written into the primary brain's right column, rows 1–5, with incoming-direction label 8.
- It passes through a pressure throttle (see [CROSS_FLOW_ARCHITECTURE.md](CROSS_FLOW_ARCHITECTURE.md)).
- ⚠ This *overwrites* cell values, so it can create or destroy energy (F16).
- `--ablate-meta` replaces this input with zeros.

## 4. Propagation: 12 primary steps, 24 meta steps
For each of `STEPS_PER_BAR` = 12 steps:
- **Primary brain step** (`.engine.runCycle`):
  1. On the first step only, copy the input into the top row (cols 2–9) with incoming-direction label 7.
  2. Clear the outputter.
  3. Run `.brain.propagate` (described below).
- **Meta-brain:** two steps (same mechanics; input in the top row, cols 0–4; output from the bottom row, cols 0–4).
- Each output slot keeps its maximum value across the steps.

**`.brain.propagate`** folds over the neurons that are active at the start of the step, in row-major order. For each active neuron:
1. Take the 8 outgoing weights for its incoming direction, `syns[r][c][inDir]`. A HIGH neuron with two incoming directions averages the two rows.
2. Multiply by the fixed downward bias `vBias` = `0.5 0.5 0.5 1.0 1.0 1.5 1.8 1.5` (NW N NE W E SW S SE).
3. Zero the weights of targets that are off-grid or already HIGH. A bottom-row neuron can also move energy into the outputter.
4. Pick a direction at random, in proportion to the weights. Move one energy unit: the target goes +1, and the source goes −1 only if the move succeeded.
5. Set the target's incoming direction, and clear the source's directions if it emptied.
6. **Hebbian strengthening:** the synapse used gets +1000 (cap 2,000,000) and is flagged in `su`.

`q/tests/test_conservation.q` checks that the energy in the brain plus the energy in the outputter never changes during propagation.

## 5. Pressure, overflow, drain
- **Pressure** = total brain energy ÷ (rows × cols × 2), so it ranges from 0 to 1.
- **Overflow:** the left column, rows 1–4, is read and *logged only*. No energy is removed.
- **Drain:** if pressure > 0.1, each active neuron independently loses one unit with probability `min(0.15, 0.3×pressure)`.
- ⚠ In practice, pressure stays below about 0.06, so **the drain never fires**. See CROSS_FLOW_ARCHITECTURE.md.

## 6. Strategic (meta-controlled) reinforcement
- **Reading the meta-brain's output:**
  - `temporalScope = clamp(out[0]×40, 20, 120)`, so 20, 40 or 80 bars (120 is unreachable).
  - `magnitude = max(1, out[1])`, so 1 or 2.
  - Outputs 2–4 are used only through cross-flow.
  - `--ablate-meta` forces scope = 20 and magnitude = 1.
- **When it fires:** if `rateCounter ≥ 3`, or `≥ 1` with direct rewards off:
  - For each of the last `temporalScope` bars that are still in the 100-bar history buffer, multiply every primary synapse used in that bar by `1.1^magnitude`, capped at 2M. A synapse used in k of those bars gets multiplied k times.
  - `--no-meta-rewards` skips this.
  - Then reinforce the meta-brain's own used synapses `rateCounter` times.
- ⚠ The decision *whether* to fire is a hard-coded threshold, not the meta-brain's. The meta-brain is rewarded according to the catch rate (its input), not according to what it output (F8).

## 7. Meta-brain reward (every bar)
- `rateCounter ≥ 3`: reinforce 2×.
- `≥ 1`: reinforce 1×.
- Otherwise: punish, but only with direct rewards on.
- One "reinforce" multiplies used synapses by `1.2 / 0.95^12` ≈ **×2.22** (±10% noise, cap 2M). One "punish" multiplies by `0.8 × 0.95^12` ≈ **×0.43** (floor 25).

## 8. Decay and history
- Every 5 bars, all synapses in both brains are multiplied by a random 0.98–0.995 (floor 25).
- This bar's primary `su` is written into the 100-slot circular history buffer `hist`.

## 9. Action and catch
- **Action** = the outputter slot with the highest value; **distance** = that value (1 or 2) × 5 units.
  - ⚠ Ties go to the lowest index, which is direction NW (F11).
  - An all-zero outputter means no move.
- **Direction mapping** (output slot → move): 0 NW, 1 N, 2 NE, 3 W, 4 E, 5 SW, 6 S, 7 SE, where row/y −1 = N and col/x +1 = E.
- The cat moves and is clamped to the 0–100 box. ⚠ The mouse wraps around the edges but the cat doesn't (F12).
- **Catch** if the segment from the old to the new position passes within 2.0 of the mouse (`.vision.pathDist`). On a catch:
  - **Catch boost:** ×1.5 on synapses used in the current bar and the 19 before it.
  - If `rateCounter ≥ 5`, 3 extra meta-brain reinforcements.
  - The mouse respawns 30–45 units from the cat (clamped to the box).

## 10. Direct (tactical) reward — skipped with `--meta-only`
- `dotP` = (cat's move vector) · (unit vector from the cat's old position to the mouse).
  - ⚠ The move vector is not normalised (length 5–14), so `dotP` is not a cosine. The "cone" below is therefore about 170° wide (F10).
- The move counts as *towards* if `dotP > 0.01`, *away* if `< −0.01`, and *perpendicular* otherwise. These counts are the reported "towards %".
- **Adaptive threshold** `adaptThresh` (0.6–0.85): every 50 bars, it rises by 0.03 if distance improved by 10%. After 100 bars without progress, it falls by 0.02.
- **The reward or punishment:**
  - `dotP > adaptThresh`: reinforce `round(2 × (0.5 + 1.5·dotP))` times, plus a momentum bonus of up to 3 more for repeating the last rewarded direction. One good move can apply dozens of ×2.22 steps, so weights jump to the cap.
  - `dotP < −0.01`: punish `round(1 + 2·epochProgress)` times.
  - Otherwise, after the first epoch: punish once.

## 11. Logging
- Append one row to the `simLog` table: `bar mouseX mouseY catX catY visionSlice brainEnergy outEnergy outAction moveDist catches pressure overflow`. `catches` is cumulative; positions are rounded to integers.
- Every 1000 bars, print a snapshot.

---

## Why 12 steps per bar
Energy moves roughly one row per step, because a neuron that only becomes active mid-step isn't processed until the next step. Twelve steps give a vision signal time to cross all 6 rows and reach the outputter within the same bar, so the move is caused by *this* bar's vision. That's what makes per-bar credit assignment through `su` meaningful.

## History buffer indexing
`hist` has 100 slots, and `histIdx` is the **next** slot to write. Bar `b`, seen from the current bar `barN` before this bar is written, lives in slot `(histIdx − (barN − b)) mod 100`. This is `.syn.histSlot`, and relies on q's floored `mod`. `.syn.lookbackBars[bar; scope; 100]` lists `bar−1, bar−2, …` while they're still in the buffer. Both are tested in `q/tests/test_geometry.q`.
