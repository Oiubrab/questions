# Bar Structure — one decision cycle, step by step

A **bar** is one decision by the cat. Everything below happens once per bar, in this order, in the main loop of [q/programs/cat_mouse.q](../q/programs/cat_mouse.q). An **epoch** is 2000 bars and is used only for reporting.

Parameter values are the current defaults in `cat_mouse.q` (since 2026-10-29: cosine reward, reward scale 0.25, hebb 250, wrapped world, angle wiring, satiety meta-brain; since 2026-11-02: the modular brain and no direct reward while the mouse is hidden; the flags that restore the originals are noted where they apply).

**The brains (since 2026-11-02, the user's design).** A **sight brain** (3×12) and a **touch brain** (3×12) feed a **central brain** (3×12, `--central-rows`), whose output moves the cat; the **reward brain** (the 3×5 meta-brain, input = satiety) feeds the central brain too. All four are the same kind of system. Between bars, sight, touch and central are kept stacked top to bottom as one 9-row "primary" state (rows 0–2 sight, 3–5 touch, 6–8 central), so every reward, history, decay and save rule below reaches all three. `--single-brain` restores the original single 6×12 primary brain. Code: `q/modules/modular.q`. Known oddities are marked ⚠, with the finding ID from [REVIVAL_PLAN.md](../plans/completed/REVIVAL_PLAN.md).

---

## 1. Bookkeeping and meta-brain input
- At an epoch boundary, store that epoch's catch and direction counts.
- **Catch rate** `rateCounter` = the number of catches in the last 200 bars (`RATE_WINDOW`).
- **Satiety** (the meta-brain's input, since 2026-10-29): `satiety` decays by `exp(−1/200)` each bar, and each catch adds 1. One MEDIUM unit enters the meta-brain's 5-cell input at the cell for its level: cell 0 (left) starving (< 0.5), 0.5–2, 2–5, 5–10, cell 4 (right) full (≥ 10). There is always exactly one unit of meta input. (Until 2026-10-29 the input was a positional code of `rateCounter`, and zero catches meant no input at all; F22.)
- **Internal input** (opt-in, W.1b): `--efference` adds a MEDIUM unit at row 1 above the output slot that moved the cat last bar; `--spontaneous P` adds a unit to each cell with probability P from a random direction. Both add, capped at HIGH, rather than overwrite.

## 2. World update and vision
- **Re-learning worlds** (G.4, only with `--relearn-schedule`): at the start of each bar the current world sets the cat's *eyes* (which input column a vision slice lights), its *legs* (which direction each output slot moves) and its *goal* (chase or flee). In a FLEE world the direct reward is negated and the catch boost and strategic lookback are off. See `q/modules/relearn.q`.
- Clear synapse-usage flags (`su`) for both brains. They record which synapses fire during *this* bar, for credit assignment.
- **Modular brain:** split the stacked state into the sight, touch and central brains for this bar (`.mod.sub`).
- **W.6** (`--world random --layout-every N`): every N bars a new random layout is drawn and the cat, mice and dog are moved clear of walls.
- Every 10 bars, the mouse takes a 2-unit random step (`--mouse-step`) and wraps around the field edges.
- **Harder goals (W.5, opt-in):** with `--evasive`, a mouse within 15 units of the cat steps away from it every bar. With `--mice N`, every mouse moves, and the one the cat sees and is rewarded on this bar is the nearest visible one (or the nearest, if none is). With `--distractor`, a dog wanders like the mouse.
- **Vision:** the angle from cat to mouse, split into 8 slices of 45°. Slice k covers angles [45k°, 45(k+1)°), measured from the +x axis turning towards +y. The active slice becomes MEDIUM in the input vector of the sight brain (the primary brain with `--single-brain`).
- **Dog** (`--distractor`): if visible, one unit enters the same input row at the dog's slice with incoming label 6 (vision uses 7), so the brain's direction-dependent synapses can tell the two apart.
- **Touch** (modular brain): the touch brain's 8 input cells are in angle order (direction 45d°). A cell is MEDIUM if a wall lies within 3 units that way (whiskers) and HIGH if the cat's last move that way was blocked (the "blocked" sense, obstacles plan §7 decision 2). Without walls, or with `--no-touch`, the touch brain gets no input.
  - The world wraps for both animals (the user's chosen geometry, the default), and vision, the direction reward, the alignment metric and the catch test all use the nearest copy of the mouse across the edges.
  - With `--walled-world` (the original world) the cat is clamped to the box and vision uses raw dx/dy, not the nearest copy (⚠ F12).
- **Obstacle worlds (only with their flags):** the mouse is *hidden* in the hidden bars of a `--blink V:B` cycle, or when a wall of `--world LAYOUT` lies on the straight line to it (unless `--see-through`). Hidden means the brain gets no vision input at all (slice logged as −1). Walls also block the mouse's random step and the cat's move: a blocked move stops 0.05 units short of the wall. Spawns that land within 2 units of a wall are re-drawn. Since 2026-11-02 there is **no direct reward while the mouse is hidden** (§10; user decision); `--hidden-reward cos` restores the reward towards the true direction, through walls. With `--policy P` a scripted baseline picks the move (one 5-unit step a bar) instead of the brain's output, and learning is off.
  - The brain has to *learn* which output direction matches which slice; nothing pre-wires them.

## 3. Meta-brain (reward brain) cross-flow into the central brain
- The **previous** bar's meta-brain output (5 values) is written into the right column of the central brain (the primary brain with `--single-brain`), rows 1–5, with incoming-direction label 8. Rows beyond the brain are dropped (the default 3-row central brain takes outputs 0–1 at rows 1–2).
- `--meta-entry top` instead adds outputs 0, 1, 3, 4 into the free top-row cells 0, 1, 10, 11; `--meta-entry none` injects nothing. Measured in the obstacles plan §6: none is ruinous with walls, top trades open-world precision for pillars; the right edge stays the default.
- It passes through a pressure throttle (see [CROSS_FLOW_ARCHITECTURE.md](CROSS_FLOW_ARCHITECTURE.md)).
- ⚠ This *overwrites* cell values, so it can create or destroy energy (F16).
- `--ablate-meta` replaces this input with zeros.

## 4. Propagation: 12 primary steps, 24 meta steps
For each of `STEPS_PER_BAR` = 12 steps:
- **Modular brain:** first a sight step and a touch step (each as below). The units that left their bottom rows this step enter the central brain's top row at the same column (`.mod.transferIn`; sight with label 7, touch with label 6), adding to what is there; a unit that finds its cell full is lost. Then the central brain steps.
- **Primary (single) or central brain step** (`.engine.runCycle`):
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
6. **Hebbian strengthening (unconditional usage reinforcement):** when learning is enabled, the synapse used gets +`.brain.HEBB` (default 250, set by `--hebb N`; originally 1000; cap 2,000,000) and is flagged in `su`. This term is deliberate and is never removed: it lets the brain retool and explore. Only its ratio to the reward reinforcement is tuned (G.R3).

`--no-learning` leaves propagation and movement active but disables Hebbian weight changes, all reward/catch reinforcement and punishment, and decay. Synapse-use flags are still recorded.

`q/tests/test_conservation.q` checks that the energy in the brain plus the energy in the outputter never changes during propagation, and that a modular step conserves energy across the sight, touch and central brains (counting units lost at a full cell).

- **Modular brain:** after propagation and the drain, the three brains are stacked again (`.mod.stack`), their usage indices offset by each brain's first row, so the rewards below apply to all three.

## 5. Pressure, overflow, drain
- **Pressure** = total brain energy ÷ (rows × cols × 2), so it ranges from 0 to 1. Modular brain: the central brain's.
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
- ⚠ The decision *whether* to fire is a hard-coded threshold, not the meta-brain's.

## 7. Meta-brain reward (every bar): be full, not starving
- Graded on where satiety enters the meta input: cell 4 (full) reinforce 2×; cell 3 reinforce 1×; cell 2 nothing; cell 1 punish 1×; cell 0 (starving) punish 2×. This is the meta-brain's only reward. Its output reaches the primary brain through cross-flow and the strategic scope and magnitude, so the routes that survive are the ones that help the cat eat.
- One "reinforce" multiplies used synapses by `1.2 / 0.95^12` ≈ **×2.22** (±10% noise, cap 2M). One "punish" multiplies by `0.8 × 0.95^12` ≈ **×0.43** (floor 25).

## 8. Decay and history
- Every 5 bars, all synapses in both brains are multiplied by a random 0.98–0.995 (floor 25).
- With `--two-timescale` (G.R5) the primary brain's weights instead relax towards a slow memory: W ← e^M + (W − e^M)·d, with d the same noise rescaled to `--tau-fast` periods (default 80, the same mean rate). Then M ← M + (ln W − M)/`--tau-slow` (default 200). M starts at the initial weights and is saved as an 11th weights-file component.
- This bar's primary `su` is written into the 100-slot circular history buffer `hist`.

## 9. Action and catch
- **Action** = the outputter slot with the highest value; **distance** = that value (1 or 2) × 5 units.
  - ⚠ Ties go to the lowest slot (F11; under the original grid wiring that was NW). `--random-tie` (G.3c) picks uniformly among tied slots.
  - An all-zero outputter means no move.
- **Direction mapping** (output slot → move), angle wiring (default): slot s moves at angle 45s° from +x towards +y, i.e. 0 E, 1 SE, 2 S, 3 SW, 4 W, 5 NW, 6 N, 7 NE, the same order as the vision slices (row/y +1 = S). `--wiring grid` restores the original 0 NW, 1 N, 2 NE, 3 W, 4 E, 5 SW, 6 S, 7 SE; `--wiring random` uses a fixed permutation per seed.
- The cat moves and wraps around the edges. With `--walled-world` it is clamped to the 0–100 box while the mouse still wraps (⚠ F12).
- **Catch** if the segment from the old to the new position passes within 2.0 of the mouse (`.vision.pathDist`). On a catch:
  - **Catch boost:** ×1.5 on synapses used in the current bar and the 19 before it.
  - Satiety +1 (it enters the meta input from the next bar).
  - The mouse respawns 30–45 units from the cat (wrapped; clamped to the box with `--walled-world`).
- **Dog contact** (`--distractor`): if the cat's path passes within 2.0 of the dog, the routes used in the current bar and the 19 before it are scaled by 1/1.5 (the catch boost reversed), and the dog respawns 30–45 units away. If the dog is visible and within 25 units, a move with cosine > 0.5 towards it is punished once.

## 10. Direct (tactical) reward — skipped with `--meta-only`, and while the mouse is hidden
- `dotP` = (unit move vector) · (unit vector from the cat's old position to the mouse): the cosine of the angle between the move and the mouse (G.3a, the default). `adaptThresh` (0.6–0.85) is a real cone of about ±32°–53°, and a rewarded move fires `round(1+3·dotP)` = 1–4 reinforce calls.
  - With `--raw-dot-reward` (the original) the move vector is not normalised (length 5–14), so `dotP` is not a cosine, the "cone" is about 170° wide and one good move can fire up to ~43 calls (⚠ F10).
- **`--pg-update`** (G.R2, opt-in) replaces the whole threshold/count rule above with a policy-gradient update. Each move's reward is its cosine R to the mouse (or, with `--rpe`, R minus a running average with rate `--rpe-rate`, default 0.01). Every used (cell, inDir) then moves all 8 of its routes in log space by `η·δ·(used − p)`, with `--pg-eta` η (default 0.3): chosen routes by +(1−p), siblings by −p. No random draws. The catch boost and strategic lookback are unchanged.
- **`--reward-scale L`** (G.3b, default 0.25; 1 is the original): every primary-brain reward is a multiplication, so a step in log space: reinforce +0.80, punish −0.84, catch boost +0.41 per history bar, strategic +0.095·m per lookback bar. `L` multiplies all of those log-steps (the same random draws are used, so L = 1 is the original update exactly). Meta-brain rewards are not scaled.
- The move counts as *towards* if `dotP > 0.01`, *away* if `< −0.01`, and *perpendicular* otherwise. These counts are the reported "towards %".
- **Adaptive threshold** `adaptThresh` (0.6–0.85): every 50 bars, it rises by 0.03 if distance improved by 10%. After 100 bars without progress, it falls by 0.02.
- **The reward or punishment:**
  - `dotP > adaptThresh`: reinforce `round(2 × (0.5 + 1.5·dotP))` times, plus a momentum bonus of up to 3 more for repeating the last rewarded direction. With the cosine `dotP` (default) that is 1–4 calls plus momentum, each a ×2.22 step scaled to ×2.22^0.25 ≈ ×1.22 by the default reward scale; with `--raw-dot-reward --reward-scale 1` one good move could apply dozens of full ×2.22 steps, so weights jumped to the cap.
  - `dotP < −0.01`: punish `round(1 + 2·epochProgress)` times.
  - Otherwise, after the first epoch: punish once.

## 11. Logging
- Append one row to the `simLog` table: `bar mouseX mouseY catX catY visionSlice brainEnergy outEnergy outAction moveDist alignment catches pressure overflow`. `alignment` is the cosine of actual movement and the pre-move direction to the mouse, null when undefined or when there was no movement. `catches` is cumulative; positions are rounded to integers.
- Every 1000 bars, print a snapshot.

---

## Why 12 steps per bar
Energy moves roughly one row per step, because a neuron that only becomes active mid-step isn't processed until the next step. Twelve steps give a vision signal time to cross all 6 rows and reach the outputter within the same bar, so the move is caused by *this* bar's vision. That's what makes per-bar credit assignment through `su` meaningful.

## History buffer indexing
`hist` has 100 slots, and `histIdx` is the **next** slot to write. Bar `b`, seen from the current bar `barN` before this bar is written, lives in slot `(histIdx − (barN − b)) mod 100`. This is `.syn.histSlot`, and relies on q's floored `mod`. `.syn.lookbackBars[bar; scope; 100]` lists `bar−1, bar−2, …` while they're still in the buffer. Both are tested in `q/tests/test_geometry.q`.
