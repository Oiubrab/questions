# Plan: a metronome (a sparse, clock-like internal input)

**Status (2026-11-03):** written, not started. Waiting on the user's decisions in §5. Replaces `--spontaneous`, which was removed on 2026-11-03.

**Why it exists.** Design principle 8 (revival plan §2): "a real organic brain is always receiving input … not just from the outside world, but from itself internally." The first attempt, `--spontaneous P` (each cell gained a unit with probability P, from a random direction), failed badly at P = 0.05: −62% to −93% catches in every world, with vision drowned out ([OBSTACLES_RESULTS.md](../../researchNotes/OBSTACLES_RESULTS.md) §2, row 7). On seeing that, the user (2026-11-03):

> "there should be some input to that, but it shouldn't be random. Right, like it should be kind of like a clock. … it should be very sparse. It can't come close to overpowering input. … write up a new plan to reintroduce some more metronomic system that … injects data much less frequently, and also injects it at a particular spot too, not randomly throughout the network."

**How to use this document:** the same rules as the other plans. Tasks are checkboxes; tick only after the acceptance check passes, with the date. Every session appends to the Progress Log (§8). Decisions in §5 belong to the user. Claims need ≥ 16 paired seeds.

---

## 0. Goal

An internal input that is:
- **a clock:** it ticks on a fixed schedule, with no random draws, so a run is reproducible and the random-number stream is untouched;
- **sparse:** in total, a small fraction of what the senses deliver, never close to overpowering them;
- **at one place:** a fixed cell, not spread over the network.

It earns its place by showing **no harm** to chasing a visible mouse, and **some benefit** when the senses are quiet: the cat keeps moving sensibly when blind, instead of standing still or drifting one way. If it passes, it becomes **part of the default brain** (it implements a user-stated principle), not a flag.

## 1. What the earlier results say about the design

- **Scale of the senses.** Vision injects 1 unit on each bar the mouse is visible. Touch injects up to 9 units near walls (one per whisker, and a second for the direction of a blocked move). The satiety input gives the reward brain 1 unit every bar.
- **Why random activity failed.** At P = 0.05 over the 108 cells of the sight, touch and central brains, it added about 5 units a bar, against vision's 1. Noise outnumbered the signal about five to one, and visible alignment fell to about 0.02.
- **A unit doesn't last.** In a trained brain an injected unit stays about 8 steps on average (a bar has 12), and under 10% is still inside after one bar (OBSTACLES_RESULTS §4.4). Each tick is therefore a short pulse, not a lasting background.
- **Energy at a fixed spot can push the cat one way.** The reward brain's energy enters a fixed column, and while the mouse is hidden it produced a fixed drift (finding F22; hidden-bar alignment −0.15 to −0.38). The clock must be checked for the same failure.
- **The candidate spots were tried once before, with much more energy.** `--meta-entry top` fed the reward brain's outputs into the free top-row cells beside sight and touch. It doubled pillars catches and cost visible alignment (obstacles plan §6). That was several units a bar; the clock is designed to be one unit every tens of bars.
- **The satiety input is already metronomic:** one unit, every bar, at a cell set by how full the cat is. The clock extends the same idea to the primary side.

## 2. Design

**A tick:** one unit (MEDIUM), injected every `PERIOD` bars, at step `STEP` of that bar, into one cell (brain, row, column), arriving with a fixed incoming-direction label. It adds to what is there, capped at HIGH, like the other inputs (`.brain.injectAdd`).

**Budget rule:** the clock's long-run input stays **at or below 10% of vision's** in the open world, so PERIOD ≥ 10 bars. Candidates are 10, 25, 50 and 100 bars (one unit per 10–100 bars, against vision's 1 per bar).

**Where:** §5, decision 1. The candidates:

| Spot | What the tick does | For | Against |
|---|---|---|---|
| (a) A free top-row cell of the **central brain** (columns 0, 1, 10 or 11, beside the sight and touch inputs) | Passes through the central brain's learned routes to an output | Learning can shape what a tick does; the cheapest to build | Close to the motor output: the likeliest to cause a fixed drift |
| (b) A free top-row cell of the **sight brain** | Passes through sight's routes, then the central brain's | Treated by the brain like a faint visual event | Could be confused with vision |
| (c) A cell of the **reward brain** (its middle row; its top row is the satiety scale) | Changes the reward brain's outputs: strategy (scope, magnitude) and its drive into the central brain | Far from the motor output; least risk to the chase | Indirect; the reward brain is small (3×5) and sensitive to resizing (CLAUDE.md) |
| (d) A **clock module** of its own (a small brain that receives the tick and feeds the central brain, like touch) | A pacemaker organ; its routes learn what to do with the tick | Fits the modular design; the clock's effect is separable and lesionable | The most code; one more brain in the stacked state |

**Settings, not literals** (the no-hard-coded-values rule): `--clock-period N` (0 = off), `--clock-cell BRAIN:ROW:COL`, `--clock-step S`, `--clock-label L`. These are measurement flags; the winning values become the default (§0).

**Two variants worth testing** (§5, decisions 2 and 3):
- **Always tick,** or tick **only when the senses have been quiet** for K bars (no vision or touch input). The second is still a clock, but one that a busy brain doesn't hear.
- **One fixed cell,** or a **fixed alternating pair** of mirror cells (for example columns 0 and 11). The pair is still deterministic, but the two ticks pull in opposite directions, so they can't add up to a one-way drift.

**What it is not:** a reward, a hint about the mouse, or a random source. A tick carries no information about the world.

## 3. Tasks

- [ ] **M0 Decisions** (§5).
- [ ] **M1 Build.** The flags above, off by default (period 0); the tick in the bar loop at the chosen step; summary columns (period, cell, step, label, ticks delivered).
  *Accept:*
  - with the clock off, runs are byte-identical to today (golden digest, plus the regression comparison used in the 2026-11-03 refactor);
  - ticks delivered = floor(bars ÷ period);
  - energy conservation holds with the ticks counted as input (`test_conservation.q`);
  - the clock makes no random draws: for the same seed, the run is identical to the clock-off run up to the first tick.
- [ ] **M2 Screen.**
  - **Arms:** periods {10, 25, 50, 100} × the spots chosen in decision 1, in the open world, blink 20:20 and pillars. 16 seeds × 10k bars.
  - **Measures:**
    - catches;
    - alignment while the mouse is visible;
    - the share of hidden bars on which the cat moves, and the alignment of those moves;
    - **drift:** how evenly the cat's moves on hidden bars spread over the 8 directions (the entropy of the move directions; a fixed drift shows up as low entropy);
    - the clock's realised share of all input units;
    - carry-over (`structure.q`).

  Roughly 36 arms, about 40 minutes on 16 cores.
- [ ] **M3 Confirm.** The best one or two settings against no clock, paired, in all 9 obstacle worlds.
- [ ] **M4 Adopt.** If the acceptance below passes, the clock becomes the default. Update the golden digest, the revival plan's §2 point 8 (note how it is implemented), BAR_STRUCTURE.md, the README flag table and CLAUDE.md.
  *Acceptance for adoption:*
  - no significant harm in the open world (catches and visible alignment);
  - no new drift (hidden-move entropy not lower than without the clock);
  - and at least one of: more hidden bars with a move, or better catches, in blink or pillars.
- [ ] **M5 (later, with the memory work).** Once a substrate can hold energy across bars (3D plan Stage A), re-test: a regular tick is the simplest thing a brain could learn to keep time with, so the structure probes should look for loops locked to the clock period.

## 4. Risks

- **Too sparse to matter.** A tick drains in about 8 steps, so one tick every 25 bars may change nothing. The screen includes period 10 to find the edge.
- **One-way drift** from a fixed spot (F22 again). The drift measure in M2 and the mirror-pair variant are the guards.
- **It doesn't give memory.** A clock is not a memory; the hidden-mouse problem still needs a substrate that holds state (OBSTACLES_RESULTS §7).
- **Interaction with the reward brain's drive,** which also enters the central brain. Measure both together, not the clock alone.

## 5. Decisions for the user

1. **Where the tick enters.** (a) A free cell of the central brain's top row; (b) a free cell of the sight brain's top row; (c) the reward brain; (d) a clock module of its own. **Recommendation:** screen (a) and (d). (a) is the cheapest, and (d) fits the modular design and keeps the clock's effect separable; (c) is the safe fallback if both drift.
2. **Always, or only when the senses are quiet?** **Recommendation:** "always" as the main line, since that is what a clock is; "only when quiet for K bars" as one variant arm in M2.
3. **One spot or a mirror pair?** **Recommendation:** one spot first; the pair only if M2 measures a drift.
4. **The budget cap.** At most 10% of vision's input (PERIOD ≥ 10). **Recommendation:** as stated; you may want it even lower.
5. **Adoption.** If M3 passes, the clock becomes the default brain (your principle 8), measured against today's behaviour. **Recommendation:** yes, as with every user-stated principle.

## 6. Results

None yet.

## 7. What was removed (for reference)

`--spontaneous P` (removed 2026-11-03): every bar, each cell of every primary brain gained a unit with probability P from a random direction. Measured only at P = 0.05 (obstacles close-out, 16 seeds; arms `results/o_{blink20,pillars,rooms}_spont_20261103`): −93% blink 20:20, −68% pillars, −62% rooms, visible alignment about 0.02. Its code is in git history (commit `21d0324` and earlier).

## 8. Progress Log

### 2026-11-03 — plan written
- The user removed `--spontaneous` after the obstacles results and asked for a clock-like, sparse, fixed-spot internal input instead.
- Removed: the flag, its helper and bar-loop call, its summary column, its tests (now a test that the old flag is rejected), the close-out arm and the docs. Golden digest unchanged.
- Proposed: one unit every 10–100 bars at one cell, deterministic, capped at 10% of vision's input; screen spots and periods for harm, drift and benefit when blind; adopt into the default if it passes.
