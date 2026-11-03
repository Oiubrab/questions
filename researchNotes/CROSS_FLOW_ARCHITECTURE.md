# Cross-Flow and Pressure Architecture

**Since 2026-11-02 the default brain is modular** (the user's design; obstacles plan §7 decision 4):

```
     vision (8 slices)            whiskers + blocked sense (8 directions)
            ↓ top cols 2–9                    ↓ top cols 2–9
   ┌──────────────────┐            ┌──────────────────┐
   │ SIGHT BRAIN 3×12 │            │ TOUCH BRAIN 3×12 │
   └──────────────────┘            └──────────────────┘
            ↓ bottom cols 2–9 (label 7)       ↓ bottom cols 2–9 (label 6)
            └──────────────┬──────────────────┘   same column = same direction,
                           ↓ top cols 2–9           told apart by incoming label
                 ┌────────────────────┐
                 │ CENTRAL BRAIN 3×12 │  ←  reward (meta) brain output, right column
                 └────────────────────┘
                           ↓ bottom cols 2–9
                outputter: 8 move directions
```

Each step, sight and touch propagate first; what leaves their bottom rows enters the central brain's top row the same step (`.mod.transferIn`, additive, conserving except for units that find a full cell). The rest of this note describes the edges of the original single primary brain (`--single-brain`), which the central brain inherits for its right and left edges.

The primary brain (6×12) has four edges. Two carry signals *in* and two carry signals *out*. A "pressure" mechanism was designed to stop the grid filling up with energy.

**Summary of current state:** the top/bottom and right-edge flows are active. The left edge is only logged. The pressure controls (the meta-input throttle and the drain) are implemented but **never engage in practice**, because measured pressure stays far below their thresholds.

```
                       vision (8 slices)
                               ↓  top row, cols 2–9
                 ┌───────────────────────────┐
  overflow    ←  │                           │  ←  meta-brain output
  (logged only)  │   PRIMARY BRAIN  6 × 12   │     (previous bar)
  left col,      │                           │     right col,
  rows 1–4       │                           │     rows 1–5
                 └───────────────────────────┘
                               ↓  bottom row, cols 2–9
                  outputter: 8 move directions
```

## The four edges

| Edge | Direction | Code | What actually happens |
|---|---|---|---|
| Top | in | `.brain.copyInputToTopRow` | On the first step of each bar, the active vision slice is written as MEDIUM into the top row (cols `IN_OFF`=2 to 9), with incoming-direction label 7. |
| Bottom | out | inside `.brain.stepNeuron` | A bottom-row neuron can move an energy unit into the outputter slot below it (cols `OUT_OFF`=2 to 9). That energy leaves the brain. The slot with the highest value picks the move. |
| Right | in | `.brain.applyThrottledMetaInput` | The previous bar's meta-brain output (5 values, 0–2) is written into the right column, rows 1–5, with incoming-direction label 8, subject to valve 1. In the modular brain this is the central brain's right column (3 rows: outputs 0–1 enter rows 1–2). `--meta-entry top` adds outputs 0, 1, 3, 4 into top-row cells 0, 1, 10, 11 instead; `--meta-entry none` injects nothing (obstacles plan §6). |
| Left | out | `.brain.captureOverflow` | Rows 1–4 of the left column are read and summed into `simLog.overflow`. **Nothing is removed or fed back;** this is a logging hook only. |

Notes:
- The top and right inputs **overwrite** cells rather than adding to them. So they can create or destroy energy (finding F16 in [REVIVAL_PLAN.md](../plans/completed/REVIVAL_PLAN.md)). Conservation is guaranteed only inside propagation.
- The incoming-direction labels (7 = S, 8 = SE in `.dirs` numbering) only choose which row of a neuron's 8×8 synapse matrix is used next. They don't mean "came from above" or "came from the right" geometrically. The system is consistent, so this doesn't break anything.
- Propagation always uses the downward bias `.dirs.vBias`. The horizontal bias `.dirs.hBias` is defined but unused.

## Pressure

```q
.brain.pressure: total energy / (rows × cols × 2)      / 0 = empty, 1 = every neuron HIGH
```

Pressure is computed after each bar's propagation and is used by:

| Mechanism | Status | Rule |
|---|---|---|
| **Valve 1** — meta-input throttle | implemented | flow probability 1 if p < 0.3; falls linearly to 0 between 0.3 and 0.6; 0 above 0.6 |
| **Drain** | implemented | if p > 0.1, each active neuron independently loses 1 unit with probability `min(0.15, 0.3p)` (uniform, not directional) |

**Measured in practice** (16 brains × 20,000 bars, October 2026):
- Mean pressure per run: 0.0002–0.009.
- The peak in a full training run was about 0.056.
- So **valve 1 is always fully open and the drain has never fired.** The brain ends almost every bar nearly empty, so energy does not build up between bars.

Two further valves in the original design (a leftward pull and a pressure-dependent bias steepening) were never implemented. The user removed them from the design on 2026-10-29 (revival plan §7 decision 2), with the dead code that went with them. Energy control now belongs to the substrate research in the 3D plan (thermostat, task A.3).

## Meta-brain size

The meta-brain is 3×5. Fortran-era notes said larger meta-brains caused "energy accumulation". In the q version, `q/tests/test_conservation.q` shows propagation conserves energy at 3×5, 6×12 and 8×16. So if accumulation does occur, it would come from the overwriting inputs (F16), not from propagation. This hasn't been re-tested. Run the tests and check `simLog.brainEnergy` before resizing.
