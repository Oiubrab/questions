# Cross-Flow and Pressure Architecture

The primary brain (6×12) has four edges. Two carry signals *in* and two carry signals *out*. A "pressure" mechanism was designed to stop the grid filling up with energy.

**Summary of current state:** the top/bottom and right-edge flows are active. The left edge is only logged. The pressure valves are partly implemented but **never engage in practice**, because measured pressure stays far below their thresholds.

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
| Right | in | `.brain.applyThrottledMetaInput` | The previous bar's meta-brain output (5 values, 0–2) is written into the right column, rows 1–5, with incoming-direction label 8, subject to valve 1. |
| Left | out | `.brain.captureOverflow` | Rows 1–4 of the left column are read and summed into `simLog.overflow`. **Nothing is removed or fed back;** this is a logging hook only. |

Notes:
- The top and right inputs **overwrite** cells rather than adding to them. So they can create or destroy energy (finding F16 in [REVIVAL_PLAN.md](REVIVAL_PLAN.md)). Conservation is guaranteed only inside propagation.
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
| **Valve 2** — leftward pull | **not wired in** | `.brain.leftwardPull` exists but is never called |
| **Valve 3** — pressure-dependent bias steepening | **not implemented** | described in the original design only |

**Measured in practice** (16 brains × 20,000 bars, October 2026):
- Mean pressure per run: 0.0002–0.009.
- The peak in a full training run was about 0.056.
- So **valve 1 is always fully open and the drain has never fired.** The brain ends almost every bar nearly empty, so energy does not build up between bars.

Whether to wire in valve 2, implement valve 3, or remove them from the design is an open decision (REVIVAL_PLAN.md §7).

## Meta-brain size

The meta-brain is 3×5. Fortran-era notes said larger meta-brains caused "energy accumulation". In the q version, `q/tests/test_conservation.q` shows propagation conserves energy at 3×5, 6×12 and 8×16. So if accumulation does occur, it would come from the overwriting inputs (F16), not from propagation. This hasn't been re-tested. Run the tests and check `simLog.brainEnergy` before resizing.
