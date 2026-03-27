# Cross-Flow Pressure-Regulated Brain Architecture

## Overview

The brain uses a **cross-flow pressure-regulated** design with internal dynamics and multiple I/O ports. Signals flow both vertically (vision from top → motor output at bottom) and horizontally (meta-brain from right → overflow drain at left).

## Architecture

```
                  SENSORY INPUT
                       ↓
        ┌──────────────────────────────┐
        │     vision slices → top      │
        └──────────────────────────────┘
                       ↓
┌──────────┐                            ┌──────────┐
│ OVERFLOW │ ←  ┌───────────────┐   ←  │META-INPUT│
│  (left)  │    │               │       │ (right)  │
│          │ ←  │  MAIN BRAIN   │   ←  │          │
│  drain   │    │    (6×12)     │       │  from    │
│  excess  │ ←  │               │   ←  │meta-brain│
└──────────┘    └───────────────┘       └──────────┘
                       ↓
        ┌──────────────────────────────┐
        │    bottom → motor commands   │
        └──────────────────────────────┘
                  MOTOR OUTPUT
```

**Four I/O Ports:**
1. **TOP (Sensory Input)**: Vision system → brain top row, always active
2. **RIGHT (Meta-Brain Input)**: Meta-brain output → right column, throttled by pressure
3. **BOTTOM (Motor Output)**: Brain bottom row → movement commands, always active
4. **LEFT (Overflow Drain)**: Brain left column → drain excess activity, pressure-activated

## Pressure Regulation System

### Brain Pressure Calculation (q)

```q
/ Activity = sum of all trinary cell values (0, 1, or 2)
/ Max possible = rows × cols × 2 = 6 × 12 × 2 = 144
/ Pressure = activity / max_activity  (range: 0.0 to 1.0)
pressure:{[br;rows;cols] (sum sum each br) % `float$(rows*cols*2i)}
```

### Three Valves

**Valve 1: Meta-Brain Input Throttle (Right Side)**
- Pressure < 0.3: Full flow (100% of meta-brain output applied)
- Pressure 0.3–0.6: Linear throttle (100% → 0%)
- Pressure > 0.6: Cut off (0% flow)
- Prevents runaway activity accumulation

```q
/ In .brain.applyThrottledMetaInput:
flowRate: $[pressure < 0.3; 1.0; pressure < 0.6; 1.0-(pressure-0.3)%0.3; 0.0]
```

**Valve 2: Overflow Drain (Left Side)**
- Pressure < 0.5: Closed (no drain)
- Pressure 0.5–0.7: Gradual leftward bias increase (drain opening)
- Pressure > 0.7: Strong leftward pull (drain wide open)
- Implemented as pressure-adjusted leftward directional bias (directions NW/SW/W boosted)

```q
/ In .brain.leftwardPull:
leftMult: $[pressure < 0.5; 1.0; pressure < 0.7; 1.0+(pressure-0.5)%0.2; 2.0]
/ Applied to bias indices for NW(0), SW(6), W(7) directions
```

**Valve 3: Dynamic Gradient Steepness**
- Vertical bias multiplier: `1.0 + pressure × 1.0` (range 1.0 → 2.0)
- Horizontal bias multiplier: `1.0 + pressure × 0.5` (range 1.0 → 1.5)
- Higher pressure = steeper gradients = faster drain to outputs

## Directional Bias System

Two bias arrays serve different signal sources (both in `.dirs` namespace):

```q
/ Vertical bias: favours downward propagation (vision signals entering top row)
vBias: 0.5 0.5 0.5 1.0 1.0 1.5 1.8 1.5
/       NW   N   NE   W   E   SW   S  SE

/ Horizontal bias: favours leftward propagation (meta-brain signals entering right column)
hBias: 1.5 1.0 0.5 0.5 0.5 1.0 1.5 1.8
/       NW   N  NE   W   E   SW   S   W
```

The active bias for each neuron step is selected based on signal origin (tracked via `inc0`/`inc1`).

## Cross-Flow Conflict Resolution

When a neuron receives signals from BOTH top and right simultaneously, the existing 4D synapse system handles it naturally:

- `inc0`/`inc1` track up to 2 incoming directions per neuron
- For HIGH state (two active inputs), synapse weights from both incoming directions are averaged
- The 4D structure `syns[r][c][inDir][outDir]` was designed precisely for this

No additional conflict-resolution logic is needed.

## Q Implementation

The cross-flow mechanics are implemented in [q/modules/brain.q](q/modules/brain.q):

- `.brain.stepNeuron` — processes one neuron, applies context-appropriate bias, returns updated accumulator
- `.brain.propagate` — folds `stepNeuron` over all active neurons
- `.brain.applyThrottledMetaInput` — applies meta-brain output to right column with pressure throttling
- `.brain.captureOverflow` — captures activity from left column (like outputter captures bottom row)
- `.brain.pressure` — calculates current brain pressure (0.0–1.0)
- `.brain.leftwardPull` — calculates leftward drain multiplier from pressure
- `.brain.drain` — applies overflow drain using pressure-scaled leftward bias

## Bar Processing Sequence

```q
/ Inside the main Bar loop in cat_mouse.q:
pressure: .brain.pressure[prim`br; ROWS; COLS];
/ 1. Apply throttled meta-brain input to right column
prim: .brain.applyThrottledMetaInput[prim; meta`out; pressure];
/ 2. Run STEPS_PER_BAR propagation cycles
do[STEPS_PER_BAR; prim: .engine.runCycle[prim]];
/ 3. Capture overflow from left column
overflow: .brain.captureOverflow[prim`br; ROWS];
```

## Design Decisions (Resolved)

1. **Overflow data**: Logged to `simLog` for analysis (not discarded, not fed back yet)
2. **Meta-brain output mapping**: Uses variable offset/length parameters matching top/bottom I/O pattern
3. **Cross-flow conflicts**: Resolved by existing 4D synapse averaging — no extra logic needed
4. **Drain mechanism**: Gradient-based flow via pressure-adjusted leftward bias (same mechanics as output vector)
5. **Incoming direction for meta-input**: Direction 8 (West) — indicates intended leftward flow

## Status

Architecture fully implemented in q. The Fortran predecessor validated the pressure thresholds (0.3/0.6 throttle, 0.5/0.7 drain) experimentally. Meta-brain size is fixed at 3×5 — larger sizes cause energy accumulation bugs (validated in prior experiments).
