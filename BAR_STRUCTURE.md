# Bar-Structured Brain Processing

## Temporal Organization

The simulation uses **"Bars"** (like musical bars) as the fundamental time unit.

### One Bar = One Real-World Decision Cycle

Each Bar consists of:

1. **World State Update** (once)
   - Mouse moves by random walk (step size units in random direction, toroidal wrap)
   - Cat position checked for catch (path distance < 2.0)
   - Vision angle calculated → active input slice determined

2. **Input Application** (once at Bar start)
   - Active vision slice copied to brain top row (columns `inOff` to `inOff+inLen-1`)
   - Implemented via `.brain.copyInputToTopRow`

3. **Brain Processing** (`STEPS_PER_BAR` = 12 steps per Bar)
   - Brain propagates state through 12 internal steps via `.engine.runCycle`
   - Each step: active neurons fold through `(br;syns;su;inc0;inc1;out)` accumulator
   - Output accumulates throughout these steps

4. **Action Execution** (once at Bar end)
   - Output vector read from bottom row (columns `outOff` to `outOff+outLen-1`)
   - Cat moves based on dominant output direction
   - Next Bar begins

## Why This Matters

### Without Bar structure (1 brain step per world step):
- With a 6-row brain, a signal needs 6 real-world steps to propagate top → bottom
- Mouse moves significantly during that delay
- Output arrives too late to be useful for causal credit assignment

### With Bar structure (12 brain steps per Bar):
- Signal has time to propagate through all 6 rows within a single real-world step
- Cat acts based on fully-propagated vision signal
- **Causal credit assignment becomes meaningful**

## Reinforcement Learning Implications

The Bar structure enables clean credit assignment:

1. **Clear causality**:
   - Bar N: Vision slice → 12 brain steps → movement direction
   - Bar N+1: Measure result (did distance decrease? was catch made?)

2. **Synapse usage tracking**:
   - `su` (synapse usage) boolean 4D array records which synapses fired during Bar N
   - Synapse history buffer (5D, 100 bars deep) supports temporal credit assignment

3. **Temporal credit assignment**:
   - History index: `i: \`int$(hi - (barN-b) + HISTORY_SIZE) mod HISTORY_SIZE`
   - Meta-brain output `temporal_scope` selects lookback window (20-120 bars)
   - Reinforcement applied across entire successful behavioral sequence

## Parameters

```q
STEPS_PER_BAR: 12i   / Brain steps per Bar (allows full 6-row propagation + margin)
MAX_BARS: 20000i     / Total simulation Bars
EPOCH_SIZE: 2000i    / Bars per analysis epoch
HISTORY_SIZE: 100i   / Synapse history buffer depth (bars)
```

## Propagation as Functional Fold

The 12 steps per Bar are implemented as a loop calling `.engine.runCycle`:

```q
/ Run STEPS_PER_BAR propagation cycles per Bar
/ Each runCycle threads (br;syns;su;inc0;inc1;out) through all active neurons
do[STEPS_PER_BAR; prim: .engine.runCycle[prim]]
```

Inside `runCycle`, `.brain.propagate` uses fold to process active neurons:
```q
/ f/[accumulator; activeNeuronList] threads mutable state through each neuron
/ This ensures energy conservation — no OpenMP needed
acc: f/[init; activeList]
```

The fold pattern is essential: `each` discards return values and would break energy conservation.

## Q/kdb-x Simulation Log

Each Bar appends a row to the live `simLog` kdb-x table:

```q
/ Query during or after simulation:
select avg catchRate by epochN from simLog
select bar, catchRate, metaScope, metaMag from simLog where catchRate > 10
```

This replaces the previous CSV output approach and enables real-time analysis.
