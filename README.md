# ForWhoseAdvantage

**A q/kdb-x neural simulation** demonstrating emergent learning through a **dual-brain architecture** with 4D directional synaptic routing and hierarchical meta-learning. Watch an artificial cat learn to hunt a mouse using 8-slice angular vision — achieving **94%+ directional accuracy** with a meta-brain that maintains and enhances learned behaviour even without direct rewards.

## What Makes This Special

ForWhoseAdvantage implements a unique dual-brain system:

**Primary Brain**: 4D directional routing where each neuron maintains 64 connection strengths (8 incoming × 8 outgoing directions), enabling context-dependent learning that mirrors biological information processing.

**Meta-Brain**: A smaller control system (3×5) that learns to trigger strategic reinforcement based on performance metrics, demonstrating **acquisition → maintenance transitions** where learned behaviour persists and improves after removing direct rewards.

**Key Capabilities:**
- Hierarchical meta-learning — meta-brain maintains and enhances behaviour without direct rewards (130% retention)
- 4D directional routing with context-dependent pathway selection
- Energy-conserving trinary neuron model (propagation as functional fold)
- Native kdb-x simulation log — query the running experiment in real time
- Brain state persistence — warm-start with saved weights
- Windowed catch-rate metrics with rolling 200-bar window

## Performance Highlights

**Latest Results** (reaction-dissociation branch):
- **Directional Learning**: 94.1% accuracy moving toward target in meta-only mode
- **Hunt Success**: 1,326 average catches/trial with meta-brain only (130% retention)
- **Training Performance**: 1,011 average catches with direct + meta rewards
- **Meta-Brain Breakthrough**: Performance *increases* when direct rewards are removed (+15%)

## Quick Start

**Prerequisites**: [kdb-x](https://kx.com) (kdb-x CLI, `q`)

```bash
# Run a 20,000-bar simulation
q q/programs/cat_mouse.q --seed 42 --bars 20000

# Meta-only mode with pre-trained weights
q q/programs/cat_mouse.q --load-weights results/path/to/weights.q --meta-only

# Interactive exploration
q q/load.q
```

## Project Structure

```
q/
├── load.q                  # Master loader — loads all modules in dependency order
├── modules/
│   ├── trinary.q           # .trinary — LOW/MEDIUM/HIGH neuron states
│   ├── directions.q        # .dirs — 8 direction deltas, opposites, biases
│   ├── synapses.q          # .syn — 4D synapse init, decay, reinforce, punish
│   ├── brain.q             # .brain — propagation fold, pressure, cross-flow
│   ├── brain_engine.q      # .engine — initSystem, runCycle, runCycleSimple
│   └── vision.q            # .vision — atan2 vision, toroidal field, mouse movement
├── programs/
│   └── cat_mouse.q         # Full simulation: main loop, reward logic, kdb-x log
└── tests/
    ├── test_trinary.q
    └── test_conservation.q
results/                    # Experimental output (gitignored)
```

See [PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md) for a detailed layout.

## Running Tests

```bash
q q/tests/test_trinary.q
q q/tests/test_conservation.q
```

Energy conservation is a hard invariant — `test_conservation.q` must pass after any brain changes.

## System Architecture

### Neural Simulation Engine (q/kdb-x)

- **`trinary.q`** — Trinary state neurons (LOW=0i, MEDIUM=1i, HIGH=2i) with shift operations
- **`brain.q`** — Primary brain (6×12) and meta-brain (3×5) with 4D synaptic routing; propagation via functional fold
- **`brain_engine.q`** — Unified interface for dual-brain system management
- **`synapses.q`** — 4D synapse arrays with adaptive decay/reinforcement/punishment
- **`vision.q`** — 8-slice angular vision system (45° per slice) on a toroidal field
- **`directions.q`** — Direction constants, biases, and opposites

### Simulation Log (kdb-x table)

`simLog` is a live kdb-x table appended each bar. Query it in real time:
```q
select avg catches from simLog where bar > 18000
select epochN, catchRate, metaScope from simLog
```

This is a significant improvement over the previous CSV-based approach.

## Understanding the Dual-Brain Learning Process

### The Dual-Brain Meta-Learning Loop

**1. Primary Brain Vision System**
- 8 angular slices detect mouse position (45° coverage each)
- Active slice triggers corresponding primary brain input (top row, cols 3-10)

**2. Primary Brain Neural Processing**
- 6×12 brain grid with 4D directional routing: `syns[r][c][inDir][outDir]`
- 12 propagation steps per Bar using `f/[acc; activeList]` fold
- Energy conservation maintained: each neuron only changes state by ±1 per step

**3. Meta-Brain Performance Monitoring**
- 3×5 meta-brain monitors rolling catch rate
- Positional encoding: rates 1-5 → MEDIUM, rates 6-10 → HIGH
- Meta-brain learns to associate high performance with strategy reinforcement

**4. Hierarchical Reinforcement**
- **Primary Brain**: Immediate rewards for moving toward mouse
- **Meta-Brain Control**: Learns to trigger broad strategy reinforcement across 20-120 previous bars
- **Temporal Credit Assignment**: 5D circular history buffer `(100; 6; 12; 8; 8)` boolean

**5. Dual-Loop Learning**
- **Fast Loop**: Primary brain learns individual vision→movement mappings
- **Slow Loop**: Meta-brain learns when to reinforce successful strategies
- **Emergent Synergy**: Combined system achieves sustained high catch rates

## Branch Context (`reaction-dissociation`)

This branch investigates the dissociation between:
- **Reaction mode** — tactical, 1-bar-timescale direct rewards
- **Dissociation mode** — strategic, 20-120 bar meta-brain reinforcement

Core hypothesis: mixed tactical+strategic signals interfere. Removing tactical rewards during meta-brain operation improves performance. Current evidence: meta-only +15% vs training average, +35% in best runs.

See [scripts/META_EXPERIMENT_README.md](scripts/META_EXPERIMENT_README.md) for experiment details.

## License

[GNU General Public License v3](LICENSE)
