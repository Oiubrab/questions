# CLAUDE.md — ForWhoseAdvantage / Questions

## Project Overview

A **q/kdb-x** neural simulation exploring emergent learning through a **dual-brain architecture** — a primary sensorimotor brain and a meta-brain that learns *when* to reinforce strategy. The biological scenario is a cat learning to hunt a mouse using 8-slice angular vision.

Current branch: `reaction-dissociation` — investigating what happens when tactical (direct) rewards are dissociated from strategic (meta-brain) reinforcement. Key finding: meta-brain-only mode *outperforms* mixed training by ~15%.

---

## Language and Runtime

**Language:** q (kdb-x)

**Run the simulation:**
```bash
q q/programs/cat_mouse.q --seed 42 --bars 20000
```

**Run with pre-trained weights:**
```bash
q q/programs/cat_mouse.q --load-weights results/path/to/weights.q --meta-only
```

**Run tests:**
```bash
q q/tests/test_trinary.q
q q/tests/test_conservation.q
```

**Load modules interactively:**
```q
\l q/load.q
```

---

## Project Structure

```
q/
├── load.q                  / Master loader — \l all modules in dependency order
├── modules/
│   ├── trinary.q           / .trinary — 3-state neuron constants and ops
│   ├── directions.q        / .dirs — 8 direction deltas, opposites, biases
│   ├── synapses.q          / .syn — 4D synapse init, decay, reinforce, punish
│   ├── brain.q             / .brain — propagation fold, pressure, cross-flow
│   ├── brain_engine.q      / .engine — initSystem, runCycle, runCycleSimple
│   └── vision.q            / .vision — atan2 vision, toroidal field, mouse movement
├── programs/
│   └── cat_mouse.q         / Full simulation: main loop, reward logic, kdb-x log
└── tests/
    ├── test_trinary.q
    └── test_conservation.q
results/                    / Experimental output (preserved from prior runs)
```

---

## Architecture Reference

### Primary Brain
- **Size:** 6 rows × 12 cols = 72 trinary neurons
- **Synapses:** 4D nested list `syns[r][c]` = 8×8 int matrix — 64 weights per neuron (8×8 directions), 4,608 total
- **Input:** 8 vision slices → top row, columns 3–10 (0-indexed: `inOff:3i; inLen:8i`)
- **Output:** 8 movement directions ← bottom row, columns 3–10
- **Processing:** 12 brain steps per Bar

### Meta-Brain
- **Size:** 3 rows × 5 cols = 15 trinary neurons
- **Input:** Positional encoding of catch rate (5 positions: rate 1-5 → MEDIUM, 6-10 → HIGH, >10 saturates)
- **Output:** 5 elements: `[temporal_scope (20-120 bars), reinforcement_magnitude (1-2×), reserved×3]`
- **Reward condition:** Only when catch rate ≥ 3 (or ≥ 1 in meta-only mode)

### Key Concepts
| Term | Meaning |
|------|---------|
| **Bar** | One decision cycle; contains 12 primary brain steps |
| **Epoch** | 2,000 Bars; used for learning curve analysis |
| **Trinary** | 3-state neuron: LOW=0i, MEDIUM=1i, HIGH=2i |
| **4D synapse** | `syns[r][c][inDir][outDir]` — weight depends on both incoming *and* outgoing direction |
| **Pressure** | Brain activity level (0–1); triggers overflow regulation via left-column drain |
| **Cross-flow** | Side I/O: meta-brain output enters right column; overflow exits left column |
| **Catch rate** | Successful hunts within rolling 200-Bar window |
| **synapse_history** | 5D circular buffer `(100; rows; cols; 8; 8)` boolean for credit assignment |
| **Propagation fold** | Active neurons processed via `f/[acc; activeList]` — threads mutable state for energy conservation |

---

## Coding Conventions

- **Namespaces:** All modules use `\d .namespace` / `\d .` pattern. Namespaces: `.trinary`, `.dirs`, `.syn`, `.brain`, `.vision`, `.engine`
- **Block comment guard:** Never use a bare `/` on its own line — it triggers a q block comment (everything until `\`). Use `/ ` with a trailing space for section dividers.
- **Multi-way conditional:** `$[c1;a1; c2;a2; default]` — conditions must be bare expressions, NOT wrapped in `[]` blocks (which are always truthy).
- **Propagation as fold:** `f/[init;list]` threads the accumulator `(br;syns;su;inc0;inc1;out)` through each active neuron. This is how energy conservation is maintained without OpenMP.
- **History index (0-indexed):** Slot for bar `b` looking back from current `barN` with write head `hi`: `i: \`int\$(hi - (barN-b) + HISTORY_SIZE) mod HISTORY_SIZE`
- **Brain state dict keys:** `br syns su inc0 inc1 out inp rows cols inOff inLen outOff outLen`
- **Simulation log:** `simLog` is a live kdb-x table — use q queries on it directly, no CSV parsing needed.
- **Comments:** Reward calculations, pressure control, and adaptive threshold logic require in-line explanation. Don't strip comments from these sections.

---

## Testing

Energy conservation is a hard invariant — `test_conservation.q` must pass after any brain changes.

| Change | Test |
|--------|------|
| Trinary constants/shift | `q q/tests/test_trinary.q` |
| Brain propagation | `q q/tests/test_conservation.q` |
| Synapse decay/reinforce | Add assertions to test_conservation or a new test file |

---

## Key Parameters

All in `q/programs/cat_mouse.q`:
```q
ROWS:6i; COLS:12i            / Primary brain size
META_ROWS:3i; META_COLS:5i   / Meta-brain size (keep small — see note below)
MAX_BARS:20000i              / Total simulation time
STEPS_PER_BAR:12i            / Brain propagation depth
RATE_WINDOW:200i             / Catch rate rolling window
EPOCH_SIZE:2000i             / Bars per epoch
HISTORY_SIZE:100i            / Synapse history buffer depth
FIELD_SIZE:100.0             / Toroidal hunting field size
```

---

## What NOT to Do

- **Don't change `META_ROWS`/`META_COLS` casually.** Prior experiments (Fortran era, still valid) show meta-brain larger than 3×5 causes energy accumulation bugs. If you resize, run energy conservation tests first.
- **Don't use bare `/` on its own line** in q files — it opens a block comment. Always add a trailing space: `/ `.
- **Don't wrap multi-way `$[...]` conditions in `[]`** — they become block expressions evaluated as always-truthy lists.
- **Don't use `each` where fold is needed** — `each` discards return values; use `f/[init;list]` when accumulating state updates.
- **Don't commit results or weight files** — `results/` data is preserved but not tracked.

---

## Branch Context (`reaction-dissociation`)

This branch is specifically investigating the dissociation between:
- **Reaction mode** — tactical, 1-bar-timescale direct rewards
- **Dissociation mode** — strategic, 20-120 bar meta-brain reinforcement

The core hypothesis being tested: mixed tactical+strategic signals interfere, and removing tactical rewards during meta-brain operation improves performance. Current evidence supports this (meta-only: +15% vs training average, +35% in best runs).
