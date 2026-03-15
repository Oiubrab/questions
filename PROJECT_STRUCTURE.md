# Project Structure

This document describes the directory structure of the ForWhoseAdvantage q/kdb-x brain simulation.

## Directory Layout

```
.
├── q/                            # All q/kdb-x source code
│   ├── load.q                    # Master loader — \l all modules in dependency order
│   │
│   ├── modules/                  # Reusable q modules (loaded via load.q)
│   │   ├── trinary.q             # .trinary — LOW/MEDIUM/HIGH constants and shift
│   │   ├── directions.q          # .dirs — direction deltas, opposites, biases
│   │   ├── synapses.q            # .syn — 4D synapse init, decay, reinforce, punish
│   │   ├── brain.q               # .brain — propagation fold, pressure, cross-flow
│   │   ├── brain_engine.q        # .engine — initSystem, runCycle, runCycleSimple
│   │   └── vision.q              # .vision — atan2 vision, toroidal field, mouse movement
│   │
│   ├── programs/                 # Simulation entry points
│   │   └── cat_mouse.q           # Full dual-brain cat/mouse simulation
│   │
│   └── tests/                    # Test programs
│       ├── test_trinary.q        # Trinary constant and shift tests
│       └── test_conservation.q   # Energy conservation invariant tests (must pass)
│
├── results/                      # Experiment output (gitignored)
│   └── run_YYYYMMDD_HHMMSS/      # Timestamped run directory
│       ├── simLog.csv            # Exported simulation log
│       ├── weights.q             # Saved brain weights (q binary)
│       └── experiment_report.md  # Auto-generated summary
│
├── scripts/                      # Experiment shell scripts
│   └── META_EXPERIMENT_README.md # Meta-experiment framework documentation
│
├── bin/                          # Empty — no compiled artifacts in q
│
├── CLAUDE.md                     # Claude Code instructions for this project
├── README.md                     # Project overview and quick start
├── BAR_STRUCTURE.md              # Temporal organization and Bar concept
├── CROSS_FLOW_ARCHITECTURE.md    # Pressure-regulated cross-flow I/O design
├── OPTIMIZATION_NOTES.md         # Performance notes and design trade-offs
├── PROJECT_STRUCTURE.md          # This file
└── LICENSE                       # GNU GPLv3
```

## Module Dependency Order

`load.q` loads modules in this order (dependency chain):

```
trinary.q → directions.q → synapses.q → brain.q → brain_engine.q → vision.q
```

Do not reorder — later modules depend on earlier namespaces (e.g. `.brain` uses `.trinary` and `.dirs`).

## Running the Simulation

```bash
# Full simulation
q q/load.q q/programs/cat_mouse.q -- --seed 42 --bars 20000

# Meta-only mode with pre-trained weights
q q/load.q q/programs/cat_mouse.q -- --load-weights results/.../weights.q --meta-only

# Interactive module exploration
q q/load.q
```

## Running Tests

```bash
q q/tests/test_trinary.q
q q/tests/test_conservation.q
```

`test_conservation.q` is a hard invariant — run it after any changes to `brain.q`.

## Querying Results

`simLog` is a live kdb-x table during simulation. After a run it can be exported:

```q
/ During simulation (interactive q session):
select avg catchRate by epochN from simLog
select bar, catchRate, metaScope from simLog where catchRate > 10

/ Save to CSV:
`:/results/run_latest/simLog.csv 0: csv 0: simLog
```

## Key Files

| File | Purpose |
|------|---------|
| `q/modules/brain.q` | Core propagation fold — most complex module; touch with care |
| `q/programs/cat_mouse.q` | Main simulation loop, reward logic, meta-brain strategy |
| `q/tests/test_conservation.q` | Energy conservation invariant — must always pass |
| `CLAUDE.md` | Coding conventions and gotchas specific to this q codebase |

## What's Not Here (Intentionally)

- **No Fortran source** — the project was fully ported to q; `src/` is gone
- **No Makefile** — q needs no compilation
- **No Python visualization scripts** — the kdb-x `simLog` table replaces CSV-based analysis
- **No results or weight files** — `results/` is gitignored; weights are saved locally only
