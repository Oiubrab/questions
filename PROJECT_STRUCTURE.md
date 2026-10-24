# Project Structure

```
.
├── q/
│   ├── load.q                   # Loads all modules (for interactive use: \l q/load.q)
│   ├── modules/
│   │   ├── trinary.q            # .trinary — LOW/MEDIUM/HIGH constants, shift, initGrid
│   │   ├── directions.q         # .dirs — direction deltas dr/dc, opposites, vBias (hBias unused)
│   │   ├── synapses.q           # .syn — synapse init, decay, reinforce/punish, history helpers
│   │   │                        #        (histSlot, lookbackBars)
│   │   ├── brain.q              # .brain — stepNeuron/propagate fold, input copy, meta cross-flow,
│   │   │                        #          overflow capture, pressure, drain
│   │   ├── vision.q             # .vision — norm/dot helpers, vision slice, toroidal distance,
│   │   │                        #           path-to-point distance (catch test), mouse walk
│   │   ├── brain_engine.q       # .engine — initSystem (state dict), runCycle
│   │   │                        #           (runCycleSimple, used for the meta-brain, is identical)
│   │   └── substrate.q          # .sub — lab-only configurable copy of propagation (wrap, bias,
│   │                            #        temperature, order, hebb, sink); not in load.q
│   ├── programs/
│   │   ├── cat_mouse.q          # The simulation: flags, main loop, rewards, outputs
│   │   ├── compare.q            # Paired multi-seed comparison of experiment arms
│   │   ├── substrate_lab.q      # Substrate lab: the energy rule alone, with dynamics metrics
│   │   ├── lab_report.q         # Averages a lab sweep's runs per configuration
│   │   └── theory_checks.q      # Reproduces every verified number in THEORY.md
│   └── tests/
│       ├── run_all.sh           # Runs every test_*.q; exit 1 on any failure
│       ├── harness.q            # Shared assertions (.t.eq / .t.near / .t.ok / .t.done)
│       ├── test_trinary.q       # Constants and shift
│       ├── test_geometry.q      # Distances, vision slices, dot sign, history indexing
│       ├── test_conservation.q  # Energy conservation and direction bookkeeping in propagation
│       ├── test_substrate.q     # .sub matches .brain bit for bit by default; conserves under all settings
│       └── test_weights.q       # Save/load round trip, output files, unknown-flag rejection
├── scripts/
│   ├── run_trials.sh            # Run many brain ids in parallel → results/<label>/s<id>/
│   ├── phase3_experiment.sh     # The controlled re-test (train 16 brains, 4 arms, report)
│   ├── lab_sweep.sh             # Named substrate-lab sweeps → results/lab/<sweep>/
│   └── META_EXPERIMENT_README.md
├── results/                     # Run outputs (gitignored)
├── README.md
├── REVIVAL_PLAN.md              # Audit findings, task list, results, progress log
├── THEORY.md                    # Mathematical model, verified results, predictions
├── BAR_STRUCTURE.md             # One decision cycle step by step
├── CROSS_FLOW_ARCHITECTURE.md   # Brain edges and pressure mechanisms
├── OPTIMIZATION_NOTES.md        # Runtimes and implementation notes
├── PROJECT_STRUCTURE.md         # This file
├── CLAUDE.md                    # Instructions for AI coding assistants
└── LICENSE                      # GNU GPLv3
```

## Module load order

`load.q` and `cat_mouse.q` both load: `trinary → directions → synapses → brain → vision → brain_engine`. Every module uses the `\d .namespace` … `\d .` pattern. Paths are relative, so **run everything from the repository root.**

## Results layout

A single run writes to `results/seed<N>/` by default, or to `--output-dir`:

```
summary.csv  epoch_summary.csv  simulation_log.csv
brain_state.csv  meta_brain_state.csv  synapse_state.csv
weights_seed<N>.bin  weights_seed<N>.bin#      # full state for --load-weights
```

`scripts/run_trials.sh -l LABEL` writes `results/LABEL/s<id>/` (the same files plus `stdout.txt`). `scripts/phase3_experiment.sh` writes `results/p3_train/`, `results/p3_{A,B,C,D}/` and `results/p3_report.txt`.

## What isn't here

- **Fortran source, the Makefile, Python visualisation tools and the old shell experiment scripts** were removed in the q port (`aab6bff`). They're still in git history.
- **Old Fortran weight files** (`.bin` from the Fortran build) are a different format and can't be loaded by the q version.
