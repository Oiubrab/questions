# Project Structure

```
.
├── q/
│   ├── load.q                   # Loads all modules (for interactive use: \l q/load.q)
│   ├── modules/
│   │   ├── trinary.q            # .trinary — LOW/MEDIUM/HIGH constants, shift, initGrid
│   │   ├── directions.q         # .dirs — direction deltas dr/dc, opposites, vBias
│   │   ├── synapses.q           # .syn — synapse init, decay, reinforce/punish, history helpers
│   │   │                        #        (histSlot, lookbackBars), two-timescale relax/consolidate (G.R5)
│   │   ├── brain.q              # .brain — stepNeuron/propagate fold, input copy, meta cross-flow,
│   │   │                        #          satiety (meta input), additive internal input,
│   │   │                        #          overflow capture, pressure, drain
│   │   ├── vision.q             # .vision — norm/dot helpers, vision slice, toroidal distance,
│   │   │                        #           path-to-point distance (catch test), mouse walk
│   │   ├── brain_engine.q       # .engine — initSystem (state dict), runCycle
│   │   │                        #           (runCycleSimple, used for the meta-brain, is identical)
│   │   ├── substrate.q          # .sub — lab-only configurable copy of propagation (wrap, bias,
│   │   │                        #        temperature, order, hebb, sink); not in load.q
│   │   ├── viz.q                # .viz — opt-in async publisher for the live viewer (--viz-port)
│   │   ├── theory.q             # .theory — absorbing-chain maths (chain, policy, stats); analysis only
│   │   ├── relearn.q            # .relearn — re-learning worlds, schedule parsing, switch metrics (G.4)
│   │   ├── world.q              # .world — obstacle worlds: layouts, line of sight, blocked moves, clear spawns,
│   │   │                        #          whiskers (touch), scripted policies, planner (geodesic distance field)
│   │   └── modular.q            # .mod — modular brain: split/stack the sight, touch and central brains, transfers
│   ├── programs/
│   │   ├── cat_mouse.q          # The simulation: flags, main loop, rewards, outputs
│   │   ├── compare.q            # Paired multi-seed comparison of experiment arms
│   │   ├── substrate_lab.q      # Substrate lab: the energy rule alone, with dynamics metrics
│   │   ├── lab_report.q         # Averages a lab sweep's runs per configuration
│   │   ├── theory_checks.q      # Reproduces every verified number in THEORY.md
│   │   ├── policy_ceiling.q     # Weights-only predicted alignment vs measured, per arm (G.R0)
│   │   ├── relearn_report.q     # Re-learning speed report: every switch metric, ratios, savings (G.4)
│   │   ├── world_report.q       # Obstacle-world report: catches, visibility, behaviour while the mouse is hidden
│   │   ├── structure.q          # I.1/I.3: carry-over and dominant-route loops from saved weights
│   │   ├── decode.q             # I.2: decode the last-seen slice from logged brain states (--log-state)
│   │   ├── lesion_report.q      # I.4: lesion maps (written by scripts/lesion_map.sh)
│   │   ├── viz_server.q         # Viewer process: latest frame, ring buffers, replay of one or two runs,
│   │   │                        #   route maps from saved weights (KX Dashboards)
│   │   └── viz_spike.q          # Dashboards feasibility spike (synthetic data)
│   └── tests/
│       ├── run_all.sh           # Runs every test_*.q; exit 1 on any failure
│       ├── harness.q            # Shared assertions (.t.eq / .t.near / .t.ok / .t.done)
│       ├── test_trinary.q       # Constants and shift
│       ├── test_geometry.q      # Distances, vision slices, dot sign, history indexing
│       ├── test_conservation.q  # Energy conservation and direction bookkeeping in propagation
│       ├── test_golden.q        # P.0 golden run: seed 42 x 300 bars, MD5 of the serialised final state
│       ├── test_relearn.q       # Re-learning worlds, schedule parsing, switch metrics on synthetic series
│       ├── test_world.q         # Walls: hits, blocked moves, line of sight, wrapped copies, clear spawns
│       ├── test_synapses.q      # Flat synapse updates match the nested reference, incl. RNG stream
│       ├── test_sparse_synapses.q  # Sparse usage/history, lookback batching, batched reward loops
│       ├── test_substrate.q     # .sub matches .brain bit for bit by default; conserves under all settings
│       ├── test_viz.q           # Viewer off = identical run; dead viewer tolerated; ring buffer wraps
│       └── test_weights.q       # Save/load round trip, output files, unknown-flag rejection
├── scripts/
│   ├── run_trials.sh            # Run many brain ids in parallel → results/<label>/s<id>/
│   ├── phase3_experiment.sh     # The controlled re-test (train 16 brains, 4 arms, report)
│   ├── lab_sweep.sh             # Named substrate-lab sweeps → results/lab/<sweep>/
│   ├── e2e_dashboards.sh        # Playwright/Firefox end-to-end check: live dashboard, or `compare`
│   ├── relearn_experiment.sh    # G.4 re-learning experiment (schedule + fresh controls + report)
│   ├── world_experiment.sh      # Obstacle worlds: brain arms vs scripted baselines, per world
│   ├── lesion_map.sh            # I.4: freeze each trained brain of an arm, lesion one cell at a time
│   ├── obstacles_closeout.sh    # Obstacles plan close-out batch (resumable; skips finished arms)
│   └── META_EXPERIMENT_README.md
├── dashboards/                  # KX Dashboards definitions (Cat Brain live, Cat Brain Compare), connection,
│                                #   builder, install script, Firefox e2e tests
├── results/                     # Run outputs (gitignored)
├── plans/
│   ├── inProgress/
│   │   └── OBSTACLES_PLAN.md    # Active: obstacles, a hidden mouse, internal-structure probes
│   ├── future/
│   │   └── 3D_PLAN.md           # A 3D world and brain, plus the carried substrate research
│   └── completed/
│       ├── REVIVAL_PLAN.md      # The 2026-10 revival: design intent, findings F1–F22, results, log
│       └── KX_DASHBOARDS_PLAN.md  # Live viewer and side-by-side comparison (V0–V3)
├── researchNotes/
│   ├── THEORY.md                # Mathematical model, verified results, predictions
│   ├── BAR_STRUCTURE.md         # One decision cycle step by step
│   ├── CROSS_FLOW_ARCHITECTURE.md  # Brain edges and pressure mechanisms
│   ├── OPTIMIZATION_NOTES.md    # Runtimes and implementation notes
│   └── PROJECT_STRUCTURE.md     # This file
├── README.md
├── CLAUDE.md                    # Instructions for AI coding assistants
└── LICENSE                      # GNU GPLv3
```

## Module load order

`load.q` and `cat_mouse.q` both load: `trinary → directions → synapses → brain → vision → brain_engine → viz`; `cat_mouse.q` also loads `relearn`, `world` and `modular`. Every module uses the `\d .namespace` … `\d .` pattern. Paths are relative, so **run everything from the repository root.**

## Results layout

A single run writes to `results/seed<N>/` by default, or to `--output-dir`:

```
summary.csv  epoch_summary.csv  simulation_log.csv
brain_state.csv  meta_brain_state.csv  synapse_state.csv
weights_seed<N>.bin  weights_seed<N>.bin#      # full state for --load-weights (11th component with --two-timescale)
relearn_log.csv  switches.csv                   # with --relearn-schedule
world_log.csv                                   # with --world, --blink, --policy, --mice, --evasive or --distractor
state_log.bin                                   # with --log-state (I.2)
```

`scripts/run_trials.sh -l LABEL` writes `results/LABEL/s<id>/` (the same files plus `stdout.txt`). `scripts/phase3_experiment.sh` writes `results/p3_train/`, `results/p3_{A,B,C,D}/` and `results/p3_report.txt`.

## What isn't here

- **Fortran source, the Makefile, Python visualisation tools and the old shell experiment scripts** were removed in the q port (`aab6bff`). They're still in git history.
- **Old Fortran weight files** (`.bin` from the Fortran build) are a different format and can't be loaded by the q version.
