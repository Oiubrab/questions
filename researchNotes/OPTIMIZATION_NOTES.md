# Performance Notes

## Measured runtimes (q/kdb-x, October 2026, 16-core Linux machine)

| Workload | Wall time |
|---|---|
| 500 bars, one run, fresh brain | ~2.6 s |
| 16 parallel training runs × 20,000 bars, from scratch | 781 s |
| 16 parallel runs × 10,000 bars from trained brains, direct + meta rewards | 430 s |
| same, meta-only | ~235 s |
| same, meta-only with strategic rewards off | 47 s |
| `q/tests/run_all.sh` (all 4 test files) | ~0.4 s |

Cost grows with brain activity: the more neurons are active, the more fold iterations each step takes. That's presumably why the arms differ so much, though the 5× speed-up of the no-strategic-rewards arm hasn't been investigated. Parallel runs share CPU cache and memory bandwidth, so per-run times above are not single-run times.

Because results vary a lot from seed to seed, a sound comparison needs ≥8–16 seeds per condition. Plan on tens of minutes per experiment and use `scripts/run_trials.sh`, which runs one process per core.

## Where the time goes (profiled 2026-10-24)

Per-section timers on 1000 bars. Numbers are ms per bar:

| Section | fresh brain | trained brain |
|---|---|---|
| Strategic (meta-controlled) reinforcement | 1.7 | **11.6** |
| Move cat (catch boost + direct-reward reinforce/punish) | 2.5 | **6.4** |
| Propagation (12 primary + 24 meta steps) | 0.75 | 0.8 |
| Everything else | ~0.3 | ~0.4 |
| **Total** | ~5.2 | **~19.2** |

- **About 95% of the time on a trained brain goes to applying rewards, not to running the brain.**
- Each reinforce, punish or decay pass touches all 4,608 synapses through 72 nested lambdas, and a single bar can call reinforce dozens of times.
- Yet a typical bar uses only about 13 synapses.
- The strategic block repeats a whole-array pass for each of up to 80 lookback bars.

Benchmarks:
- Flattening the synapse array speeds a reinforce up about 9×, with bit-identical results.
- Updating only the used synapses speeds it up about 360×. Results are statistically identical but follow a different random stream.

The step-by-step speed-up plan, with acceptance tests, is Phase P in [REVIVAL_PLAN.md](../plans/inProgress/REVIVAL_PLAN.md).

### Substrate lab (no rewards)

In `q/programs/substrate_lab.q`, propagation is ~95–97% of the cost: **~21 µs per active neuron per step**, constant across regimes.

| Grid | active neurons/step | ms per step |
|---|---|---|
| 16×16 | ~139 | 2.9 |
| 32×32 | ~548 | 12 |

Decay and metrics add only ~2–3% at a sample interval of 50. Larger lab grids are therefore limited by `.sub.step` and `.brain.stepNeuron`. Phase P task P.5 covers propagation speed-ups, and is the priority for lab work.

### Propagation internals

After the reward fixes, propagation becomes the main cost in the cat simulation too. `.brain.stepNeuron` is scalar q inside a fold (q's slowest style): it repacks a 6-element state tuple per neuron, which forces copy-on-write, and it recomputes neighbour indices every time.

## Why propagation is a fold

`f/[acc; activeNeurons]` threads the whole state `(br;syns;su;inc0;inc1;out)` through the active neurons one at a time, so each neuron sees the moves made by the ones before it. That's what makes energy conservation exact: a source loses a unit only if its target actually gained one. `each` would process neurons independently against the same starting state and discard the updates. The Fortran/OpenMP version needed critical sections for the same reason.

A side effect: neurons are processed in row-major order, and only neurons active at the *start* of a step are processed. So energy generally advances about one row per step.

## Data layout

- `syns`: a nested list — rows of columns of 8×8 int matrices. `syns[r;c;inDir;outDir]`.
- `hist`: 100 × rows × cols × 8 × 8 booleans (about 460k flags), written once per bar.
- Direction constants (`.dirs.dr/dc/opp/vBias`) are precomputed 8-element vectors.

## Fortran baseline (historical)

The Fortran implementation, removed in commit `aab6bff` and still in git history, reported 50.7 s single-threaded and 27.8 s with OpenMP on ~15 cores for 20,000 bars. These timings aren't directly comparable with the q numbers above, and the learning results reported alongside them came from code with known bugs (REVIVAL_PLAN.md F6).
