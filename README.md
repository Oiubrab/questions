# ForWhoseAdvantage

A **q/kdb-x** neural simulation in which an artificial cat learns to hunt a mouse. The cat has no neural-network library, no gradients and no backpropagation. Its "brain" is a set of small grids of three-state neurons that pass units of energy to their neighbours along weighted routes: a sight brain and a touch brain feed a central brain that moves the cat, and a reward brain driven by hunger feeds the central brain too. Learning means strengthening the routes that were in use when something good happened.

The project also asks a research question about *how* rewards should be delivered (see [Research status](#research-status)).

> **Goal:** the cat reliably learns to head towards the mouse, and then finds it when it can't see it. Progress (16 seeds × 20,000 decisions, the current defaults): mean move alignment with the mouse direction is **0.68** (0 is random, ~0.90 is the best possible), against 0.41 with the original rewards and wiring. Every seed learns, typically within ~540 decisions. 7 of 16 seeds reach alignment 0.7 by 10,000 decisions; the target is 14 of 16. See the [revival plan](plans/completed/REVIVAL_PLAN.md) §0 and §6.
>
> **Status (October 2026):** revived after a pause. A full audit found and fixed serious bugs and re-ran the project's main experiment under controlled conditions; the headline result in earlier versions of this README did not hold up. That revival is complete ([REVIVAL_PLAN.md](plans/completed/REVIVAL_PLAN.md)). The active work is [obstacles and a hidden mouse](plans/completed/OBSTACLES_PLAN.md); a [3D world and 3D brain](plans/future/3D_PLAN.md) is planned.

---

## What it does

- **The world:** a 100×100 field that wraps around at the edges for both animals (`--walled-world` restores the original box for the cat).
  - The mouse wanders randomly (2 units every 10 decisions).
  - The cat sees only *which of 8 directions* the mouse lies in (45° vision slices).
  - The cat moves 5 or 10 units per decision in one of 8 compass directions.
  - A **catch** is when the cat's path passes within 2 units of the mouse. The mouse then respawns 30–45 units away.
- **The brains** (since November 2026; `--single-brain` restores the original single 6×12 brain):
  - Grids of **trinary** neurons (LOW/MEDIUM/HIGH = 0/1/2 energy units), all of the same kind.
  - **Sight brain** (3×12): vision lights up one cell of its top row.
  - **Touch brain** (3×12): eight "whiskers" light when a wall is within 3 units in that direction, and more strongly just after the cat bumped into one.
  - **Central brain** (3×12): what flows out of the sight and touch brains enters its top row; whatever reaches its bottom row picks the move. The 8 output slots are wired in the same angle order as the vision slices.
  - Each neuron has **64 synapse weights**, one for each (incoming direction, outgoing direction) pair. So where energy goes next depends on where it came from.
  - Energy is **conserved** while it moves; this is enforced by tests.
- **Learning:**
  - **Direct ("tactical") rewards:** every decision, routes used on a move towards the mouse are strengthened, and routes used on a move away are weakened.
  - **Catch boost:** after each catch, routes used in the last 20 decisions are boosted.
  - **Hebbian strengthening and decay:** every use strengthens a route a little, and all routes slowly decay.
- **The reward brain (meta-brain):**
  - A 3×5 brain whose input is the cat's **satiety**: a store of food that each catch fills and time drains. Energy enters its input on the left when the cat is starving and on the right when it's full, and its only reward is to be full.
  - Its output sets how far back (20–80 decisions) and how strongly to apply a "strategic" reinforcement of past routes. That reinforcement fires whenever the catch rate is high enough.
  - Its output is also fed into the side of the central brain. When the cat can't see the mouse, that is what keeps it moving.

- **Harder worlds** (opt-in): walls that block movement and sight (`--world pillars|wall|rooms|random`), vision that blinks off (`--blink`), an evasive mouse, several mice, a dog to avoid, and scripted comparison cats (`--policy random|greedy|reactive|persist|planner`; the planner follows the shortest path around walls).

With the modular brain (the default), a cat averages about 79 catches per 1000 decisions over 20,000 decisions, statistically the same as the original single brain (85). When the mouse can hide, it does worse than a simple scripted cat, and its brain holds no memory of where the mouse was ([obstacles plan](plans/completed/OBSTACLES_PLAN.md) §6).

---

## Research status

**The question (branch `reaction-dissaccociation`):** do fast tactical rewards interfere with the slower, meta-brain-controlled strategic reinforcement? In other words, does a trained cat do *better* if direct rewards are switched off and only the meta-brain's reinforcement remains ("meta-only" mode)?

**The earlier claim** was "+15%: meta-only beats training". It came from the older Fortran version and **does not stand**:
- In the Fortran code, the meta-brain's strategic reinforcement only ran during the first 100 decisions of each run. After that, the meta-brain had no effect at all.
- The comparison set the *best* of 30 trained brains, already warmed up, against the *average* of all 30 runs from scratch, including the early phase before any of them had learned.
- The q port that followed had arithmetic bugs that produced phantom catches. These are now fixed.

**Controlled re-test (October 2026).** 16 brains were trained, and each was continued for 10,000 decisions in four conditions. Results are paired by brain, with bootstrap 95% confidence intervals:

| Condition (from the same trained brain) | Catches per 1000 decisions | vs meta-only |
|---|---|---|
| A — keep training (direct + meta rewards) | **31.1** | |
| B — meta-only (no direct rewards) | 15.7 | — |
| C — meta-only, strategic reinforcement off too | 22.8 | +45% (CI just includes 0) |
| D — meta-only, meta-brain's output zeroed | 16.0 | +2% (no effect) |

What this shows:
- **Switching off direct rewards halves performance** (−49%, CI −66% to −31%). It also keeps falling over time, from 20 to 11 catches per 1000 decisions over the run.
- **The meta-brain contributes nothing measurable:** zeroing its output changes nothing.
- **A minority of brains (3 of 16) do better without direct rewards,** while others collapse. Testing only the best trained brain, as the old experiment did, will tend to pick out exactly these cases.

Full numbers and caveats are in [REVIVAL_PLAN.md §6](plans/completed/REVIVAL_PLAN.md#6-results).

**Since then (October 2026, all paired over 16 seeds):**
- Gentler, cosine-based rewards and angle-ordered output wiring took alignment from 0.41 to 0.68. Both are now the defaults.
- The meta-brain was redesigned around satiety (hunger vs fullness). In the open world it still doesn't help: removing it *raises* alignment by about 0.06.
- Re-learning after the world changes takes a few hundred to a few thousand decisions, and gets no faster with practice.
- November 2026: the brain became modular (sight, touch and reward brains feeding a central brain), and was tested in nine worlds with walls and a hidden mouse against scripted cats ([results and verdicts](researchNotes/OBSTACLES_RESULTS.md)). Touch and the modular layout are clear wins with walls. But the brain has no memory of where the mouse was, and a scripted cat that simply moves at random when it loses sight still beats it in 8 of the 9 worlds.

---

## Quick start

**Requires:** [kdb-x](https://kx.com) with `q` on your `PATH`. **Run everything from the repository root**, because scripts load modules by relative path.

```bash
# One run: 20,000 decisions, seed 42; writes results/seed42/
q q/programs/cat_mouse.q --seed 42 --bars 20000 < /dev/null

# Same, but stay in the q session afterwards to query the live log table
q q/programs/cat_mouse.q --seed 42 --bars 5000
q) select avg pressure, max catches by 1000 xbar bar from simLog

# Continue from a saved brain without direct rewards ("meta-only")
q q/programs/cat_mouse.q --seed 7 --bars 10000 \
  --load-weights results/seed42/weights_seed42.bin --meta-only < /dev/null

# Tests (must pass after any code change)
q/tests/run_all.sh
```

`< /dev/null` makes q exit when the run ends. Without it, q stays open at its prompt.

### Command-line flags (`q/programs/cat_mouse.q`)

| Flag | Meaning |
|---|---|
| `--seed N` | Random seed (default: time of day) |
| `--bars N` | Number of decisions to simulate (default 20000) |
| `--output-dir DIR` | Where outputs go (default `results/seed<N>/`) |
| `--load-weights FILE` | Start from a brain saved by an earlier run (`weights_seed<N>.bin`) |
| `--meta-only` / `--no-direct-rewards` | Turn off direct (tactical) rewards |
| `--no-meta-rewards` | Turn off the meta-brain-controlled strategic reinforcement |
| `--ablate-meta` | Treat the meta-brain's output as all zeros (a control condition) |

An unknown flag stops the run with an error, so a typo can't silently run the wrong condition.

### Dashboards (live viewer and run comparison)

```bash
dashboards/start.sh                                  # live viewer + a simulation (seed 42, 20,000 bars)
dashboards/start.sh live -- --seed 7 --world rooms   # the same with your own simulation flags
dashboards/start.sh compare results/A/s1 results/B/s1   # two finished runs side by side
```

It starts the viewer process and the KX Dashboards server, prints the address, and stops everything on Ctrl-C. Bookmarkable addresses (they work while the script runs): live `http://localhost:10001/#/5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b01`, compare `http://localhost:10001/#/5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b02`. Ports, replay speed and the Dashboards install location are environment variables, listed at the top of the script. `dashboards/e2e/run_e2e.sh` runs the same thing under Firefox as a test.

### Outputs (per run)

| File | Contents |
|---|---|
| `summary.csv` | One row: seed, settings, total catches, move directionality, mean pressure |
| `epoch_summary.csv` | Catches and move counts for each 2000-decision epoch |
| `simulation_log.csv` | One row per decision: positions, vision slice, brain energy, action, catches, pressure |
| `brain_state.csv`, `meta_brain_state.csv`, `synapse_state.csv` | Final brain state |
| `weights_seed<N>.bin` (+ `.bin#`) | Full saved state, for `--load-weights` |

### Multi-seed experiments

Results vary a lot from seed to seed. One trained brain scored 1 catch where another scored 570 under the same condition. So **compare conditions across many seeds, paired by brain**:

```bash
scripts/run_trials.sh -l myrun -i "1 2 3 4 5 6 7 8" -- --bars 10000          # parallel runs
q q/programs/analysis/compare.q results/myrun results/otherrun < /dev/null           # paired stats
scripts/experiments/phase3_experiment.sh                                                # the full re-test above
```

See [scripts/experiments/META_EXPERIMENT_README.md](scripts/experiments/META_EXPERIMENT_README.md).

---

## Documentation

| Document | Covers |
|---|---|
| [OBSTACLES_RESULTS.md](researchNotes/OBSTACLES_RESULTS.md) | **Read first:** what every element did in every world, what to keep, what to do next |
| [OBSTACLES_PLAN.md](plans/completed/OBSTACLES_PLAN.md) | The completed obstacles plan: harder worlds, a hidden mouse, internal-structure probes |
| [VISION_PLAN.md](plans/future/VISION_PLAN.md), [VIEWER_UPLIFT_PLAN.md](plans/future/VIEWER_UPLIFT_PLAN.md), [PARAMETERS_PLAN.md](plans/future/PARAMETERS_PLAN.md), [METRONOME_PLAN.md](plans/future/METRONOME_PLAN.md) | Future plans: richer vision, a better viewer, no hard-coded values, a clock-like internal input |
| [3D_PLAN.md](plans/future/3D_PLAN.md) | Future plan: a 3D world and brain, and the substrate research it builds on |
| [REVIVAL_PLAN.md](plans/completed/REVIVAL_PLAN.md) | The completed revival: design intent, audit findings, fixes, every result so far |
| [KX_DASHBOARDS_PLAN.md](plans/completed/KX_DASHBOARDS_PLAN.md) | The completed live viewer and the side-by-side run comparison |
| [THEORY.md](researchNotes/THEORY.md) | The mathematics: the brain as a Markov chain, learning as a reinforced urn, memory and reward arithmetic, testable predictions |
| [BAR_STRUCTURE.md](researchNotes/BAR_STRUCTURE.md) | What happens in one decision cycle ("bar"), step by step, including every reward |
| [CROSS_FLOW_ARCHITECTURE.md](researchNotes/CROSS_FLOW_ARCHITECTURE.md) | The brain's four input/output edges and the pressure mechanisms (mostly inactive in practice) |
| [OPTIMIZATION_NOTES.md](researchNotes/OPTIMIZATION_NOTES.md) | Runtime costs, why propagation is a fold, Fortran-era history |
| [PROJECT_STRUCTURE.md](researchNotes/PROJECT_STRUCTURE.md) | Directory layout |
| [scripts/experiments/META_EXPERIMENT_README.md](scripts/experiments/META_EXPERIMENT_README.md) | How to run and analyse controlled experiments |
| [CLAUDE.md](CLAUDE.md) | Instructions for AI coding assistants working in this repo |

## History

The project was written in Fortran (later with OpenMP and GPU support), then ported to q/kdb-x in March 2026. The Fortran source is still in git history, e.g. `git show c6f1c17:src/programs/cat_mouse_learning.f90`.

## License

[GNU General Public License v3](LICENSE)
