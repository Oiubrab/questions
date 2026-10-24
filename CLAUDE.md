# CLAUDE.md — ForWhoseAdvantage / Questions

> **North star: the cat must actually learn to head towards the mouse.** Every change should be justified by progress on that goal, measured as in REVIVAL_PLAN.md §0. The lab, the theory and the speed work are means to it, not ends in themselves.
>
> **Read [REVIVAL_PLAN.md](REVIVAL_PLAN.md) before starting work.** It is the active plan: audit findings (F1–F18), checkboxed tasks, results and a progress log. Continue its unchecked tasks. Tick each one only after its acceptance check passes, and append what you did to its Progress Log. Decisions in its §7 belong to the user, so ask rather than choose.

## Project overview

A **q/kdb-x** simulation: a cat learns to hunt a mouse with a 6×12 grid of trinary neurons, using direction-dependent ("4D") synapses, energy-conserving propagation and reward-modulated route strengthening. A 3×5 **meta-brain** reads the recent catch rate and modulates a "strategic" reinforcement of past routes.

**Current branch:** `reaction-dissaccociation` (spelt that way in git). It tests whether tactical (per-move) rewards interfere with strategic (meta-brain) reinforcement.

**Status:** a controlled re-test (Oct 2026, REVIVAL_PLAN.md §6) found that:
- removing tactical rewards **halves** performance (−49%);
- the meta-brain's outputs have **no measurable effect**.

The earlier "+15% meta-only" claim was an artefact. Don't repeat it, or any other performance claim not in §6.

## Commands (run from the repo root — module paths are relative)

```bash
q q/programs/cat_mouse.q --seed 42 --bars 20000 < /dev/null       # one run → results/seed42/
q q/programs/cat_mouse.q --seed 7 --bars 10000 --load-weights results/seed42/weights_seed42.bin --meta-only < /dev/null
q/tests/run_all.sh                                                  # all tests (~0.4 s); must pass
scripts/run_trials.sh -l LABEL -i "1 2 3 4" -- --bars 5000          # parallel seeds → results/LABEL/s<id>/
q q/programs/compare.q results/REF results/OTHER < /dev/null        # paired comparison
q q/programs/substrate_lab.q --rows 16 --cols 16 --wrap 1 --steps 3000 < /dev/null   # substrate lab (Phase S)
scripts/lab_sweep.sh s3                                             # named lab sweep → results/lab/s3/report.txt
```

The substrate lab (`q/modules/substrate.q`, `.sub`) is a configurable copy of the propagation rule, used for Phase S experiments. With `.sub.defaults` it must stay bit-identical to `.brain.propagate`; `test_substrate.q` checks this. If you change `.brain.stepNeuron`, mirror the change in `.sub.step`.

- Without `< /dev/null`, q stays at its prompt after the run, with the `simLog` table available to query.
- Flags: `--seed --bars --output-dir --load-weights --meta-only/--no-direct-rewards --no-meta-rewards --ablate-meta`. They're documented at the top of `cat_mouse.q`.
- An unknown `--` flag aborts the run.
- Interactive: `\l q/load.q`.

Theory: [THEORY.md](THEORY.md) models the brain as an absorbing Markov chain and learning as a reinforced urn. Its claims are tagged by evidence level, and `q q/programs/theory_checks.q < /dev/null` reproduces the verified numbers. When an experiment confirms or refutes one of its predictions (P1–P7), update the tag there.

Layout: see [PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md). The exact per-bar sequence and every reward rule: see [BAR_STRUCTURE.md](BAR_STRUCTURE.md).

## Architecture quick reference

| | Primary brain | Meta-brain |
|---|---|---|
| Size | 6×12 = 72 neurons | 3×5 = 15 neurons |
| Synapses | `syns[r][c]` = 8×8 int (inDir × outDir), init 25–1000, range [25, 2,000,000] | same structure |
| Input | vision slice → top row, cols 2–9 (`IN_OFF:2i`) | catch-rate positional code → top row, cols 0–4 |
| Output | bottom row, cols 2–9 → 8 move directions | bottom row, cols 0–4 → `out[0]` scope (20/40/80 bars), `out[1]` magnitude (1/2), and all 5 → primary right column |
| Steps per bar | 12 | 24 |

| Term | Meaning |
|---|---|
| Bar | One decision cycle. Epoch = 2000 bars (reporting only). |
| Trinary | LOW=0i, MEDIUM=1i, HIGH=2i energy units per neuron |
| `su` | Synapse-usage flags for the current bar, the basis for all credit assignment |
| `hist` | 100-slot circular buffer of past `su`. `histIdx` = next slot to write. Use `.syn.histSlot` / `.syn.lookbackBars`. |
| `rateCounter` | Catches in the last 200 bars |
| Pressure | Energy ÷ (rows×cols×2). **In practice < 0.06, so the drain (p > 0.1) and the meta throttle (p ≥ 0.3) never engage.** |
| Brain state dict keys | `br syns su inc0 inc1 out inp rows cols inOff inLen outOff outLen` |

## Coding conventions

- **Namespaces:** every module uses the `\d .namespace` … `\d .` pattern. The namespaces are `.trinary .dirs .syn .brain .vision .engine`. Tests use `.t`, from `q/tests/harness.q`.
- **Block comment guard:** never put a bare `/` on its own line, because it starts a block comment that runs to the next `\`. Use `/ ` with a trailing space for dividers.
- **Multi-way conditional:** `$[c1;a1; c2;a2; default]`. Conditions are bare expressions, never wrapped in `[]` (a `[]` block is always truthy).
- **Propagation is a fold:** `f/[(br;syns;su;inc0;inc1;out); activeList]`. Never use `each` where state must accumulate, because `each` discards the updates.
- **Comments:** reward calculations, pressure control and adaptive-threshold logic need in-line explanation. Don't strip those comments.
- **Simulation log:** `simLog` is a live table (`bar mouseX mouseY catX catY visionSlice brainEnergy outEnergy outAction moveDist catches pressure overflow`), and `catches` is cumulative. Query it directly.

## q pitfalls that have caused real bugs here

- **Right-to-left evaluation:** `dx*dx+dy*dy` is `dx*(dx+(dy*dy))`, and `abs dx < y` is `abs (dx<y)`. Use `.vision.norm` / `.vision.dot`, or parenthesise the left operand. This bug class made the q port's catches phantom (F1).
- **Null comparisons:** `0n < x` is true. Guard any float that could be null before comparing it.
- **`` `int$`` rounds**, it doesn't truncate. Use `floor` for bucketing (F2).
- **`x mod y`** is x modulo y, and it's floored (`-3 mod 100` = 97).
- **Subprocesses:** use `system "env q …"`, never `system "q …"`. q can take the latter as its own `\q` command and silently do nothing.
- **Symbol literals can't contain `-`:** write `hsym `$"path"` for paths.
- **`set` on nested data** writes a companion file `name#`. Both files are needed for `get`.

## Testing

`q/tests/run_all.sh` runs every `q/tests/test_*.q` and exits 1 on any failure. Add new tests as `test_<topic>.q` using `.t.eq / .t.near / .t.ok / .t.done[]`.

| Change | Covered by |
|---|---|
| Trinary constants/shift | `test_trinary.q` |
| Geometry, vision, catch test, history indexing | `test_geometry.q` |
| Propagation (energy conservation is a hard invariant) | `test_conservation.q` |
| Weight save/load, output files, CLI flags | `test_weights.q` |
| Lab substrate (`.sub`) faithful to `.brain`, conserving under all settings | `test_substrate.q` |
| Reward or decay rules | no unit test yet. Measure behaviour with a paired multi-seed experiment (see `scripts/META_EXPERIMENT_README.md`). |

Learning behaviour varies hugely by seed: never draw conclusions from single runs or a few seeds.

## Tooling

Two Claude Code skills from the `q-knowledge@kx-skills` plugin apply to this repo:

| Skill | Use |
|-------|-----|
| `q` | kdb+/q language reference — idiomatic q, qsql, IPC, common errors. Auto-triggers on q work; no setup needed. |
| `/qlint-snippet` | KX qlint. Requires `AXLIBRARIES_HOME` (machine-local setup, see below). |

**qlint is a *snippet* linter, not a file linter.** Feeding it a whole module produces a cascade of false positives: every module here opens with `\d .namespace` (see Coding Conventions), and qlint parses everything after the `\d` as one unindented expression. On `trinary.q` that yields 4 bogus `UNINDENTED_CODE` errors plus `UNUSED_VAR` on every top-level constant — all noise. Lint one function at a time:

```q
shift:{[v;d] HIGH & LOW | v + `int$d}
```

Also expect `UNDECLARED_VAR` on `HIGH`/`MEDIUM`/`LOW` in any snippet lifted out of `.trinary` — they resolve through the namespace at runtime, so that warning is expected here too.

**Per-machine setup:** qlint ships inside KX Developer's *nested* `ax-libraries.zip`. Extract that, flatten `ws/lib/<os>_x64/*` into `ws/lib/`, then export `AXLIBRARIES_HOME=<ax-libraries dir>` and `QLINT_DIR=$AXLIBRARIES_HOME/ws`. `AXLIBRARIES_HOME` is the variable that matters — setting `QHOME` or `LD_LIBRARY_PATH` instead does **not** work, and a full KX Developer `install.sh` run is unnecessary.

## Key parameters (`q/programs/cat_mouse.q`)

```q
ROWS:6i; COLS:12i              / primary brain
IN_OFF:2i; OUT_OFF:2i          / input/output column offsets (8 wide)
META_ROWS:3i; META_COLS:5i     / meta-brain
META_INPUT_OFFSET:1i           / meta output → primary right column, rows 1–5
MAX_BARS:20000i                / default; override with --bars
STEPS_PER_BAR:12i              / primary steps per bar (meta runs 2× per step)
RATE_WINDOW:200i               / catch-rate window
EPOCH_SIZE:2000i               / reporting epoch
HISTORY_SIZE:100i              / su history depth
FIELD_SIZE:100.0               / mouse wraps; cat is clamped to the box
MOUSE_MOVE_INTERVAL:10i        / mouse steps 2.0 units every 10 bars (STEP_SIZE:5.0 is unused)
```

## What NOT to do

- **Don't trust or repeat performance claims** that aren't backed by REVIVAL_PLAN.md §6.
- **Don't resize the meta-brain casually.** Fortran-era notes blamed sizes above 3×5 for energy accumulation. Propagation itself conserves energy at larger sizes (tested), so any accumulation would come from the overwriting inputs (F16). Run the tests and watch `brainEnergy` if you resize.
- **Don't use a bare `/` on its own line**, and don't wrap `$[...]` conditions in `[]`.
- **Don't use `each` where a fold is needed.**
- **Don't commit `results/` or weight files** (`*.bin`, `*.bin#`); both are gitignored.
- **Don't run qlint on a whole module.** Lint single functions (see Tooling).
- **Don't commit unless the user asks.**
