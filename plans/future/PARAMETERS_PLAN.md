# Plan: no hard-coded values

**Status (2026-11-03):** written, not started. Waiting on the user's decisions in §5. The inventory in §1 is from a quick audit; task P0.1 produces the exact one.

**Why it exists.** The user (2026-11-03): "There should never be hard-coded values anywhere. If there's some value that needs to be set, it should be set in the application of that script. Not in something hard-coded in the script itself. … If there's way too many hard-coded values then yeah, make a plan." There are way too many (§1), and the right mechanism is a design choice (§5), so this is a plan.

**How to use this document:** the same rules as the other plans. Tasks are checkboxes (`[ ]` not started, `[~]` partly done, `[x]` done and verified). Tick only after the acceptance check passes, with the date. Every session appends to the Progress Log (§7). Decisions in §5 belong to the user.

---

## 0. Goal

Every value someone might want to change is **named, documented in one place, and set when the program is run**, not buried as a literal in the code. A run records the values it used, so any result can be reproduced from its own output directory.

**The hard constraint: defaults don't change anything.** With no overrides, every program produces byte-identical output to today (the golden digest, and the old-versus-new comparison used in the 2026-11-03 refactor: 12 flag configurations plus a 20,000-bar run). This is a refactor, not a re-tuning; no result in the other plans changes.

## 1. What the audit found

Roughly 200 values in about 25 files. Counts are approximate; P0.1 makes them exact.

| Area | Where | About | Examples |
|---|---|---|---|
| Simulation constants | `q/programs/cat_mouse/params.q`, `state.q`, `config.q` | 45 | steps per bar 12, epoch 2,000, catch-rate window 200, history depth 100, mouse moves every 10 bars, step 5 units, meta-brain size 3×5, input/output offsets, whisker range 3, evade radius 15, dog distance 25, anti-oscillation thresholds 0.6 / 0.7 / 0.85, progress interval 50, snapshot every 1,000 bars |
| Inline in the bar loop | `q/programs/cat_mouse/bar_loop.q` | 40 | catch radius 2.0, spawn ring 30–45 units, catch-boost lookback 20 bars, strategic scope 20–120 (×40), strategic factor 1.1, decay every 5 bars, drain 0.1 / 0.15 / 0.3, alignment window 500 and its 0.6 threshold, towards/away band ±0.01, move-vs-dog cosine 0.5, momentum cap 4, reward step 0.9 / +0.03 / −0.02 |
| Learning rules | `q/modules/brain/synapses.q` | 20 | reinforce ×1.2, punish ×0.8, per-step 0.95, noise 0.9 + 0.2·U, decay 0.98 + 0.015·U (period 80), strength floor 25, cap 2,000,000, initial range 25–1,000 |
| Brain physics | `q/modules/brain/brain.q` | 12 | unconditional reinforcement 1,000 (flag overrides), satiety edges 0.5 / 2 / 5 / 10, input labels 7 and 8, flow throttle 0.3 / 0.6, drain probabilities |
| World | `q/modules/world/world.q`, `vision.q`, `relearn.q` | 25 | wall thickness 4, the four named layouts' coordinates, random layout sizes 6–20, stop-short 0.05, retry limit 10,000, vision epsilon 0.0001, re-learning windows (99, 199, 499, 999), small-drop 0.1, recovery thresholds 0.5 / 0.6 |
| Analysis programs | `q/programs/analysis/*.q`, `lab/`, `viz/` | 20 | bootstrap draws 5,000 / 10,000, CI quantiles, fixed seeds 42 / 7 / 11 / 12, decode bands, viewer ring buffer 240 frames, rolling windows 200 / 100, theory-checks seeds `101…116` |
| Experiment scripts | `scripts/**`, `dashboards/e2e/run_e2e.sh` | 40 | seeds 1…16, bars 10,000 / 20,000 / 5,000 / 2,000, arm and world lists (`blink 20:5`, `--spontaneous 0.05`, `--sat-edges 1,4,10,20`), seed offset 100, ports 6814 / 10001, lab sweep grid |
| Tests | `q/tests/` | many | fixtures: seeds, sizes, expected values |

**Three kinds of value**, which need different treatment:

1. **Model parameters:** a different value is a different experiment (reward factors, drain thresholds, window lengths, distances). These are the point of this plan.
2. **Experiment design:** which worlds, arms, seeds and run lengths an experiment script covers. These belong in the experiment's own definition and settings, not inside the shell logic.
3. **Structure:** 8 directions, 64 synapses per cell, π, the three trinary levels, array indices. Changing these would change the program, not tune it. They stay in code, each as a named constant with a comment, never as a bare number.

Already done well: many `cat_mouse.q` flags (`--hebb`, `--reward-scale`, `--wiring`, `--weight-ratio` …) and most script knobs (`K`, `BARS`, `TAG`, `CONFIGS`, `SCHED` …) are settable, with their defaults written next to the code that reads them.

## 2. Proposed mechanism

**One registry of named parameters per program, with the defaults in a plain config file, not in code.**

- **`config/<program>.conf`:** `name = value  # description`, one per line, grouped. This holds every default. The code contains no tunable numbers; when a value is needed it is looked up by name, and a missing name is a startup error that names it. (A user can copy the file, edit it, and pass `--config FILE`.)
- **Overrides at launch, three layers, last wins:** `config/cat_mouse.conf` → `--config FILE` (any number) → `--set name=value` (any number) → the existing named flags (`--hebb 250` and the rest keep working and map onto the same names).
- **Every run writes `params.csv`** (name, value, source) into its output directory, so a result carries its own full settings. This also lets `compare.q` and the reports show exactly which values differ between arms.
- **Library modules** (`.syn`, `.brain`, `.world`, …) read their values from a namespace dictionary the program fills at startup (the pattern already used by `.syn.MAX_STRENGTH` and `.brain.HEBB`), so a module function doesn't contain `1.2` or `0.95`.
- **Analysis programs and shell scripts:** analysis programs take their values as flags with a config file for the rest (`--boot 10000`, `--seed 42`); shell scripts take environment variables, as most already do, with their defaults in a settings block at the top and their arm and world lists in a `.conf` beside the script.

Why not the alternatives: one flag per value would be about 200 flags to document and validate (§5, decision 1); a no-defaults rule (every run must be given every value) makes the common command about 200 arguments long, unless a config file supplies them, which is what the proposal above is.

## 3. Rules once this is done

- A tunable number appears **once**, in a `.conf` file, with a description.
- Code looks a value up by name (for example `.params.get`stepsPerBar`); a numeric literal in code is either an index, 0 or 1, a structural constant with a name and a comment, or a bug.
- A new value or flag goes in the `.conf` file first. (CLAUDE.md gets this rule when decision 1 is made.)
- `q/tests/test_params.q` enforces it (task P7): it scans the source and fails on a bare tunable-looking literal outside an allowlist, so the rule doesn't decay.

## 4. Tasks

Every task's acceptance check includes: **defaults give byte-identical output.** The check is the golden digest plus `scripts/probes/regression_compare.sh` (P0.2), which compares a saved reference build against the working tree on a dozen flag configurations.

- [ ] **P0.1 Exact inventory.** A script lists every numeric literal in `q/` and `scripts/` outside tests, with file, line and a proposed kind (parameter, design, structure). Output into this plan's §6. *Accept:* every literal classified; the counts in §1 replaced.
- [ ] **P0.2 Regression comparator.** Save the 2026-11-03 old-versus-new comparison as `scripts/probes/regression_compare.sh REF_TREE`: runs both trees on a fixed list of configurations in parallel and compares every output file. *Accept:* run against the current tree gives all "ok".
- [ ] **P1 Registry.** `q/modules/params.q` (`.params`): read `.conf`, layer `--config` and `--set`, typed values, unknown or missing name = startup error, write `params.csv`. Existing flags map onto names. *Accept:* unit tests (layering, types, errors); `cat_mouse.q` writes `params.csv`; defaults still byte-identical.
- [ ] **P2 cat_mouse constants** (`params.q`, `state.q`, `config.q`; about 45). *Accept:* P0.2 all ok.
- [ ] **P3 Bar loop** (about 40 inline). *Accept:* P0.2 all ok.
- [ ] **P4 Learning rules and brain physics** (`synapses.q`, `brain.q`; about 32). Functions read from `.syn.P` and `.brain.P`. *Accept:* P0.2 all ok; `test_synapses.q`, `test_sparse_synapses.q` and `test_conservation.q` pass unchanged; per-bar time unchanged (±2%).
- [ ] **P5 World, vision, re-learning** (about 25; layouts become named layout data in the config). *Accept:* P0.2 all ok; `test_world.q` and `test_relearn.q` pass.
- [ ] **P6 Analysis, lab and viewer programs** (about 20): bootstrap draws, seeds, bands, windows and ring sizes become flags or config. *Accept:* the saved-baseline comparison of every analysis program's output (the 2026-11-03 method) is identical.
- [ ] **P7 Experiment scripts** (about 40): settings blocks at the top, arm and world lists in `.conf` files beside the scripts, no inline bar counts. `K`/`SCALE` style overrides stay. *Accept:* each script's reduced-scale run (K=2, short runs) completes as it did on 2026-11-03, and the full-scale defaults are unchanged.
- [ ] **P8 Enforcement.** `q/tests/test_params.q` (source scan with an allowlist); CLAUDE.md "What NOT to do" gains the rule; docs (README flag table, BAR_STRUCTURE) point at the `.conf` files instead of repeating defaults. *Accept:* the test fails when a literal is added and passes on the tree.

Rough size: P1 is the design work; P2–P7 are mostly mechanical but touch hot code, which is why every step is gated by byte-identical output.

## 5. Decisions for the user

1. **Mechanism.**
   - (a) A registry with defaults in `.conf` files, `--config` and `--set`, existing flags kept (§2).
   - (b) One named flag per value, defaults in code (about 200 flags).
   - (c) Defaults in a registry inside the code, overridable with `--set`.

   **Recommendation: (a).** It is the only option where the code contains no tunable numbers, as you asked, without a 200-flag interface. (c) is cheaper but leaves the defaults "hard-coded in the script", just in one table.
2. **What counts as a value.** The three kinds in §1: parameters and experiment design both go to config; structure (8 directions, π …) stays as named constants. **Recommendation: as in §1.** If you want even those configurable, say so, but changing the number of directions would change the code, not tune it.
3. **Tests.** Fixtures such as `--seed 5 --bars 200` in tests are inputs, not defaults. **Recommendation:** exempt `q/tests/`.
4. **Experiment definitions.** Arm and world lists move into `.conf` files beside each script (the script becomes a runner). **Recommendation: yes.** Alternative: keep them in the script as a clearly-marked settings block at the top.
5. **Reproducibility file.** Write `params.csv` into every run directory. **Recommendation: yes;** it is small and makes results self-describing.
6. **Pace.** All of P1–P8 in one go, or one phase at a time with your sign-off after P1 (the mechanism)? **Recommendation:** P0 and P1 first, then your look at the mechanism on a real file before the mechanical phases.

## 6. Results

None yet.

## 7. Progress Log

### 2026-11-03 — plan written
- The user asked that nothing be hard-coded and that a plan be made if there were too many. An audit found about 200 tunable values in about 25 files, of three kinds (model parameters, experiment design, structure).
- Proposed: a parameter registry with defaults in `.conf` files, layered overrides, and a `params.csv` per run; phases gated by byte-identical output on defaults; an enforcing test at the end.
- Nothing built. Waiting on §5.
