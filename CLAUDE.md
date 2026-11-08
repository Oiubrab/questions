# CLAUDE.md — ForWhoseAdvantage / Questions

> **North star: the cat must actually learn to head towards the mouse, and then find it when it can't see it.** Every change should be justified by progress on that goal: the open-world measures are in the [revival plan](plans/completed/REVIVAL_PLAN.md) §0, the hidden-mouse measures in the obstacles plan §3. The lab, the theory and the speed work are means to it, not ends in themselves.
>
> **In progress (2026-11-08): [CATCH_RATE_PLAN.md](plans/inProgress/CATCH_RATE_PLAN.md)**, raising the catch rate without losing spontaneity or re-learning. Its analysis is [CATCH_RATE_ANALYSIS.md](researchNotes/CATCH_RATE_ANALYSIS.md); its predictions (§3) were fixed before any run, and it tests on fresh seeds 17–32. The obstacles plan is completed; [OBSTACLES_RESULTS.md](researchNotes/OBSTACLES_RESULTS.md) says what each element did in every world. Every plan has checkboxed tasks, results, decisions and a progress log: tick a task only after its acceptance check passes, append what you did to its Progress Log, and ask the user about the decisions in its decisions section rather than choosing. The completed [revival plan](plans/completed/REVIVAL_PLAN.md) holds the design intent (§2), the audit findings F1–F22 (§3) and every result so far (§6); read §2 and §6 too.

## Project overview

A **q/kdb-x** simulation: a cat learns to hunt a mouse with grids of trinary neurons, using direction-dependent ("4D") synapses, energy-conserving propagation and reward-modulated route strengthening. Since 2026-11-02 the brain is **modular** (the user's design): a sight brain and a touch brain (3×12 each) feed a central brain (3×12) whose output moves the cat, and a 3×5 **reward brain** (the meta-brain) reads the cat's satiety (starving … full), feeds the central brain and modulates a "strategic" reinforcement of past routes; its motivation is to be full. `--single-brain` restores the original 6×12 brain.

**Branch:** work happens on `main`. The `reaction-dissaccociation` branch (spelt that way in git) was merged in PR #4; it tested whether tactical (per-move) rewards interfere with strategic (meta-brain) reinforcement.

**Docs layout:** plans live in `plans/inProgress/` (`CATCH_RATE_PLAN.md`), `plans/future/` (not started: `VISION_PLAN.md`, seeing walls and other objects without flooding the sight brain; `PARAMETERS_PLAN.md`, removing hard-coded values; `VIEWER_UPLIFT_PLAN.md`, a purpose-built viewer that shows inputs, outputs, flow and learning; `METRONOME_PLAN.md`, a sparse clock-like internal input at a fixed spot, replacing the removed random `--spontaneous`; `EVOLUTION_PLAN.md`, an evolutionary search over the setup (settings first, structure later); `3D_PLAN.md`, a 3D world and brain, plus the carried substrate research) and `plans/completed/` (`REVIVAL_PLAN.md`, the 2026-10 revival; `KX_DASHBOARDS_PLAN.md`, the viewer; `OBSTACLES_PLAN.md`, harder worlds and the hidden mouse). Move a plan between folders as its state changes. Reference notes live in `researchNotes/` (CATCH_RATE_ANALYSIS, OBSTACLES_RESULTS, THEORY, BAR_STRUCTURE, CROSS_FLOW_ARCHITECTURE, OPTIMIZATION_NOTES, PROJECT_STRUCTURE). Only README.md and CLAUDE.md stay at the root. Keep one copy of each doc; don't duplicate.

**Status (2026-11-03; numbers in the revival plan §6, the obstacles plan §6 and `researchNotes/OBSTACLES_RESULTS.md`):**
- Removing tactical rewards **halves** performance (−49%).
- The revival's working configuration is now the **default**: cosine reward, reward scale 0.25, hebb 250, wrapped world, angle output wiring. Open-world alignment ≈ 0.68, with 7/16 seeds at ≥ 0.7 by 10k bars (target 14/16).
- The **meta-brain** (satiety input since 2026-10-29) still doesn't help in the open world: ablating it *raises* alignment (+0.057, CI +0.019 to +0.098). Where its output enters is obstacles-plan task O.2.
- Since 2026-11-02 the default brain is **modular** (sight + touch + reward brains → central). In the open world it ties the single brain; with walls it beats it by 29–83%, and touch is worth 47–89% of catches (all significant; OBSTACLES_RESULTS §2).
- With walls or a hidden mouse the brain still trails the memoryless `reactive` cat in 8 of 9 worlds (it beats it only with see-through pillars). It has **no memory**, and learning *shortens* how long energy stays in the brain (OBSTACLES_RESULTS §4.4). Spontaneous activity (P = 0.05) was harmful and has been **removed**; a sparse clock input replaces it as a plan (`plans/future/METRONOME_PLAN.md`). The efference copy is harmful and stays off.

The earlier "+15% meta-only" claim was an artefact. Don't repeat it, or any other performance claim not in those §6 sections or OBSTACLES_RESULTS.md.

## Commands (run from the repo root — module paths are relative)

```bash
q q/programs/cat_mouse.q --seed 42 --bars 20000 < /dev/null       # one run → results/seed42/
q q/programs/cat_mouse.q --seed 7 --bars 10000 --load-weights results/seed42/weights_seed42.bin --meta-only < /dev/null
q/tests/run_all.sh                                                  # all tests (~0.4 s); must pass
dashboards/start.sh                                                 # live viewer + a simulation at http://localhost:10001 (or `compare RUN_A RUN_B`)
scripts/run_trials.sh -l LABEL -i "1 2 3 4" -- --bars 5000          # parallel seeds → results/LABEL/s<id>/
q q/programs/analysis/compare.q results/REF results/OTHER < /dev/null        # paired comparison
q q/programs/lab/substrate_lab.q --rows 16 --cols 16 --wrap 1 --steps 3000 < /dev/null   # substrate lab (3D plan, Stage A)
scripts/experiments/world_experiment.sh 16 TAG                                 # obstacle worlds: brain arms vs scripted baselines
scripts/experiments/relearn_experiment.sh 16 TAG                               # G.4 re-learning test (CONFIGS="name=flags;...")
q q/programs/analysis/policy_ceiling.q results/ARM < /dev/null             # weights-only predicted alignment (cheap screen)
scripts/lab/lab_sweep.sh s3                                             # named lab sweep → results/lab/s3/report.txt
```

The substrate lab (`q/modules/lab/substrate.q`, `.sub`) is a configurable copy of the propagation rule, used for Phase S experiments. With `.sub.defaults` it must stay bit-identical to `.brain.propagate`; `test_substrate.q` checks this. If you change `.brain.stepNeuron`, mirror the change in `.sub.step`.

- Without `< /dev/null`, q stays at its prompt after the run, with the `simLog` table available to query.
- **Defaults:** cosine direction reward, reward scale 0.25, hebb 250, wrapped world, angle wiring, satiety meta-brain (since 2026-10-29); the modular brain and no direct reward while the mouse is hidden (since 2026-11-02). Restore the originals with `--raw-dot-reward`, `--reward-scale 1`, `--hebb 1000`, `--walled-world`, `--wiring grid`, `--single-brain`, `--hidden-reward cos`. `--cos-reward` and `--wrap-world` are still accepted (they're the default).
- Flags: `--seed --bars --output-dir --load-weights --meta-only/--no-direct-rewards --no-meta-rewards --ablate-meta --no-learning --cos-reward/--raw-dot-reward --reward-scale --pg-update --pg-eta --rpe --rpe-rate --hebb --relearn-schedule --wrap-world/--walled-world --weight-ratio --wiring --random-tie --mouse-step --field-size --two-timescale --tau-fast --tau-slow --world --see-through --blink --hidden-reward --policy --efference --single-brain --central-rows --no-touch --meta-entry --mice --evasive --distractor --layout-every --lesion --log-state --sat-edges --viz-port --viz-delay-ms --punish-calls --strategic-gate --floor --decay-mult`. Catch-rate plan: `--punish-calls N` (neutral and bad moves both get N punish calls from bar 1), `--strategic-gate N` (strategic needs N catches in 200 bars; default 3), `--floor F` (weight floor, default 25; the cap is ratio × floor, so pair it with `--weight-ratio`), `--decay-mult D` (scales the primary brain's decay loss). Brain: `--single-brain`, `--central-rows N`, `--no-touch`, `--meta-entry right|top|none` (where the reward brain's energy enters the central brain). Harder goals: `--mice N`, `--evasive`, `--distractor` (a dog to avoid). `--layout-every N` redraws random layouts during training. Probes: `--lesion R:C` (with `--no-learning`), `--log-state` (`state_log.bin`); analyse with `q/programs/analysis/structure.q`, `decode.q` and `scripts/probes/lesion_map.sh`. Obstacle worlds: `--world pillars|wall|rooms|random` adds walls that block movement and sight (not with `--walled-world`; `--see-through` keeps sight), `--blink V:B` hides the mouse for B of every V+B bars, `--hidden-reward none` drops the direct reward while it's hidden, and `--policy random|greedy|reactive|persist|planner` runs a scripted baseline (planner: shortest path around walls) in the same world (learning off). These write `world_log.csv`; analyse with `q/programs/analysis/world_report.q`. `--wiring angle|grid|random` sets which move direction each output slot drives. `--random-tie` breaks output ties at random (G.3c); `--mouse-step` and `--field-size` vary the world (G.5); `--two-timescale` (G.R5) makes weights relax towards a slow memory, saved as an 11th weights-file component. `--weight-ratio R` caps weights at R × floor (G.R4). `--relearn-schedule "A:10000,L180:5000,..."` (G.4) flicks the cat between worlds (legs/eyes rotated, goal flipped) and writes per-switch re-learning metrics; analyse with `q/programs/analysis/relearn_report.q`. `--hebb N` (G.R3) sets the unconditional per-use reinforcement (default 250; must be > 0). `--pg-update` (G.R2) makes the direct reward a policy-gradient update in log space; `--rpe` (G.R1) subtracts a running-average baseline. `--reward-scale L` (G.3b) multiplies every primary-brain reward's log-step by L. `--no-learning` disables Hebbian updates, rewards and decay while leaving propagation active. They're documented at the top of `cat_mouse.q`.
- An unknown `--` flag aborts the run.
- Interactive: `\l q/load.q`.

Theory: [THEORY.md](researchNotes/THEORY.md) models the brain as an absorbing Markov chain and learning as a reinforced urn. Its claims are tagged by evidence level, and `q q/programs/analysis/theory_checks.q < /dev/null` reproduces the verified numbers. When an experiment confirms or refutes one of its predictions (P1–P7), update the tag there.

Layout: `q/modules/{brain,world,analysis,lab,viz}/` are libraries; `q/programs/` holds `cat_mouse.q` (entry point and flag docs; the run itself is in `q/programs/cat_mouse/`: `config`, `params`, `state`, `bar_loop`, `outputs`) plus `analysis/`, `lab/` and `viz/` programs; `scripts/` has `run_trials.sh` plus `experiments/`, `probes/` and `lab/`; see [PROJECT_STRUCTURE.md](researchNotes/PROJECT_STRUCTURE.md). The exact per-bar sequence and every reward rule: see [BAR_STRUCTURE.md](researchNotes/BAR_STRUCTURE.md).

## Architecture quick reference

| | Sight brain | Touch brain | Central brain | Reward (meta) brain |
|---|---|---|---|---|
| Size | 3×12 | 3×12 | 3×12 (`--central-rows`) | 3×5 |
| Input | vision slice → top row cols 2–9, label 7 (dog: label 6) | 8 whiskers in angle order (MEDIUM: wall within 3 units; HIGH: last move that way was blocked) | sight and touch outputs → top row cols 2–9, labels 7 / 6 | satiety → top row cols 0–4: one unit a bar, col 0 starving … col 4 full |
| Output | bottom cols 2–9 → central | bottom cols 2–9 → central | bottom cols 2–9 → 8 move directions | `out[0]` scope (20/40/80 bars), `out[1]` magnitude (1/2); all 5 → central right column |
| Steps per bar | 12 (each step: sight, touch, transfer, central) | | | 24 |

Synapses everywhere: `syns[r][c]` = 8×8 int (inDir × outDir), init 25–1000, range [25, 2,000,000]. Between bars, sight, touch and central are **stacked** in one 9-row primary state (`prim`; rows 0–2, 3–5, 6–8), so rewards, history, decay and weight files treat them as one array (`q/modules/brain/modular.q`). With `--single-brain` the primary state is the original 6×12 brain.

| Term | Meaning |
|---|---|
| Bar | One decision cycle. Epoch = 2000 bars (reporting only). |
| Trinary | LOW=0i, MEDIUM=1i, HIGH=2i energy units per neuron |
| `su` | Synapse-usage flags for the current bar, the basis for all credit assignment |
| `hist` | 100-slot circular buffer of past `su`. `histIdx` = next slot to write. Use `.syn.histSlot` / `.syn.lookbackBars`. |
| `rateCounter` | Catches in the last 200 bars (gates strategic reinforcement) |
| `satiety` | Leaky food store: +1 per catch, 200-bar decay. Its level is the meta-brain's input; the meta-brain is rewarded for being full and punished for starving |
| Pressure | Energy ÷ (rows×cols×2). **In practice < 0.06, so the drain (p > 0.1) and the meta throttle (p ≥ 0.3) never engage.** |
| Brain state dict keys | `br syns su inc0 inc1 out inp rows cols inOff inLen outOff outLen` |

## Coding conventions

- **Namespaces:** every module uses the `\d .namespace` … `\d .` pattern. The namespaces are `.trinary .dirs .syn .brain .vision .engine`, plus `.mod` (modular brain), `.relearn` (G.4 worlds), `.world` (obstacle worlds, touch, scripted policies, planner), `.theory` (Markov-chain analysis), `.sub` (substrate lab), `.arm` (an experiment arm's run directories, for the analysis programs) and `.viz` (viewer). Tests use `.t`, from `q/tests/harness.q`.
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
| Weight save/load, output files, CLI flags (including Phase W worlds, blink, policies) | `test_weights.q` |
| Walls: hits, blocked moves, line of sight, wrapped copies, clear spawns; whiskers, scripted policies, planner | `test_world.q` |
| Modular brain: split/stack, transfers between brains, conservation across brains | `test_conservation.q` |
| Lab substrate (`.sub`) faithful to `.brain`, conserving under all settings | `test_substrate.q` |
| Reward or decay rules | no unit test yet. Measure behaviour with a paired multi-seed experiment (see `scripts/experiments/META_EXPERIMENT_README.md`). |

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

## Key parameters (`q/programs/cat_mouse/params.q`; flag-driven ones in `config.q`)

```q
ROWS:6i; COLS:12i              / primary brain
IN_OFF:2i; OUT_OFF:2i          / input/output column offsets (8 wide)
META_ROWS:3i; META_COLS:5i     / meta-brain
META_INPUT_OFFSET:1i           / meta output → primary right column, rows 1–5
MAX_BARS:20000i                / default; override with --bars
STEPS_PER_BAR:12i              / primary steps per bar (meta runs 2× per step)
RATE_WINDOW:200i               / catch-rate window; also the satiety decay time constant
EPOCH_SIZE:2000i               / reporting epoch
HISTORY_SIZE:100i              / su history depth
FIELD_SIZE:100.0               / --field-size; both cat and mouse wrap unless --walled-world
MOUSE_MOVE_INTERVAL:10i        / mouse steps 2.0 units every 10 bars (STEP_SIZE:5.0 is unused)
```

## What NOT to do

- **Don't trust or repeat performance claims** that aren't backed by the revival plan's or the obstacles plan's §6.
- **Don't put a design principle the user states behind an opt-in flag.** Build it into the default (updating the golden digest and the revival plan's §2), then measure it against the old behaviour. Flags are for experiments, baselines and ablations.
- **Don't remove the unconditional usage reinforcement** (the per-use Hebbian increment). It's deliberate: it lets the system retool when conditions change and be creative. Tune its *ratio* to the directed (reward) reinforcement instead (REVIVAL_PLAN.md §2 point 7, task G.R3).
- **Don't change `.brain.stepNeuron` without checking `.mod`:** the modular brain runs it on each brain separately; a change must keep `test_conservation.q`'s cross-brain conservation passing.
- **Don't resize the meta-brain casually.** Fortran-era notes blamed sizes above 3×5 for energy accumulation. Propagation itself conserves energy at larger sizes (tested), so any accumulation would come from the overwriting inputs (F16). Run the tests and watch `brainEnergy` if you resize.
- **Don't use a bare `/` on its own line**, and don't wrap `$[...]` conditions in `[]`.
- **Don't use `each` where a fold is needed.**
- **Don't commit `results/` or weight files** (`*.bin`, `*.bin#`); both are gitignored.
- **Don't run qlint on a whole module.** Lint single functions (see Tooling).
- **Don't add hard-coded values** (user, 2026-11-03: "there should never be hard-coded values anywhere"). A new tunable number is a flag or an environment variable with its default documented, not a literal buried in the code; give structural constants a name and a comment. Existing literals (about 200) are the subject of `plans/future/PARAMETERS_PLAN.md`; don't add to the pile while it waits.
- **Don't commit unless the user asks.**
