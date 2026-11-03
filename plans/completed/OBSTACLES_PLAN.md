# Plan: obstacles, and finding a mouse the cat can't see

**Status (2026-11-03): completed.** The close-out batch ran in full (113 arms × 16 seeds), with the lesion map, structure and decoding probes. **All results, verdicts on what to keep, and what to do next are in [researchNotes/OBSTACLES_RESULTS.md](../../researchNotes/OBSTACLES_RESULTS.md).** In short: touch and the modular brain are clear wins with walls; the brain still has no memory (learning removes carry-over) and trails the memoryless reactive cat in 8 of 9 worlds. Open at completion: I.5 (watching, waits on the viewer plan) and §7 decision 5 (speed, not needed to finish).

Before completion (2026-11-03): the active plan, paused mid close-out at the user's request (runs had become too slow to be useful on one machine). The modular brain, the decisions of §7 and all the code for the remaining tasks are built and tested; what's missing is mostly measurement. To resume: `scripts/experiments/obstacles_closeout.sh` (resumable; ~2 h on 16 cores; it skips the open, blink 20:5 and blink 20:20 arms already done), then analyse with `world_report.q`, `structure.q`, `decode.q` and `scripts/probes/lesion_map.sh`.

Earlier status (2026-10-29): the active plan. It takes over from the [revival plan](../completed/REVIVAL_PLAN.md) (now completed), whose Phase W (harder worlds) and Phase I (internal-structure probes) it continues. The world code, the scripted baselines and a first pilot are already built and measured (§6).

- **Next:** the user's choice among the follow-on plans (vision, viewer, parameters, 3D); the write-up's §7 lists what the results say to do first.
- **Walls are invisible in every result here** (they block sight but send nothing to the eye). Letting the cat see them without flooding its input is [plans/future/VISION_PLAN.md](../future/VISION_PLAN.md) (written 2026-11-03, waiting on the user's decisions); its winner would mean re-running the walled-world arms.

**Read first:** the revival plan's §2 (design intent, points 1–9), §3 (findings F1–F22) and §6 (results). The 3D follow-on is [plans/future/3D_PLAN.md](../future/3D_PLAN.md).

**How to use this document:** tasks are checkboxes (`[ ]` not started, `[~]` partly done, `[x]` done and verified). Tick `[x]` only after the acceptance check passes, with the date and what was verified. Every session appends to the Progress Log (§8). Decisions in §7 belong to the user: ask, don't choose. Every claim needs a paired experiment (≥ 16 seeds, `compare.q` or `world_report.q`); never conclude from one seed.

---

## 0. Goal

> **The user (2026-10-29):** "we really want to see training get really efficient. We also want internal structure to form. But also, we really need to expand the testing regime. The inputs and outputs here are very basic. We need something more trying. … at first, we could do something simple, like put obstacles in the environment, or walls, and see if the cat can come up with some strategies to find the mouse even when it can't 'see' it."

**Why these goals are one goal.** In the open world, a memoryless lookup table (vision slice → move) is optimal, so nothing rewards internal structure. A world where the mouse can be hidden is the smallest change that *needs* state (where the mouse was last seen) or a search routine. "Efficient training" becomes measurable per rung, as bars to criterion against scripted baselines. Internal structure becomes measurable with the Phase I probes.

**Carried from the revival plan: the open-world north star** (alignment ≥ 0.7 in ≥ 14/16 seeds by 10k bars) is **not yet met**: 7/16 with the default brain, 11/16 with the meta-brain ablated (revival §6). Decision 3 below is the most direct lever.

## 1. Design intent that applies here (revival plan §2)

- Core mechanism unchanged: usage-weighted probabilistic routing of conserved energy, turn reinforcement, decay (points 1–2, 6).
- **Unconditional usage reinforcement stays**; tune its ratio, never remove it (point 7).
- **A brain always receives input, from the world and from itself** (point 8).
- **The meta-brain's input is a hunger-to-full scale**, starving entering on the left and full on the right; its motivation is to be full (point 9). Built into the default, not a flag.
- **User-stated principles go into the default model, not behind flags** (2026-10-29). Flags are for experiments, baselines and ablations.

## 2. What exists (built 2026-10-29)

- `q/modules/world/world.q` (`.world`): layouts `none pillars wall rooms random` (walls 4 units thick, axis-aligned, wrapped copies), slab-method segment hits, blocked moves (stop 0.05 short of the wall, no sliding), line of sight, clear spawns. `q/tests/test_world.q` (30 tests).
- `cat_mouse.q`: `--world LAYOUT` (walls block movement and sight; needs the wrapped world, the default), `--see-through`, `--blink V:B`, `--hidden-reward cos|none`, `--policy brain|random|greedy|reactive|persist` (scripted baselines; learning off), internal-input experiments `--efference` and `--spontaneous P`. Outputs `world_log.csv` and summary columns `visibleFrac alignVisible alignHidden blockedFrac catchTimeMed meanSatietyCell`.
- `q/programs/analysis/world_report.q` (catches per 1,000 bars, catch time, visibility, behaviour by bars since the mouse was last seen); `scripts/experiments/world_experiment.sh` (each world × brain arms × baselines).
- The satiety meta-brain (default since 2026-10-29): one unit a bar enters the meta input at its satiety cell (0 starving … 4 full); its only reward is graded on that cell.

## 3. Rules for every task

- Each rung adds one difficulty; everything else stays as in the previous rung.
- Every rung is scored against **scripted baselines run in the same world code**. Beating `reactive` (the best memoryless policy) while the mouse is hidden is the evidence that internal state is used.
- With walls, straight-line alignment stops being the goal metric. Primary: **catches per 1,000 bars** and **time to catch**; also the normalised score (cat − random) ÷ (greedy − random), or against `planner` once it exists.
- Generalisation is tested on held-out layouts (O.6).

## 4. Tasks

### World rungs (were revival W.0–W.6)
- [x] **W.0** Baseline agents and task scoring. *(2026-10-29: `--policy` built; open-world pilot: greedy 146 > reactive 126 > random 1.4 catches per 1,000 bars, as required. Still to do: unit tests of each policy's choice on hand-made cases, and a `planner` (shortest path around walls), the true ceiling with obstacles; it needs the geodesic distance field of §7 decision 1.)* *(2026-11-02: done. Policies moved to `.world.policyDir` with unit tests for each choice (test_world.q); `planner` added (`.world.distField` geodesic field on 2-unit cells, recomputed when the mouse moves; tested). Open world, 16 seeds: greedy 146 ≥ planner 144 > reactive = persist 127 ≫ random 1.6 catches per 1,000 bars; in rooms the planner catches 123 vs greedy 0.35 (smoke run))*
  - `random`: uniform direction each bar (the floor). `greedy`: the direction closest to the true line to the mouse, through walls (full information, no planning; the open-world ceiling). `reactive`: when the mouse is visible, one of the two directions bounding its slice; when hidden, random (the best memoryless policy). `persist`: as `reactive`, but while hidden it keeps its last heading chosen while the mouse was visible (one-slot memory).
- [x] **W.1** Blinking vision: object permanence with no geometry (`--blink V:B`). *(2026-10-29: V=20, B=20 measured, §6. Still to do: B ∈ {5, 50}.)* *(2026-11-03: B=5 and B=20 measured on the modular brain (§6); B=50 is in the close-out batch)* *(2026-11-03, close-out: B=5, 20 and 50 measured on the modular brain: the brain trails reactive and persist in all three (normalised 0.44 / 0.29 / 0.16 against 0.68 / 0.40 / 0.22), and moves on only 2–3% of hidden bars; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **W.1b** Internal input (revival §2 points 8–9). *(Satiety meta input: built in, default. `--efference`: measured, harmful, not kept (§6). `--spontaneous P`: energy measured (it drains within the bar, like everything else), effect on hunting not yet measured.)* *(2026-11-02: `--spontaneous` now reaches every brain of the modular architecture (0.20 → 2.34 mean units at P=0.05); its effect on hunting is in the close-out batch)* *(2026-11-03, close-out: efference: harmful, not kept; spontaneous P=0.05: −62 to −93% in every world, visible alignment ≈ 0.02 (noise swamps vision). The principle stands; a lower dose is the next test; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **W.2** Walls that block movement only (`--world pillars --see-through`): detours. *(Pilot measured, §6: every agent gets trapped against pillars.)* *(2026-11-02: decision-3 arms measured pillars and rooms on the modular brain (§6); see-through pillars, wall and random layouts with all baselines and the touch ablation are in the close-out batch)* *(2026-11-03, close-out: see-through pillars: the brain (19.5 per 1,000 bars) beats reactive (7.7) in 14 of 16 seeds, the only world where it does; touch and the modular layout carry it; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **W.3** Occlusion, **the user's target** (`--world pillars|rooms`). *(Pilot measured, §6.)* What counts as "came up with a strategy": (i) heading to where the mouse was last seen (beats `reactive` while hidden); (ii) searching when lost: re-acquire time shorter than `random`, coverage of the field; (iii) going round walls through doors (catches in `rooms` above `greedy`). *(same as W.2)* *(2026-11-03, close-out: pillars, wall, rooms, random layouts measured with every baseline. Criteria: (i) beats reactive while hidden: no; (ii) searching when lost: no (it sees the mouse on 7–26% of bars vs reactive's 17–73%); (iii) rooms above greedy: yes (1.76 vs 0.41) but far below reactive (19.6). No strategy found; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **W.4** Richer senses (§7 decision 4): touch ("whiskers": which of 8 directions is blocked within 3 units), a "mouse visible" signal, coarse distance. Needs more input cells, i.e. a wider brain or inputs injected elsewhere. *(2026-11-02: built as the touch brain (decision 4): whiskers MEDIUM, blocked direction HIGH, its own 3×12 brain feeding the central brain. Tests: whiskers (test_world.q), touch rows learn only with walls (test_weights.q). Its value (`--no-touch` ablation in each walled world) is in the close-out batch)* *(2026-11-03, close-out: touch is the clearest win: removing it costs 47–89% of catches in all 5 walled worlds (significant); [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **W.5** Harder goals: an evasive mouse (steps away when the cat is near); a distractor to avoid (a second input channel with the opposite goal, so the brain must combine inputs); several mice. *(2026-11-02: built and tested: `--evasive`, `--mice N` (nearest visible mouse), `--distractor` (a dog seen through the same input with its own label; touching it reverses the catch boost; moving towards it nearby is punished). Measurement against reactive/greedy/planner is in the close-out batch)* *(2026-11-03, close-out: three mice +29% (open) and +473% (pillars) against one mouse; the dog −67% in the open world, and the brain touches it 2.7× as often as the scripted cats (it can't tell dog from mouse). Evasive (rerun after a bug fix, with a test): a fleeing mouse costs every cat about the same (brain 78.5 → 62.8, reactive 126.5 → 101.1, greedy 146.3 → 125.7); [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **W.6** Generalisation and re-learning: train on some layouts, test on held-out ones frozen (`--no-learning`) and still learning. Layout switches become G.4 worlds (`--relearn-schedule` with layouts). *(2026-11-02: built and tested: `--layout-every N` redraws random layouts during training. The held-out test (frozen and learning, against fresh and untrained brains) and the curriculum are in the close-out batch)* *(2026-11-03, close-out: held-out layouts: fresh learning beats every pretrained brain (−28% to −42%; negative transfer); many layouts vs one ns; curriculum ns; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*

### New tasks from the pilot
- [x] **O.1** **Getting unstuck** (§7 decision 2). In the pilot, a blocked move stops dead, and any agent that keeps one heading (greedy, persist, and the brain's meta-driven drift) is trapped against a wall: the brain had 78% of its moves blocked in pillars and 97% in rooms, and caught fewer mice than `random` in rooms. Candidates: *(2026-11-02: the blocked sense is built into the touch brain (decision 2/4); acceptance (catches above random in pillars and rooms, blocked fraction falling) not yet measured with baselines. Decision-3 arms: modular brain in rooms 1.76 catches per 1,000 bars vs random 0.89 in the pilot)* *(2026-11-03, close-out: acceptance met: catches above random, paired, in pillars (+79 per run, CI +52 to +110) and rooms (+8, CI +3.6 to +12.3); blocked fraction down from 78% to 67% (pillars) and 97% to 83% (rooms); [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
  - (a) a **"blocked" sense**: an internal input lit on the bar after a blocked move (proprioception: input from itself, §2 point 8), so the brain can learn to turn;
  - (b) **sliding** along walls (a world rule change; makes walls less of a trap for every agent);
  - (c) touch whiskers (W.4).
  - Acceptance: the brain's catches in `pillars` and `rooms` above `random`, paired, with its blocked fraction falling.
- [x] **O.2** **Where the meta-brain's output enters** (§7 decision 3). Three measurements point the same way: in the open world, ablating the meta-brain *raises* alignment (+0.057, CI +0.019 to +0.098); while the mouse is hidden its cross-flow gives a fixed drift (hidden alignment −0.15 in blink); and a random tie-break costs −0.254 alignment with the meta-brain but only −0.049 (ns) without it, because the default lowest-slot tie rule makes the cross-flow's drift (which drains to the highest slot) lose every tie (revival §6 "G.3(c)"). Its output enters one column (the right edge, rows 1–5) and drains to the nearest output slot. Test where it should enter (spread over rows, the free top-row cells beside vision, or nowhere directly but only through the strategic scope and magnitude). *(2026-11-02: tested on the modular brain, 16 seeds × 4 worlds: right edge kept as the default (§6, §7 decision 3))*
- [x] **O.3** **Variability when lost.** The memoryless `reactive` cat won pillars (88 catches per 1,000 bars against the brain's 6) and rooms (19 against 0.2) simply because it moves randomly when the mouse is hidden. Test whether the brain gets the same benefit from resting activity (`--spontaneous`) or a higher route temperature while hidden. Within the design: randomness comes from the substrate, not from a scripted rule. *(2026-11-03, close-out: spontaneous activity tested (P=0.05): strongly harmful. Route temperature while hidden not tested; carried to the next round in the write-up; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **O.4** **Calibrate the satiety scale.** In the open world a good chaser sits at cell 3.9 of 4 (nearly always "full"), so the meta-brain's reward barely varies. Harder worlds use more of the scale (to be measured from `meanSatietyCell`). Choose edges (`.brain.SAT_EDGES`) that spread a typical run over the scale, and re-test the meta-brain against its ablation. *(2026-11-02: `--sat-edges a,b,c,d` added (tested); the alternative 1,4,10,20 vs default is in the close-out batch)* *(2026-11-03, close-out: scale use: mean cell 3.7 (open), 2.4–3.1 (blink), about 1 (walls), 0.26 (rooms); alternative edges 1,4,10,20 ns in open and pillars. Default kept; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **O.5** **Reward while hidden** (§7 decision 1): build the geodesic "smell gradient" (option b) and the `planner` baseline from the same distance field. *(2026-11-02: resolved by §7 decision 1 (no reward while hidden, now the default). The geodesic field was built for the `planner` baseline instead)*
- [x] **O.6** The north star in harder worlds: targets per world, set from the baselines once O.1–O.3 are in (e.g. the brain beats `reactive` in blink and pillars in ≥ 14/16 seeds). *(2026-11-03, close-out: target set: beat reactive in ≥ 14/16 seeds. Met in 1 of 9 worlds (see-through pillars); the bar for the next round; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*

### Internal-structure probes (were revival I.1–I.5)
- [x] **I.1** Carry-over, exactly from the weights: expected residence time of an injected unit (fundamental matrix $N=(I-Q)^{-1}$ of `.theory.chain`) and P(still in the brain after 1, 2, 5 bars). Memory needs this well above one bar. Cheap enough to track every epoch. *(2026-11-02: `q/programs/analysis/structure.q` (exact, from `.theory.chainMod`/`chain`). Trained brains hold an injected vision unit 8–12 steps on average; under 2.3% is still inside after two bars, under 0.2% after five (§6). Memory of the kind the hidden-mouse task needs isn't there)*
- [x] **I.2** Memory decoding: in hidden bars, decode the last-seen slice from the brain state (energy per cell plus incoming directions) with a nearest-centroid classifier trained on half the seeds and tested on the other half. Report bits against bars since seen, with a label-shuffled control. *(2026-11-03: `q/programs/analysis/decode.q` with `--log-state`. Blink 20:20, 16 seeds (odd train, even test): accuracy at chance in every band (0.10–0.12; shuffled 0.10–0.14), because at the end of a hidden bar the brain holds any energy 0.1–0.3% of the time (§6))*
- [x] **I.3** Route-graph structure: from the weights, the graph of dominant routes. Count directed cycles (recurrent loops: the directed-edge reinforced walk literature says such walks get trapped in directed circuits, THEORY.md §6.1), path lengths from inputs to outputs, cells taking part, pathway overlap between slices. Track over training. The dashboards' route map (Cat Brain Compare) shows the same data visually. *(2026-11-02: in `structure.q`: trained dominant-route graphs have almost no loops (0–1.5 states of 432–576 on cycles; dominant paths reach an output in ~7–11 steps). Comparison with untrained brains is in the close-out batch (`o_untrained*` arms))* *(2026-11-03, close-out: trained vs untrained: learning cuts residence from 14–15 steps to 8–10 and loop states from 4.2 to 0.6–1.9; training removes carry-over instead of building it; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [x] **I.4** Lesion maps: freeze a trained brain, block each cell in turn, measure the change in catches and alignment. *(2026-11-02: `scripts/probes/lesion_map.sh` + `q/programs/analysis/lesion_report.q` built and smoke-tested; the full map on a trained arm is not run)* *(2026-11-03, close-out: lesion map of the 16 open-world brains (72 cells): the chase is distributed (worst cell −30% catches, −0.18 alignment), concentrated on the middle columns of the sight and central brains' lower rows; [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*
- [~] **I.5** Watch it: the live viewer and Cat Brain Compare during hidden episodes. Intuition only, never evidence. *(2026-11-02: both dashboards now show the whole 9-row modular brain (sight, touch, central) and pass their Firefox tests; watching hidden episodes not done)* *(2026-11-03: the user found the dashboards too simplistic to build intuition from (no inputs, outputs or flow; everything a dot). Watching waits on [plans/future/VIEWER_UPLIFT_PLAN.md](../future/VIEWER_UPLIFT_PLAN.md))* *(2026-11-03, close-out: deferred: the user found the dashboards too simplistic to watch (see the viewer plan); [the results write-up](../../researchNotes/OBSTACLES_RESULTS.md))*

## 5. Efficiency levers to judge here as well
Output wiring (angle is now the default); G.R5 two-timescale weights were tested and **not kept** (revival §6: they anchor the brain to the old world; a reward-gated consolidation variant is untested); curriculum (train open → pillars → rooms: does it beat training on rooms from scratch?).

## 6. Results

**The close-out (2026-11-03) and the verdicts are in [researchNotes/OBSTACLES_RESULTS.md](../../researchNotes/OBSTACLES_RESULTS.md)**, which supersedes the partial close-out table below. The sections below are the earlier results it builds on.

### Pilot (2026-10-29): `results/w_report_20261029.txt`
Brain arms ran on the then working configuration with angle wiring and the satiety meta-brain (now the default). 16 seeds × 10k bars per arm. Catches per 1,000 bars:

| World | brain | brain, meta ablated | `--efference` | `--hidden-reward none` | reactive | persist | greedy | random |
|---|---|---|---|---|---|---|---|---|
| open | 84.6 | 88.5 | 28.4 | 84.6 | 126.3 | 126.3 | 146.1 | 1.4 |
| blink 20:20 | 46.7 | 48.2 | 19.6 | 42.0 | 59.0 | **64.6** | 146.0 | 1.4 |
| pillars, see-through | 7.2 | 4.4 | 8.8 | 7.2 | 7.4 | 7.4 | 3.8 | 1.4 |
| pillars | 6.2 | 1.9 | 4.9 | 6.5 | **88.4** | 3.8 | 3.1 | 1.3 |
| rooms | 0.23 | 0.16 | 0.18 | 0.30 | **19.1** | 0.58 | 0.35 | 0.89 |

Behaviour while the mouse is hidden:

| World | Brain: moves on hidden bars | Brain: hidden alignment (bars 1–5 / 6–20 after losing sight) | Meta ablated: moves on hidden bars | Brain: blocked moves |
|---|---|---|---|---|
| blink | 40% | −0.23 / −0.13 | 0.2% | — |
| pillars | 37% | −0.20 / −0.27 | 0.02% | 78% |
| rooms | 39% | +0.05 / −0.21 | 0.01% | 97% |

**Reading:**
1. **Walls trap any fixed heading.** With no sliding, greedy, persist and the brain all end up pushing into walls (blocked 78–98%). In rooms the brain catches fewer mice than `random`. O.1.
2. **Random movement when lost beats everything else here.** `reactive`'s random moves while the mouse is hidden unstick it: 88 catches in pillars against the brain's 6, and 19 in rooms against 0.2. O.3.
3. **No evidence of memory.** The brain's hidden-bar alignment is *negative*: the meta-brain's cross-flow drives a fixed drift that is unrelated to where the mouse was (F22). Without the meta-brain it barely moves while hidden. `persist` beats `reactive` in blink (64.6 vs 59.0), so a one-slot memory would pay. I.1/I.2 will say whether the substrate can hold it.
4. **The efference copy is harmful** (open-world catches 85 → 28; alignment 0.67 → 0.27): feeding back the last motor command makes the cat repeat itself instead of following vision. Not kept.
5. `--hidden-reward none` makes little difference (blink 42.0 vs 46.7).
6. Open world: ablating the meta-brain helps (88.5 vs 84.6 catches; alignment +0.057, CI +0.019 to +0.098, from the 20k-bar paired test in revival §6). O.2.

### Modular brain: central depth (2026-11-02), `results/arch_report_20261102.txt`
Open world, 16 seeds × 20k bars, paired against the single 6×12 brain (the previous default):

| | single 6×12 | modular, central 3 rows | central 4 | central 6 |
|---|---|---|---|---|
| Alignment | 0.680 | 0.658 (−0.022, CI −0.092 to +0.051) | 0.634 (−0.046, ns) | 0.596 (**−0.084**, CI −0.157 to −0.014) |
| Catches/run | 1,702 | 1,590 (−7%, ns) | 1,490 (−12%, ns) | 1,379 (**−19%**) |
| Acquired / median bars | 16/16, 538 | 14/16, 523 | 14/16, 631 | 14/16, 1,117 |
| ≥ 0.7 at 10k bars | 7/16 | 6/16 | 5/16 | 4/16 |

Deeper central brains are slower and worse; **3 rows is the default**. In the open world the modular brain is statistically indistinguishable from the single brain (the touch brain is silent without walls).

### Decision 3: where the reward brain's energy enters the central brain (2026-11-02), `results/d3_report_20261102.txt`
Modular brain, 16 seeds; open 20k bars, others 10k. Catches per 1,000 bars (paired Δ vs right in brackets where significant):

| World | right (default) | top | none | reward brain ablated |
|---|---|---|---|---|
| open | 79.5 | 67.5 (alignment −0.087, ns) | 83.8 | 83.9 |
| blink 20:20 | 42.7 | 39.1 (alignment **−0.120**, CI −0.193 to −0.048) | 42.8 | 45.1 |
| pillars | 9.2 | **18.9** (+97 per run, CI +55 to +140) | **3.1** (−61, CI −90 to −35) | **2.3** |
| rooms | 1.76 | 1.29 | **0.28** (−15, CI −19 to −10) | **0.26** |

- With walls, the reward brain's energy is what keeps the cat moving when it can't see: without it the modular cat sits still behind walls.
- Entering beside sight (`top`) doubles pillars catches but costs alignment when the mouse is visible. No option wins everywhere; `right` stays.
- Hidden-bar alignment with `none` is positive (+0.31 in blink), but on very few moves.

### Close-out batch, worlds completed before the pause (2026-11-03), `results/obs_report_20261103.txt`
Modular default vs single brain vs baselines, 16 seeds × 10k bars. Catches per 1,000 bars:

| World | modular | single | reactive | persist | greedy | planner | random |
|---|---|---|---|---|---|---|---|
| open | 78.5 | 84.6 (ns) | 126.5 | 126.5 | 146.3 | 144.2 | 1.6 |
| blink 20:5 | 64.3 | 71.8 (ns) | 98.2 | 99.3 | 145.9 | 144.2 | 1.6 |
| blink 20:20 | 42.7 | 42.0 | 59.0 | **64.8** | 146.0 | 144.3 | 1.5 |

- While the mouse is hidden the modular cat moves on only 2–3% of bars (single brain ~40%): the reward brain's energy enters 2 rows of the 3-row central brain's edge (5 rows in the single brain), so there is less drive when blind. Its hidden-bar moves are no better than chance (alignment +0.08 in the first 5 bars, −0.33 after).
- `persist` (a one-slot memory) beats `reactive` in blink 20:20 (64.8 vs 59.0): memory would pay. The brain has none (I.1, I.2).

### Internal structure (I.1–I.3), `q/programs/analysis/structure.q` and `decode.q`

| Arm (16 seeds) | residence (steps) | still inside after 1 bar / 2 bars / 5 bars | states on loops | dominant path to output (steps) |
|---|---|---|---|---|
| single, open 20k | 8.0 | 2.9% / 0.002% / ~0 | 0.13 | 7.8 |
| modular c3, open 20k | 8.2 | 5.9% / 0.07% / ~0 | 0.25 | 8.0 |
| modular c6, open 20k | 11.9 | 30% / 0.23% / ~0 | 0.31 | 11.3 |
| modular, pillars 10k | 8.4 | 8.1% / 0.6% / 0.04% | 1.5 | 7.2 |
| modular, rooms 10k | 10.3 | 20% / 2.3% / 0.19% | 1.0 | 7.0 |
| modular, blink 10k | 7.9 | 1.8% / 0.07% / ~0 | 0 | 7.8 |

Decoding the last-seen slice from the end-of-bar brain state (blink 20:20, `o_blink20_brain`): chance in every band (0.10–0.12 vs shuffled 0.10–0.14); the brain holds any energy at the end of only 3.1% of visible bars and 0.1–0.3% of hidden bars. **The substrate drains within a bar, so it can't hold where the mouse was** — the 3D plan's Stage A (a sealed, self-sustaining substrate) is the route to memory. Walled worlds push carry-over up a little (rooms 2.3% after two bars), the only sign of structure forming for the hidden task so far.

## 7. Decisions for the user

1. **Reward while the mouse is hidden** (was revival decision 7). **ANSWERED 2026-11-02: (c) no directed reward while hidden** — now the default (`--hidden-reward cos` restores the old one). The geodesic "smell gradient" reward is not built; the same distance field drives the `planner` baseline. (a) Straight-line cosine, as now (it rewards pushing into a wall when the mouse is behind it); (b) **geodesic progress**: the drop in shortest-path distance around walls ÷ move length, a "smell gradient" that equals the cosine in the open field; (c) no directed reward while hidden (`--hidden-reward none`). **Recommendation:** (b) as the main arm, (c) as the hard arm. The pilot found (a) vs (c) makes little difference so far, because the cat is mostly stuck.
2. **Getting unstuck (O.1):** a "blocked" internal sense, sliding along walls, or touch? **ANSWERED 2026-11-02: the "blocked" sense.** Built into the touch brain (decision 4): the cell for the direction of a blocked move lights HIGH on the next bar. **Recommendation:** the "blocked" sense first: it is input from itself (§2 point 8), keeps walls a real obstacle, and is a one-cell input. Test sliding as a world variant only to see how much of the problem it removes.
3. **Where the meta-brain's output enters (O.2).** Today it does two things: it sets the primary brain's strategic reinforcement (scope and magnitude), *and* it injects energy into the primary brain's right column, which drains to one output slot and pushes the cat one fixed way. Options: (a) keep the injection where it is; (b) inject it where vision enters (the free top-row cells beside vision), so it flows through the learned routes; (c) no direct energy at all: the meta-brain acts only by setting the primary brain's reward. The user's description ("it'll flow through into the output, and then that'll determine some reward structure") reads like (c), but (c) removes one of the two links between the brains, so it's the user's call. **Recommendation:** run (a), (b) and (c) as paired arms in the open and hidden-mouse worlds, then decide; the satiety input and reward stay exactly as designed in every arm. **ANSWERED 2026-11-02: test all three, make the winner the default.** Result (§6): no single winner. `none` is ruinous with walls (the reward brain's energy is the only drive when the cat can't see); `top` doubles pillars catches but costs precision when the mouse is visible; **`right` stays the default** (never significantly worse in catches except against `top` in pillars). Flag `--meta-entry right|top|none`.
4. **Senses and brain layout for W.4** (was revival decision 8). **ANSWERED 2026-11-02 (user): "touch should be an entirely new brain. There should be a reward brain, a sight brain and a touch brain, that all feeds into a central brain, before its output is actually translated into action."** Built as the default architecture (`q/modules/brain/modular.q`): sight 3×12 and touch 3×12 feed a central brain whose output moves the cat; the reward brain (satiety meta-brain) feeds the central brain. Central depth chosen by experiment: 3 rows (§6). `--single-brain` keeps the original as a baseline.
5. **Speed** (raised 2026-11-03 by the user: "getting to be way too slow to be useful, at least on a single machine"). The modular brain costs ~1.2 ms a bar (≈12 s per 10k-bar run), the planner baseline ~5 ms a bar; a full close-out is ~110 arms × 16 seeds ≈ 2 h on 16 cores. Options: (a) the bit-identical propagation speed-ups (3D plan C.0, was P.5a/b: in-place state, precomputed neighbours), expected 15–25% per bar; (b) the synchronous vectorised step (3D plan C.1, a semantic change), possibly 10×+; (c) spread `run_trials.sh` over several machines (runs are independent); (d) fewer seeds or bars per screening arm, keeping 16 seeds for confirmations. **Not needed to finish this plan** (the batch ran in about 2 h); still open for the next round of experiments.

## 8. Progress Log

### 2026-10-29 — plan created from the revival plan's Phase W and Phase I
- Carried W.0–W.6 and I.1–I.5 (W.7, the 3D rung, went to the 3D plan), revival decisions 7, 8 and 10, and finding F22.
- Added the pilot results (§6) and tasks O.1–O.6 that came out of them.
- Close-out results that bear on this plan: the random tie-break result strengthens O.2 (above); G.R5 not kept; G.5 shows the open-world learning generalises across mouse speed and field size.

### 2026-11-02/03 — Session: finish the plan (user request), paused for speed
- User decisions: 1 no reward while hidden (default); 2 the blocked sense; 3 test all three entry points; 4 **a modular brain: sight, touch and reward brains feeding a central brain**.
- Built: `q/modules/brain/modular.q` (split/stack/transfer; stacked state keeps every reward, history, decay and save rule unchanged), modular default with `--single-brain`, `--central-rows`, `--no-touch`, `--meta-entry`; touch whiskers + blocked sense; scripted policies in `.world` with unit tests; `planner`; W.5 `--mice --evasive --distractor`; W.6 `--layout-every`; O.4 `--sat-edges`; I.2 `--log-state` + `decode.q`; I.4 `--lesion` + `scripts/probes/lesion_map.sh` + `lesion_report.q`; I.1/I.3 `structure.q` (`.theory.chainG/chainMod/policyG/carry`; theory_checks output unchanged); policy_ceiling reads modular brains; viewer and dashboards show the 9-row brain (both Firefox tests pass).
- Verified: `--single-brain` is byte-identical to the previous default (seed 1, 20k bars); golden updated to `3e7bda69…`; all tests pass (test_weights 106, test_world 48, test_conservation 39 incl. cross-brain conservation).
- Measured: central depth (3 rows best, ties the single brain in the open world), decision 3 (right stays), open/blink worlds with all baselines, I.1–I.3, I.2.
- **Paused** at the user's request (too slow on one machine): the close-out batch was stopped after three worlds; `scripts/experiments/obstacles_closeout.sh` resumes it. Speed is §7 decision 5.

### 2026-11-03 — vision plan split off
- The user raised that a cat sees the whole world, and that presence-coded walls would light every input. Planned separately in `plans/future/VISION_PLAN.md`; this plan's walled-world results are its "walls invisible" reference.

### 2026-11-03 — close-out completed; plan finished
- Ran the whole close-out batch from a frozen snapshot (113 arms × 16 seeds, about 2 h on 16 cores), then the lesion map (open-world brains, 72 cells × 16 seeds), structure (trained vs untrained) and decoding (pillars).
- Found and fixed a bug: with one mouse, `--evasive` overwrote each respawn with the old position next to the cat (greedy "caught" 985 per 1,000 bars). Fixed in `bar_loop.q` with a regression test in `test_weights.q` (fails before, passes after); the four evasive arms rerun; the invalid outputs kept as `*_buggy`.
- Wrote `researchNotes/OBSTACLES_RESULTS.md`: per-element verdicts across every world (keep / keep as an option / not proven / not at this setting / remove), with the contexts in which a weak result may still matter. No default changes are implied; the only removal candidate (`--efference`, already off) waits for the user.
- Ticked W.1–W.6, O.1, O.3, O.4, O.6, I.3, I.4. I.5 deferred to the viewer plan. Plan moved to `plans/completed/`.
- After completion (2026-11-03, user): `--spontaneous` removed (code, tests, close-out arm, docs); a sparse clock input replaces it as `plans/future/METRONOME_PLAN.md`.
