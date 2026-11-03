# Plan: a viewer that builds intuition (dashboard uplift)

**Status (2026-11-03):** not started. Decision 1 answered (§8): **build in KX Dashboards with its Vega Chart component, after a spike**, falling back to a purpose-built page only if the spike fails. Next: U0.1 (the Vega spike). **All §8 decisions answered** (2026-11-03): dark theme; old views retired at parity; `--viz-record` built; first release U0–U4.

**Why it exists.** The user (2026-11-03): "the dashboards as it stands isn't equipped to be able to show me what it needs to show me in order to give me an intuition here. It's way too simplistic. It needs a massive uplift. … It would also be nice if it was more just graphically pleasing as well. … the cat and the mouse and everything else is just a dot. Activity, that needs to be a whole lot more well developed. I can't even see input or output factors here."

The obstacles plan's I.5 ("watch it") can't be done with today's viewer, and neither can the vision plan's checks by eye. This plan is the prerequisite for both.

**How to use this document:** the same rules as the other plans. Tasks are checkboxes (`[ ]` not started, `[~]` partly done, `[x]` done and verified). Tick only after the acceptance check passes, with the date. Every session appends to the Progress Log (§10). Decisions in §8 belong to the user.

---

## 0. Goal

Watching a run should answer, by eye, the questions the experiments ask:

| Question | What has to be on screen |
|---|---|
| Does the cat head for the mouse, and why does it sometimes not? | The vision slice that's lit, the energy it injects, where that energy goes, which output slot wins, the move against the true direction |
| What does the brain do when the mouse disappears? | Visible / hidden state, where the mouse really is and where it was last seen, the brain's activity in the hidden bars (today: it drains within the bar) |
| Why does it get trapped against walls? | Walls, whiskers, the blocked sense lighting up, the repeated blocked moves |
| What is the reward brain doing? | Satiety (starving … full), the reward brain's energy, its outputs (scope, magnitude) and where its energy enters the central brain |
| What is learning changing? | Which routes were used this bar, which rewards were applied (reinforce / punish / strategic / catch boost / decay), and how the route map evolves |

And it should be pleasant to look at: a designed screen with drawn animals, walls and flows, not scatter-chart dots.

**The hard constraint, unchanged from the original viewer:** with the viewer off, a run is byte-identical (the golden digest and `test_viz.q`). The viewer never touches the RNG or the learning path.

## 1. What the viewer shows today, and why it stops there

**Today** (`dashboards/catbrain.json`, built by `dashboards/build_dashboard.py`; data from `q/programs/viz/viz_server.q`):
- **Energy matrix:** the 9×12 stacked brain as 108 square bubbles coloured by level (0 / 1 / 2). Sight, touch and central brains are not separated or labelled; no input or output marking.
- **Reward-brain matrix:** 3×5, same style.
- **Field:** cat and mouse as two dots, 60-bar trails as dots, 8 "gaze" dots for the lit vision slice. No walls, no dog, no extra mice, no visibility.
- **Two curves:** 200-bar catch rate and 100-bar alignment.
- **A text grid:** bar, step, catches, rate, alignment, energy, action, slice.
- **Compare dashboard:** two replayed runs, each with a final-weights route map, field and the two curves.

**Root cause 1: the simulation publishes almost nothing.** `q/modules/viz/viz.q` sends:
- per step: the stacked energy grid and the reward-brain grid;
- per bar: mouse and cat positions, vision slice, brain energy, action, catches, alignment, pressure.

**Not published at all:**
- **Inputs:** the sight input (8 slices, which column it lights), the touch input (whiskers, blocked direction), the satiety input cell.
- **Outputs:** the central outputter (8 slots, accumulated over the bar), the reward brain's outputs, scope and magnitude.
- **Flow:** the incoming-direction label of each unit (`inc0` / `inc1`, i.e. which way energy is moving), the units passed from sight and touch into the central brain, the units lost at full cells.
- **World:** the wall layout (and redraws), visibility, blink phase, blocked moves, the dog, extra mice, true and last-seen mouse positions while hidden.
- **Learning:** which routes were used (`su`), which reward was applied and how strongly, decay, the weights as they change.
- **Replay** has even less: `simulation_log.csv` holds no per-step energy, so the compare dashboard can't show activity at all.

**Root cause 2: the drawing tool.** Everything is drawn with one KX Dashboards widget, the ChartGL scatter (bubble) layer, because its heatmap layer didn't work (completed dashboards plan, V0.2). A scatter layer can only draw dots, so the cat, the mouse, the gaze and each brain cell are dots. Arrows, wedges, walls, sprites, module outlines, animation between steps and event flashes aren't available in what we've used. Dashboards also polls at 0.25 s, so a bar's 12 steps can't be played smoothly.

## 2. Approach (decision 1, answered 2026-11-03)

**Build the uplift inside KX Dashboards, drawing with its Vega Chart component, after a spike proves it.**

What the check of the install found (U0.2):
- **Vega Chart** (`ChartVega`, component version 4.7.7) takes a **full Vega specification**, not just Vega-Lite: the bundled example dashboards use Vega signals, data transforms (`filter`, `contour`) and `path` marks.
- Vega is a drawing grammar. It has the marks the screen in §3 needs:
  - `rect` for walls, cells and module outlines;
  - `symbol` with custom SVG paths for drawn sprites;
  - `arc` for the vision wedge;
  - rotated `symbol`/`path` arrows for flow direction and heading;
  - `rule`, `line` and `text`, and `group` for labelled brain modules.
- **External Content** ("Embed web page") can host a purpose-built page inside a Dashboards screen, if one part can't be done in Vega.
- Also available: `PlayBack` (stream player controls), `RangeSlider`, `Datagrid`, `Gauge`, `ChartGL`/`ChartJS` for curves.

The limits, and how the design answers them:
- **Polling, not push:** about 4 refreshes a second (measured in the original plan, V0.3). Each refresh is a jump, not a tweened animation, so watching a bar's 12 steps needs the simulation slowed (`--viz-delay-ms`) or a replay played at step speed.
- **Widgets poll independently,** so two panels could show different bars. **The world and the brain are drawn in one Vega widget from one combined query** (a long table with a `layer` column that the spec splits with `filter` transforms), so what you see is always one frame.
- **Unproven:** how a Dashboards data source feeds a Vega spec (one dataset per widget, or several). The spike settles it.

```
cat_mouse.q --(async IPC, richer frames)--> viz_server.q <--(polling queries)-- KX Dashboards (Vega + curves + controls)
                                              │  ring buffers, recordings, replay, compare
recorded run (--viz-record) ──────────────────┘
```

**Fallback** (only if U0.1 fails its acceptance): a purpose-built web page served by `viz_server.q` itself (q's built-in HTTP `.z.ph` and WebSocket `.z.ws`), drawn with Canvas in plain JavaScript, either standalone in Firefox or embedded in Dashboards through External Content. Same data (§4), same screen (§3).

## 3. The screen (proposed layout)

One page, dark theme, four regions plus a timeline. Proportions are a starting point for U0.

```
┌──────────────────────────── header: run, seed, bar / step, speed, mode (live / replay / compare) ─────────────────────────────┐
│ ┌───────────── WORLD ──────────────┐ ┌──────────────────────────── BRAIN ────────────────────────────────┐ ┌── SENSES & MOTOR ─┐ │
│ │ walls, cat sprite with heading,   │ │  sight 3×12        touch 3×12           reward 3×5               │ │ sight: 8-slice    │ │
│ │ mouse (solid; ghost when hidden), │ │  ┌──────────┐      ┌──────────┐         ┌─────┐  satiety         │ │  compass, lit one │ │
│ │ last-seen marker, dog, mice,      │ │  │ ● →  ●   │      │   ●      │         │ ●   │  starving→full    │ │ touch: whiskers,  │ │
│ │ vision wedge (8 slices, lit one), │ │  └────┬─────┘      └────┬─────┘         └──┬──┘  [■■■□□]          │ │  blocked flash    │ │
│ │ line of sight (cut by walls),     │ │       └──── label 7 ────┼──── label 6 ──    │ scope / magnitude │ │ satiety gauge     │ │
│ │ whisker rays, fading trails,      │ │                    ┌────▼─────┐◄────────────┘ (right column)    │ │ outputter: 8 bars │ │
│ │ catch flash, blocked-move bump    │ │                    │ central  │  units lost at full cells ✕      │ │  winner = move,   │ │
│ │                                   │ │                    └────┬─────┘                                  │ │  compass arrow vs │ │
│ │                                   │ │                      8 output slots → move direction             │ │  true direction   │ │
│ └───────────────────────────────────┘ └──────────────────────────────────────────────────────────────────┘ └───────────────────┘ │
│ ┌──────────────────────────────────── LEARNING ─────────────────────────────────────┐ ┌──────────── NUMBERS ─────────────────┐ │
│ │ routes used this bar (overlay on the brain) · reward this bar: reinforce ×n /      │ │ catches, catch rate, alignment,      │ │
│ │ punish ×n / strategic (scope × magnitude) / catch boost / decay · live route map   │ │ visible %, blocked %, satiety,      │ │
│ │ (sharpness, dominant route) · change in log-weight per cell over the last K bars   │ │ energy, pressure, units lost         │ │
│ └────────────────────────────────────────────────────────────────────────────────────┘ └──────────────────────────────────────┘ │
│ timeline: catch rate · alignment · satiety · energy, with hidden bars shaded and event ticks (catch, blocked, bitten, layout,  │
│ world switch); scrub to any bar, play / pause / step one step / step one bar, speed, "next hidden episode", "next catch"       │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**Brain drawing.**
- **Each brain is drawn as its own module,** labelled, with its input row and output row marked. The arrows between modules follow the real wiring: sight and touch outputs feed the central top row (labels 7 and 6), and the reward brain's outputs feed the central right column (rows 1–2; the 3 outputs with no row to land in are shown as dropped).
- **A cell's look:**
  - its energy (0 / 1 / 2) as fill;
  - its incoming direction (`inc0`, and `inc1` for HIGH) as a small arrow, so flow is visible;
  - a fading glow of the last few steps, so movement reads as movement, not flicker (the original plan's "risk" note: trinary frames look blocky).
- **Each step animates** the units that moved: the step is the frame, and the animation interpolates.
- **Transfers:** units passed between brains travel along the module arrows; units lost at full cells show a brief ✕.

**World drawing.**
- **Walls:** filled rectangles.
- **Wrapping:** the wrapped field is drawn with wrap-around ghosts near the edges, so a cat crossing an edge doesn't jump.
- **Sprites:** the cat and the mouse are simple drawn vector sprites (no image files), and the cat faces its last move.
- **Vision:** a translucent 8-slice wedge with the active slice lit, and the line of sight drawn and cut where a wall hides the mouse.
- **When hidden:** the mouse turns into a ghost at its true position, with a marker where it was last seen.
- **Touch:** whiskers as short rays, lit when touching.

## 4. Data the simulation must publish (the "frame schema")

All of it is copied from values the simulation already computes, after they are computed. Nothing new is calculated inside the learning path.

| When | New fields |
|---|---|
| Run start | Schema version, run flags and seed, brain layout (rows per brain, input and output offsets, wiring permutation), field size, walls (raw rectangles), named layout |
| Layout redraw (`--layout-every`) | Walls again |
| Every step (12 per bar) | Stacked energy and `inc0`/`inc1`, reward-brain energy, the step's central output, units transferred and lost |
| Every bar | Sight input vector (which column), touch input vector, satiety level and cell, reward-brain outputs, scope, magnitude, central outputter, chosen slot, move direction and distance, visible, blink phase, blocked, true and last-seen mouse position, dog and extra mice, catch / bitten events, reward events (type and count: reinforce n / punish n / PG δ / strategic scope × magnitude / catch boost / decay this bar), routes used this bar (`su`, sparse) |
| Every N bars (setting) | Route-map summary computed from the weights (sharpness and dominant route per cell), not the full weights |

**Recording (`--viz-record FILE`).** The same frames written to a file, so replay and compare show activity, not just positions. A rough size: 108 cells × 2 arrays × 12 steps × 20,000 bars ≈ 52 M bytes before compression; a recording can be limited to a bar range or every k-th bar (settings).

All new settings (ports, rates, glow length, colours, recording ranges) follow the no-hard-coded-values rule: named, with documented defaults (plans/future/PARAMETERS_PLAN.md).

## 5. Tasks

### U0 — Spike and decisions
- [ ] **U0.1** **Vega spike.** One Vega Chart widget, fed by one combined query from `viz_server.q`, draws the world (walls, cat and mouse sprites with heading, the vision wedge) and the three brains as labelled modules with energy and flow arrows, from a live run at `--viz-delay-ms`. *Accept:* in Firefox (Playwright), the widget updates every poll without a "Loading" state or errors; the world and brain always show the same bar and step (checked against the frame counter drawn in the widget); refreshes per second and the simulation's slowdown are recorded here. *If it fails:* record why, and build the fallback (§2) instead.
- [x] **U0.2** Check KX Dashboards for components that allow custom drawing. *(2026-11-03: yes. `ChartVega` 4.7.7 takes full Vega specs (signals, transforms, path marks in the bundled examples); `External Content` embeds a web page; `PlayBack`, `RangeSlider`, `Gauge` exist. §2 has the details. The user chose Dashboards with Vega, after a spike (decision 1))*
- [ ] **U0.3** A static mock-up of the full screen (§3) with real data from one recorded bar, for the user to react to before the build. *Accept:* the user's comments recorded in §10.

### U1 — Data
- [ ] **U1.1** Publisher: the §4 schema in `q/modules/viz/viz.q`, versioned; viewer side in `viz_server.q` (ring buffers per field). *Accept:* `test_viz.q`: viewer off is byte-identical (golden), schema round-trips, a dead viewer is tolerated.
- [ ] **U1.2** `--viz-record FILE` (with bar range / every-k settings), and replay from a recording at full per-step fidelity, including compare (two recordings). *Accept:* a recorded run replays frame for frame identically to its live frames; size measured and recorded.
- [ ] **U1.3** Publishing cost measured: per-bar slowdown with the viewer on (the original publisher cost +11%). *Accept:* recorded here; if it is above an agreed limit, the per-step fields get a sampling setting.

### U2 — World view
- [ ] **U2.1** Field: walls, wrapped edges with ghosts, cat and mouse sprites (heading), trails with fade, dog and extra mice.
- [ ] **U2.2** Senses in the world: vision wedge with the lit slice, line of sight cut by walls, whiskers, hidden mouse as a ghost at its true position, last-seen marker.
- [ ] **U2.3** Events: catch flash, bitten flash, blocked-move bump, layout redraw.
*Accept (U2):* on a recorded `--world rooms --blink 20:20` run, every element checked against `world_log.csv` and `simulation_log.csv` for a set of bars (positions, visibility, blocked) by an automated test.

### U3 — Brain view
- [ ] **U3.1** The three brains and the reward brain as labelled modules with marked input and output rows, and the wiring arrows between them (§3).
- [ ] **U3.2** Cells: energy fill, incoming-direction arrows, fading glow; step-by-step animation within a bar.
- [ ] **U3.3** Transfers between brains travelling along the arrows; units lost at full cells.
- [ ] **U3.4** Single-brain runs (`--single-brain`) drawn as one 6×12 module.
*Accept (U3):* energy per brain on screen equals the frame's sums every step; units conserved on screen (in brains + output + lost) as `test_conservation.q` asserts in code.

### U4 — Senses and motor panel
- [ ] **U4.1** Sight input as an 8-slice compass (the lit slice and the column it lights, with the eye permutation under `--relearn-schedule`).
- [ ] **U4.2** Touch input (whiskers, blocked direction), satiety gauge (starving … full, with the input cell), the reward brain's outputs and where they enter.
- [ ] **U4.3** Central outputter as 8 bars accumulating over the bar's steps; the winning slot, its move direction (wiring) and distance; the move against the true direction to the mouse.
*Accept (U4):* every value shown equals the frame's value for a set of recorded bars (automated).

### U5 — Learning view
- [ ] **U5.1** Routes used this bar drawn over the brain (from `su`).
- [ ] **U5.2** Reward events per bar (type, count, scope × magnitude) on the brain and in the timeline.
- [ ] **U5.3** Live route map (sharpness, dominant route) every N bars, and the change in log-weight per cell over the last K bars.
*Accept (U5):* the route map from the live stream equals `.viz.routesFrom` on the saved weights at the end of the run.

### U6 — Timeline and controls
- [ ] **U6.1** Curves: catch rate, alignment, satiety, energy; hidden bars shaded; event ticks.
- [ ] **U6.2** Controls: play / pause, one step, one bar, speed, scrub to any recorded bar, jump to the next hidden episode / catch / blocked run / world switch.
*Accept (U6):* scrubbing to bar b shows exactly the recorded frame b (automated).

### U7 — Compare mode
- [ ] **U7.1** Two recordings side by side, synchronised by bar, every view in both. *Accept:* the compare test passes with per-step activity on both sides.

### U8 — Finish
- [ ] **U8.1** Visual design pass: a consistent palette (legible, colour-blind safe), typography, legends, a short on-screen key.
- [ ] **U8.2** Firefox end-to-end tests (Playwright, as today) for live, replay and compare; `dashboards/start.sh` starts the new viewer; README and PROJECT_STRUCTURE updated.
- [ ] **U8.3** Retire or keep the KX Dashboards views (decision 3), and move the obstacles plan's I.5 to "ready to watch".

## 6. Order and size

U0 first (a day-sized spike and the mock-up, so the user can say yes to the look before the build). Then U1 (the data, which everything else needs), then U2–U4 (what the user asked for first: world, activity, inputs and outputs), then U5–U7, then U8. U1–U4 are the bulk of the work.

## 7. Risks

- **Observer effect.** More data per step costs time with the viewer on. It is opt-in and measured (U1.3); off stays byte-identical.
- **Too much on one screen.** §3 is dense. U0.3's mock-up is where to cut. Views can be toggled.
- **Looks like evidence.** The viewer builds intuition; claims still need paired multi-seed experiments (the rule in every plan).
- **Vega inside Dashboards is unproven here** (the ChartGL heatmap once stuck on "Loading"). U0.1 is a gate: the plan doesn't build further on Vega until it passes.
- **Big specs.** The world-plus-brain spec will be large; it is generated by `dashboards/build_dashboard.py` like today's dashboards, never hand-edited in the Dashboards editor.
- **A second front end to maintain,** only if the fallback is needed. Then retire the old views once the new one has parity (decision 3).
- **Recordings are large** at full fidelity. Ranges and sampling settings (U1.2).

## 8. Decisions for the user

1. **Drawing technology.**
   - (a) A purpose-built web page served by the viewer process (§2).
   - (b) Stay on KX Dashboards and push its widgets further (U0.2 checks whether a custom HTML/canvas widget exists).
   - (c) A desktop window (for example Python with pygame) reading the same stream.

   **ANSWERED 2026-11-03: (b), with Vega Chart, after a spike.** U0.2 found that Dashboards' Vega component takes full Vega specs, so drawing isn't limited to dots after all. The user's question was whether something adequate could be built in Dashboards; the answer was "likely yes, at about 4 refreshes a second without smooth animation, if the world and brain share one widget". (a) remains the fallback if U0.1 fails, possibly embedded through External Content.
2. **JavaScript libraries** (only if the fallback is built). Plain JavaScript and Canvas, no build step, with any library copied into the repo (no internet needed at run time). **Recommendation: plain JavaScript.** **ANSWERED 2026-11-03: not applicable** (the user: "null because we're only doing dashboards"); revisit only if the fallback is built.
3. **The KX Dashboards views.** Keep them alongside, or retire them once the new viewer reaches parity. **Recommendation: retire at U8,** to have one viewer to maintain. **ANSWERED 2026-11-03: retire today's Cat Brain views once the new Vega views reach parity (U8).**
4. **Recording.** `--viz-record` at full per-step fidelity, opt-in, with range and sampling settings. **Recommendation: yes;** replay and compare without activity is half of today's problem. **ANSWERED 2026-11-03: yes.**
5. **Look.** A dark theme with drawn vector sprites (cat, mouse, dog), or a lighter, diagram-like look. **Recommendation:** decide on the U0.3 mock-up rather than in the abstract. **ANSWERED 2026-11-03: dark theme** ("dark theme is cool"); details on the U0.3 mock-up.
6. **Scope of the first release.** U0–U4 (world, activity, inputs and outputs, with recording) as the first thing you use, then U5–U8. **Recommendation: yes.** **ANSWERED 2026-11-03: yes.**

## 9. Results

None yet.

## 10. Progress Log

### 2026-11-03 — plan written
- From the user's review of the dashboards: too simplistic, everything a dot, no inputs or outputs, activity under-developed, not pleasing to look at.
- Found two root causes: the simulation publishes very little (no inputs, outputs, flow directions, walls, visibility, satiety, rewards or route usage; replay has no per-step activity), and everything is drawn with one scatter-chart widget.
- Proposed: a purpose-built web viewer served by the viewer process (HTTP + WebSocket), a richer versioned frame schema with recording, and views for the world, the brain's modules and flow, senses and motor, learning, a timeline and compare; spike and mock-up first.
- Nothing built. Waiting on §8.

### 2026-11-03 — decision 1 answered: Dashboards with Vega, after a spike
- The user asked whether something adequate could be built in KX Dashboards. Checked the install (U0.2): `ChartVega` takes full Vega specs, `External Content` embeds a web page. Answer given: likely yes, at polling speed (about 4 refreshes a second, no smooth animation), with world and brain in one Vega widget so they never disagree.
- The user agreed. §2 rewritten around Vega; U0.1 is now the Vega spike, with the purpose-built page as the fallback.

### 2026-11-03 — remaining decisions answered
- 2: not applicable (Dashboards only). 3: retire today's views at parity. 4: build `--viz-record`. 5: dark theme. 6: first release U0–U4. The plan is ready to start at U0.1.
