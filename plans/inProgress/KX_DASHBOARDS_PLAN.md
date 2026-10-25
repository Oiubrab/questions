# Plan: live visualisation in KX Dashboards

**Goal:** watch the cat learn. Two live views: (1) the 6×12 network energy flowing through the matrix, and (2) the cat chasing the mouse. This serves the north star in CLAUDE.md. Today the only way to see whether the cat heads towards the mouse is to read CSV summaries after a run.

## 1. What happened to the old visual component

- The Fortran-era visuals lived in `visualization/` (pygame `cat_mouse_gui.py`, about 295 lines, plus matplotlib scripts: `visualize_brain.py`, `visualize_dual_brain.py`, `analyze_brain_pathways.py`, `brain_summary.py`, `detect_oscillation.py`) and `scripts/run_gui.sh` / `single_trial_gui.sh`.
- They were deleted in the q port, commit `aab6bff`. They're still in git history: `git show 'aab6bff^:visualization/cat_mouse_gui.py'`. Commit `3c2cd73` was their last update.
- **Not usable as-is.** They were written against the Fortran program's output, and nothing in the q tree calls them. The GUI was a toy: cat, mouse, a vision line and a text readout. It never showed per-step energy flow.
- REVIVAL_PLAN.md item 7.2 ("Visualisation of `simLog` trajectories") is the existing placeholder for this work. This plan supersedes it. Tick 7.2 when Phase V1 passes.
- Nothing from the old tools is worth porting except layout ideas. Rebuild on the q data model.

## 2. What the q sim already exposes

| Need | Source | Granularity |
|---|---|---|
| Cat/mouse position, vision slice, outAction, catches, brainEnergy | `simLog` row, appended at the end of each bar (`cat_mouse.q`, "APPEND TO LOG TABLE") | per bar |
| 6×12 trinary energy matrix | `prim`br` (also `metaBr`br`, 3×5) | available after each `.engine.runCycle` call, 12 per bar (24 for the meta-brain) |
| Synapse weights | `prim`syns` (8×8 per neuron) | per bar, large (72×64 ints) |
| Route usage | `prim`su` | per bar |

**Gap:** the within-bar energy flow, the thing you want to see moving, is **not logged**. `simLog` records only the summed `brainEnergy` per bar. The step loop at `cat_mouse.q` (`while[step<STEPS_PER_BAR; prim: .engine.runCycle[prim]; …]`) is the hook point.

**Constraints:**
- A q process is single-threaded. The sim is a blocking top-level `while` loop, so a Dashboards client cannot query the same process mid-run.
- The sim must stay bit-identical with the viz off. Visualisation must not touch RNG use or the learning path.

## 3. Architecture

Two processes, with the sim publishing asynchronously:

```
cat_mouse.q  --(async IPC, neg[h])-->  viz process (q/programs/viz_server.q)  <--(WebSocket/IPC)--  KX Dashboards
 (opt-in --viz-port)                   holds latest frame + ring buffers
```

- **Sim side:** a new `.viz` module (`q/modules/viz.q`) with `.viz.init[port]` and `.viz.pubStep[bar;step;br]` / `.viz.pubBar[row]`. It is a no-op unless `--viz-port` is passed, and it is called from the step loop and after the `simLog` append. Publishing is fire-and-forget, so a slow or absent viewer never blocks the sim. If the connection drops, disable publishing and carry on.
- **Viz process:** holds
  - `energyNow`: long-format table `(row;col;energy)`, 72 rows, overwritten each frame;
  - `energyHist`: ring buffer of the last N frames, for scrubbing;
  - `catMouse`: latest position plus a trail of the last N bars;
  - `stats`: rolling catch rate, alignment and brain energy.
  
  It serves these to Dashboards and pushes updates to subscribers.
- **Dashboards side:** a kdb+ data source pointing at the viz process. The 72-cell matrix is long-format so heatmap/scatter views can consume it directly.
- **Playback mode:** the viz process can also replay a finished run from `results/<label>/s<id>/simulation_log.csv`. Per-step frames only exist if the run was recorded with the publisher on (or a `--viz-record` file). Replay gives deterministic demos and lets us debug the dashboard without the sim running.

### Frame pacing
The sim runs thousands of steps per second, far faster than eyes can follow. The viz process should therefore:
- keep only the latest frame for "live" display, sampled at roughly 10–30 Hz by a `.z.ts` timer;
- keep the ring buffer for scrub and replay;
- offer a **slow-motion** option in the sim (`--viz-delay-ms`, sleeping between steps only when viz is on) for watching one bar's 12 steps flow.

## 4. Views

1. **Energy matrix (priority):** 6×12 heatmap, colour = trinary energy (0/1/2). Mark input cols (2–9, top row) and output cols (2–9, bottom row). Overlay the meta-right-column injection (col 11, rows 1–5). Add a 3×5 meta-brain panel with the same style.
2. **Cat–mouse field:** 100×100 scatter with cat, mouse and a trail of the last N bars. The mouse wraps (toroidal) and the cat is clamped, so draw the wrap correctly and don't connect trail segments across a wrap. Show the active vision slice as a wedge or a coloured axis.
3. **Learning curves:** rolling catch rate (the 200-bar `rateCounter`), alignment (the move-alignment window, `alignment` in `simLog`), and towards/away/perp move counts per epoch. These are the north-star measure from REVIVAL_PLAN §0, so they must be on screen next to the animation.
4. **Optional later:** synapse-strength heatmap per neuron (8×8 in/out directions) with route-usage highlighting. Useful for the "what distinguishes self-sustaining brains?" question (§7, decision 4), but a large payload, so refresh it per bar at most.

## 5. Phases

### V0 — Feasibility spike
- [x] **V0.1** *(2026-10-25)* Dashboards is installed at `~/.kx/dashboards`, `kc.lic` is present and `q` runs. `q dash.q -p 10001 -u 1` serves HTTP 200. The gateway talks to a q process over plain IPC (connection JSON in `data/connections/`: `dbtype q`, host, port). It supports polling queries and `.u.sub`/`.u.snap` streaming, and the bundled `sample/demo.q` (ring buffers + `.u.pub`) is the pattern to follow. Heatmap views are used in the bundled demo dashboards. Auth is off (`-u 1` file-based users, none configured).
- [x] **V0.2** *(2026-10-25)* Matrix and field both render in Firefox. Server: [q/programs/viz_spike.q](../../q/programs/viz_spike.q) (port 6814). Dashboard: generated by [dashboards/build_dashboard.py](../../dashboards/build_dashboard.py), installed by `dashboards/install.sh` (restart the gateway to pick up changes). The ChartGL `Heatmap` layer stayed on "Loading" even though the data arrived, so the matrix is one square-bubble layer coloured by a `lvl` category (`l0`/`l1`/`l2` = energy 0/1/2). Off-grid dummy rows keep all three categories present each frame, which keeps the colours stable. Test: `scripts/e2e_dashboards.sh` (7 Firefox checks, all pass).
- [x] **V0.3** *(2026-10-25)* Polling works at sub-second intervals: with `_subscriptionInterval` 0.25 the matrix changed about twice a second in a 6 s Firefox capture (1 s gave 5 changes in 6 s). Polling is enough, so streaming via `.u.sub` is optional and not needed for V1/V2.

### V1 — Publisher and replay
- [x] **V1.1** `q/modules/viz.q` with `.viz.init`, `.viz.pubStep`, `.viz.pubBar`. Namespace pattern per CLAUDE.md. *(2026-10-25)* Also `.viz.send`/`.viz.close`; a dead viewer turns publishing off instead of stopping the run. `pubBar` groups its values because q lambdas take at most 8 arguments.
- [x] **V1.2** `--viz-port` (and optionally `--viz-delay-ms`) in `cat_mouse.q`, documented in the header. Unknown-flag abort must still work. *(2026-10-25)* Done, including `--viz-delay-ms`; both are in the `KNOWN_FLAGS` check and the header.
- [x] **V1.3** `q/programs/viz_server.q`: tables above, `.z.ts` sampling, a ring buffer and a replay command for finished runs. *(2026-10-25)* Done with `-port` (not `-p`, so it can bind to 127.0.0.1 only), `-replay DIR -rate N`, a 240-frame energy ring buffer and a 5000-bar history. A bar number lower than the last one resets the history (a new run). Replay covers field and curves only: `simulation_log.csv` has no per-step energy.
- [x] **V1.4** `q/tests/test_viz.q`: with viz off, a seeded run is identical to before (compare `simulation_log.csv` and final weights hash against a reference); the publisher tolerates a dead connection; the ring buffer wraps correctly. *(2026-10-25)* 15 checks, run by `run_all.sh`.
- **Acceptance:** `q/tests/run_all.sh` passes; seed 42 × 2000 bars gives byte-identical `simulation_log.csv` with and without `--viz-port`; per-bar slowdown with viz on is measured and recorded here (target: small relative to the ~21 µs/neuron/step propagation cost, but measure it rather than assume). *(2026-10-25: all tests pass; seed 42 x 2000 bars is byte-identical with and without `--viz-port`. Cost of publishing every step with a local viewer: 2.64 s -> 2.93 s (about +11%). Meaning ~800 bars/s unthrottled, so the live view needs `--viz-delay-ms` to be watchable.)*

### V2 — Dashboards
- [x] **V2.1** Energy-matrix heatmap (primary + meta). *(2026-10-25)* Primary 6x12 and meta 3x5 matrices, one square-bubble layer each, coloured by energy level (dark/amber/red). Real sim data, checked in Firefox.
- [x] **V2.2** Cat–mouse field with trail and vision indicator. *(2026-10-25)* Mouse and cat, a 60-bar trail of each (drawn as points because the mouse wraps), and 8 blue gaze dots fanned across the active vision slice.
- [x] **V2.3** Learning-curve panel. *(2026-10-25)* Rolling 200-bar catch rate and 100-bar mean move alignment, over bar number. Both are from `simLog`-style data, not a performance claim.
- [x] **V2.4** Commit the exported dashboard definition under `dashboards/` (exact format depends on V0.1) and document how to import it. *(2026-10-25)* `dashboards/catbrain.json` and the connection are generated and committed under `dashboards/`. To use: `dashboards/install.sh`, restart `q dash.q -p 10001 -u 1`, start `q q/programs/viz_server.q -port 6814`, run the sim with `--viz-port 6814 --viz-delay-ms 5`, open `http://localhost:10001/#/5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b01`.
- **Acceptance:** run a live 5000-bar seed and watch energy visibly entering at the top row and leaving at the bottom; check the cat/mouse positions on screen against `simLog` for a few bars. *(2026-10-25: `scripts/e2e_dashboards.sh` runs the real sim (seed 42, 5 ms/step) against the viz server and Firefox: 10 checks pass, including every panel changing within 8 s. Energy is sparse, as expected from pressure < 0.06: typically one or two lit cells. The meta matrix stays dark until catches arrive, so only its presence is checked.)*

### V3 — Optional
- [ ] **V3.1** Synapse/route heatmap view.
- [ ] **V3.2** Side-by-side comparison of two runs (for example a trained vs an untrained brain, or `--no-direct-rewards` vs normal).
- [ ] **V3.3** Substrate-lab viewer: `.sub` runs on arbitrary grids (up to 16×16 and beyond), where the regimes (frozen / gas / channels) are far easier to recognise visually than from metrics. The publisher interface should take the grid shape from the brain dict rather than hard-coding 6×12.

## Progress log

- **2026-10-25** Plan moved to `inProgress`. Playwright 1.64 is installed globally; Firefox was fetched with `playwright install firefox`, and tests run with `NODE_PATH=/usr/lib/node_modules`. Dashboards definitions can be generated as JSON (the demo dashboards are the template), which avoids driving the editor by hand. Learned: the gateway reads dashboard files at start-up, ChartGL axes inherited from the template must be reset (the template had a Time x-axis and a fixed y range), and the bundled `Heatmap` component is a treemap, not a matrix.

## 6. Risks

- **Observer effect:** any per-step work inside the fold or the loop slows the sim. Publish only a copy of `prim`br` after `runCycle`, never from inside `.brain.stepNeuron`, and keep it opt-in.
- **Energy flows between steps, not within `runCycle`'s fold:** `br` after each `runCycle` is the right frame. Intermediate states inside the fold aren't observable without changing the engine, so don't attempt that.
- **Trinary display:** values are only 0/1/2, so the matrix will look blocky. A short fade (a decaying "recent energy" layer in the viz process) may be needed to make flow visible. Keep that logic in the viz process, never in the sim.
- **Pressure < 0.06 in practice:** the matrix will be sparse. That is itself informative, but don't expect it to look busy.
- **Dashboards limits:** custom drawing (trails, wedges) may be awkward. Fall back to plain scatter series if so.
- **Do not make performance or learning claims from the viewer.** Anything it appears to show about learning still needs a multi-seed paired experiment (REVIVAL_PLAN §6).

## 7. Decisions for the user

1. Is a KX Dashboards instance (and licence) already available, or is that part of V0?
2. Must live view work during normal runs, or is replay of recorded runs enough for a first version? Replay-only would be simpler and still shows energy flow, and live could follow later.
3. Is the optional slow-motion flag (`--viz-delay-ms`) acceptable, since it changes wall-clock time but not results?
4. Should per-step frames also be written to disk (`--viz-record`)? Cost: 72 values × 12 steps × bars, about 17M values for 20000 bars (17 MB as bytes, 69 MB as ints).
