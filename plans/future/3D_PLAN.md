# Plan: a 3D world and a 3D brain

**Status (2026-10-29):** future plan, not started. Written at the user's request when the revival plan closed. It carries the revival plan's unfinished substrate research (Phase S S.3b–S.7, P.5c, M.6), the 3D task W.7 and the dashboards substrate viewer (V3.3), because the 3D work starts in the same substrate lab.

**Read first:** [the revival plan](../completed/REVIVAL_PLAN.md) §2 (design intent, points 1–9), §3 (findings F1–F22) and §6 (results), and [THEORY.md](../../researchNotes/THEORY.md). The active plan is [OBSTACLES_PLAN.md](../completed/OBSTACLES_PLAN.md); this one starts only when its prerequisites (§2) are met.

**How to use this document:** same rules as the revival plan. Tasks are checkboxes (`[ ]` / `[~]` / `[x]`), ticked only after their acceptance check passes, with the date and what was verified. Every session appends to the Progress Log (§8). Decisions in §7 belong to the user.

---

## 0. Goal

> **The user (2026-10-29):** "that idea of creating a full 3D environment and a full 3D brain, so that needs to be a plan as well."

A cat with a **3D brain** hunting a mouse in a **3D world**, keeping the core concept unchanged:
- energy moves between neighbouring cells by usage-weighted probabilistic choice, and is conserved;
- the reinforced object is a *turn* (incoming direction × outgoing direction), strengthened by use (unconditional, §2 point 7) and steered by reward;
- decay lets structures break down and reorganise;
- a brain always receives input, from the world and from itself (§2 point 8);
- the hierarchy stays: a same-kind meta-brain whose input is satiety (starving left … full right) and whose motivation is to be full (§2 point 9).

**Success, in order:**
1. A 3D substrate with **persistent, reorganising activity** (the Phase S question, asked in 3D).
2. A 3D cat that learns to head towards the mouse in an open 3D world (a 3D north star; targets set in D.6).
3. The same cat finding a hidden mouse among 3D obstacles (the obstacles plan, lifted to 3D).

## 1. Why 3D is more than decoration

- **Loops.** The directed edge reinforced random walk (Erhard, Franco & Reis 2019; THEORY.md §6.1, M.5) is almost surely trapped in a **directed circuit** on any finite non-tree graph and on $\mathbb Z^d$, $d\ge2$. Superlinear, direction-dependent reinforcement on a lattice therefore favours circulating activity, *if* nothing drains it first. On today's 2D sheet, the downward bias and the bottom-row sink drain an injected unit within about a bar (F19, F22, W.1b). A 3D lattice offers many more short directed circuits per cell. **Hypothesis H3D:** at matched pressure, temperature and decay, a 3D lattice sustains activity (persistence, circulating flux) where the 2D sheet doesn't. Testable in the lab before any cat exists.
- **Harder tasks.** A third axis multiplies the directions to search and the places a mouse can hide. That makes memory and search strategies (the obstacles plan's question) necessary, not optional.

## 2. Prerequisites

- The obstacles plan's W.1–W.3 measured (blink, see-through walls, occluding walls), with the brain against the scripted baselines. That fixes what a 2D cat can and can't do, so 3D results have a reference.
- Stage A below (2D substrate research) far enough to know which surroundings (seal, thermostat, temperature, decay) give persistent activity in 2D.

## 3. Stage A: substrate research carried from the revival plan (2D lab first)

These were revival-plan Phase S tasks (`q/modules/lab/substrate.q`, `.sub`; `q/programs/lab/substrate_lab.q`; `scripts/lab/lab_sweep.sh`). Results so far: revival plan §6 "Phase S — substrate lab, sweep S.3". The lab doesn't touch `cat_mouse.q`; with `.sub.defaults` it must stay bit-identical to `.brain.propagate` (`test_substrate.q`).

- [ ] **A.1** (was S.3b) **Reorganisation.** In S.3, route turnover in the structured configs (≈0.003–0.008 per 50 steps) sat at the noise floor that decay alone produces (hebb-0 runs: ≈0.004–0.014). So structures form but **don't break down and reorganise** (design intent, point 2).
  - Sweep decay strength and interval against `hebb` (e.g. decay every 5–60 steps; hebb 100–1000) on the two middle-regime configs.
  - Target: persistence still well above churn (~0.3–0.8) *and* turnover clearly above the noise floor.
  - Needs a `--decay-rate` lab option (decay is fixed at ×0.98–0.995).
  - Tests THEORY.md P2 (channel transition between T ≈ 0.5 and 0.9, moving towards T = 1 as τ_d grows) and P3 (turnover rises above the noise floor once τ_d is below the run length and off-channel 𝓡 ≈ 1–10).
- [ ] **A.2** (was S.4) **Perturbation response** for promising configs. Run twin copies with identical randomness, poke one, and track how the difference evolves: it dies out (forgets), stays bounded, or spreads. This is the "input churns through it" property.
- [ ] **A.3** (was S.5) **Open system:** input injection plus a thermostat (set-point drain), and a non-destructive output readout. Use `.brain.injectAdd`-style additive injection (it never destroys energy, unlike the overwriting inputs, F16). Add a net-flux metric.
  - Then (was M.6) verify THEORY.md P1 (circulating current in a sealed, wrapped, downward-biased brain peaks near pressure 0.5) and P6 (pressure reaches 0.1 only when inflow ≳ 11 units per bar; Little's law with injection), and update their tags.
- [ ] **A.4** (was S.6) **Two coupled substrates:** one sets the other's reward and gets a basic reward itself (design intent, point 5). Use the satiety design: the upper substrate's input is a satiety-like scale of the lower one's success, and its reward is to keep that entry point on the "full" side.
- [ ] **A.5** (was S.7) **User decision:** what to port back into the 2D `cat_mouse.q` brain (seal, thermostat, temperature, decay settings), then re-run Phase 3-style controls (brain vs `--ablate-meta`, ≥ 16 seeds, paired).

## 4. Stage B: an N-dimensional substrate

- [ ] **B.1** Generalise `.sub` from a 2D 8-neighbour grid to an **N-dimensional lattice with a neighbour list**:
  - a cell is an index into a shape vector (e.g. `8 8 8`); a direction is an index into a list of neighbour offsets;
  - neighbour sets: `2D-8` (today), `3D-6` (faces), `3D-18` (faces + edges), `3D-26` (all);
  - a turn synapse is `K×K` per cell for K neighbours (36 for 3D-6, 676 for 3D-26); "came from" is the index of the opposite offset;
  - the direction bias becomes a vector over the neighbour list (gravity along one axis, or flat);
  - wrap per axis; sink and input faces are named faces of the lattice.
- [ ] **B.2** Tests (`test_substrate_nd.q`): with shape `6 12` and `2D-8`, bit-identical to today's `.sub` and so to `.brain.propagate`; energy conserved for every neighbour set, with wrap on and off, at temp 1, 0.3, 0; opposite-direction labels correct for every offset list.
- [ ] **B.3** The Phase S metrics in N-D (persistence, activity, saturation, spread per axis, route sharpness, route turnover), plus a **directed-cycle count** on the dominant-route graph (obstacles plan I.3, generalised).

## 5. Stage C: speed (a 3D brain is large)

An `8×8×8` brain with 26 neighbours has 512 cells × 676 weights ≈ 346k turn weights. The current sequential fold costs ~21 µs per active neuron per step (revival plan Phase P, lab profile), so 3D needs a faster step.
- [ ] **C.0** (was P.5a/b) **Bit-identical speed-ups first** (golden test guards them):
  - (a) avoid copy-on-write in the fold: unpacking `acc` into locals and repacking a 6-tuple per neuron forces copies. Keep state in globals and amend in place, or fold over a flat state.
  - (b) precompute per-cell neighbour indices and validity masks once, instead of recomputing `nrs/ncs/inBrain8/safeR/safeC` for every neuron at every step.
  - Measured context (revival P.7, 2026-10-29): in the cat, propagation is ~half of a ≈1.2 ms bar, with only one or two active cells per step, so (a)/(b) would gain ~15–25% there. In the lab, propagation is ~95% of the cost at ~21 µs per active neuron per step, so they matter here.
- [ ] **C.1** (was P.5c) **Synchronous, vectorised step.** *A semantic change: needs user approval (§7 decision 1).* Every active neuron chooses at once (n×K weight matrix, row-wise `sums`, n random draws); conflicts are resolved by capacity, with conservation still exact. Could be 10×+ on large grids, but it changes the dynamics from sequential to simultaneous.
  - Validate it *in the 2D lab first*: the S.3 regimes (frozen / gas / channels) and their metrics must reproduce. The random-order setting, already preferred because it removes the F19 artefact, is the closest sequential analogue to a synchronous update with random conflict resolution.
- [ ] **C.2** Profile 2D and 3D lattices; record ms per step against active neurons.

## 6. Stage D: the 3D lab study, then the 3D cat

- [ ] **D.1** **H3D in the lab:** an S.3-style sweep in 3D (3D-6 vs 3D-26; wrap; bias down vs flat; temp; hebb; decay from A.1), at matched pressure, against the 2D sheet. Measure persistence, circulating flux and directed-cycle counts. Acceptance: a clear answer, with seeds ≥ 3 per cell and CIs, to "does 3D sustain activity where 2D doesn't?".
- [ ] **D.2** **3D world module** (`.world3`, or `.world` generalised): a wrapped cube field (side 100); nearest-copy displacement per axis (`.vision.torDelta` already works component-wise; extend the tests to 3 components); obstacles as axis-aligned boxes (the slab test in `.world.hitT` generalises to 3 axes); line of sight; catch test (`.vision.pathDist` is vector code; test it in 3D).
- [ ] **D.3** **Vision and moves in 3D** (§7 decision 2): a direction set on the sphere (6 faces, 14 = faces + corners, or 26), used both for vision slices (the nearest direction to the mouse) and for moves. Compute the best possible alignment for that set (the 3D analogue of the 2D ≈ 0.90).
- [ ] **D.4** **3D brain for the cat:** vision directions enter on an input face; an output face maps to move directions, with an angle-consistent wiring (the 2D lesson: wiring order matters, revival §6 "Output wiring"). Satiety meta-brain (§7 decision 3: 2D or 3D) with cross-flow into a side face. Internal input (efference copy, resting activity) carried over as the obstacles plan decides.
- [ ] **D.5** **`cat_mouse3.q`** (or `cat_mouse.q` with a `--dims 3` mode), reusing the output files, `compare.q`, the G.4 re-learning test and `world_report.q`. A golden test of its own.
- [ ] **D.6** **The 3D north star:** set targets from D.3's ceiling (e.g. alignment ≥ 0.75 × ceiling in ≥ 14/16 seeds by a stated bar count). Paired experiments as always (≥ 16 seeds).
- [ ] **D.7** Hidden mouse in 3D: the obstacles plan's W.1–W.3 rungs in 3D, against 3D scripted baselines (random, greedy, reactive, persist).

## 7. Decisions for the user

1. **Synchronous step (C.1):** approve the semantic change from sequential to simultaneous updating, if the 2D lab shows the regimes reproduce?
2. **Direction set (D.3):** 6, 14 or 26 directions for vision and moves (and so the brain's neighbour set)? Recommendation: 26 for the brain (most loops), 14 or 26 for vision and moves; decide after D.1.
3. **Meta-brain:** 2D (as now) or 3D? "Of the same kind" (§2 point 5) argues for 3D; cost argues for starting 2D.
4. **Gravity:** keep an input face → output face drift (as the 2D brain's downward bias), or a flat bias with inputs and outputs anywhere? The drift is what drains activity (F19); the lab answers whether it's needed.
5. **Field size and mouse speed** in 3D (the 2D values were 100 and 2 units per 10 bars; G.5 measured sensitivity in 2D).

## 8. Also carried here

- [ ] **V.1** (was dashboards V3.3) **Substrate-lab viewer:** `.sub` runs on arbitrary grids, where the regimes (frozen / gas / channels) are far easier to recognise visually than from metrics. The publisher interface should take the grid shape from the brain dict rather than hard-coding 6×12. For 3D, show slices (one layer at a time) or a projection. See the completed [dashboards plan](../completed/KX_DASHBOARDS_PLAN.md).

## 9. Risks

- **Cost:** 3D weight arrays are ~100× larger per cell for 3D-26. Without C.1, runs may be too slow for 16-seed experiments.
- **Energy accumulation:** the Fortran-era notes blamed large meta-brains for energy build-up; propagation conserves at larger sizes (tested), so watch the overwriting inputs (F16) and use additive injection.
- **Comparability:** 3D results must be compared with 2D through scripted baselines and ceilings, not raw numbers.
- **Don't claim novelty** (THEORY.md §6.1, M.5): the combination wasn't found in a brief search; that isn't a review.

## 10. Progress Log

### 2026-10-29 — plan written
- Created at the user's request as the revival plan closed. Carried: S.3b–S.7 (as A.1–A.5), M.6 (in A.3), P.5a/b (as C.0), P.5c (as C.1), W.7 (as Stage B/D), dashboards V3.3 (as V.1), revival §7 decision 9 (as decisions 2–4 here).
