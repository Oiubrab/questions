/ Cat-Mouse Learning Simulation — q/kdb-x port
/ Mirrors src/programs/cat_mouse_learning.f90

/ Usage (run from repo root):
/   q q/programs/cat_mouse.q --seed 42 --bars 20000 [flags]
/ Flags:
/   --seed N              RNG seed (or first positional arg); default: time of day
/   --bars N              bars to simulate (default 20000)
/   --output-dir DIR      where CSVs and weights go (default results/seed<N>/)
/   --load-weights FILE   warm start from a weights_seed<N>.bin written by an earlier run
/   --meta-only           same as --no-direct-rewards (no tactical per-bar rewards)
/   --no-direct-rewards
/   --no-meta-rewards     disable strategic (meta-controlled) reinforcement of the primary brain
/   --ablate-meta         treat meta-brain output as all-zero: no cross-flow, minimum scope/magnitude
/   --no-learning        disable Hebbian updates, rewards, and decay
/ Defaults (since 2026-10-29, the revival plan's working configuration): cosine direction reward,
/ reward scale 0.25, hebb 250, wrapped world, angle wiring. The flags below restore the originals.
/   --raw-dot-reward      direction reward uses the raw (unnormalised) move . direction product, as
/                         before G.3a. --cos-reward (the default) is still accepted
/   --reward-scale L      multiply every primary-brain reward's log-step by L (default 0.25; 1 = original; G.3b)
/   --pg-update           direct reward becomes a policy-gradient update in log space (G.R2)
/   --pg-eta E            step size for --pg-update (default 0.3)
/   --rpe                 with --pg-update: reward = cosine minus its running average (G.R1)
/   --rpe-rate B          running-average rate for --rpe (default 0.01, ~100 moves)
/   --hebb N              unconditional per-use reinforcement of a route (default 250; original 1000; G.R3)
/   --walled-world        the cat is clamped to the box while the mouse wraps (the original world, F12).
/                         The default is the wrapped world (§7 decision 1); --wrap-world is still accepted
/   --weight-ratio R      synapse cap = R x floor (default 80000 = 2,000,000/25; G.R4). Lower R keeps
/                         every route explorable (MAX-MIN Ant System); R x floor >= 1000 so initial weights fit
/   Catch-rate plan (plans/inProgress/CATCH_RATE_PLAN.md); every default reproduces the original rule exactly:
/   --punish-calls N      neutral and bad moves both get N punish calls, from bar 1 (default: bad moves
/                         1 + 2 x epoch/numEpochs calls, neutral moves 1 call from bar 2,001)
/   --strategic-gate N    strategic reinforcement needs >= N catches in the last 200 bars (default 3)
/   --floor F             weight floor (default 25; 1-999). Initial weights stay 25-1000. Raises the cap too
/                         (cap = ratio x floor): give --weight-ratio to keep it, e.g. 20000 at floor 100
/   --decay-mult D        scale the primary brain's decay loss by D (default 1; 0.5 halves it; not with
/                         --two-timescale). The meta-brain's decay is unchanged
/   --wiring W            output slot -> move direction: angle (default; slot s -> direction at 45s degrees,
/                         the vision slices' order), grid (the original NW N NE W E SW S SE)
/                         or random (a fixed permutation per seed). REVIVAL_PLAN.md §7 decision 6
/ Brain architecture (default since 2026-11-02, the user's design): separate sight, touch and reward
/ brains feed a central brain, whose output moves the cat (q/modules/brain/modular.q).
/   --single-brain        the original architecture: one 6x12 brain with vision in its top row (baseline)
/   --central-rows N      rows of the central brain (default 3); sight and touch brains are 3x12
/   --no-touch            the touch brain gets no input (ablation; it is silent anyway without walls)
/   --meta-entry E        where the reward brain's output enters: right (the central brain's right
/                         column, as in the original), top (the free top-row cells beside sight/touch)
/                         or none (it only sets the strategic reinforcement). Default right
/ Harder goals and generalisation (obstacles plan W.5, W.6) and structure probes (I.2, I.4):
/   --mice N              N mice; each bar the nearest visible one is the one the cat sees and is rewarded on
/   --evasive             a mouse within 15 units of the cat steps away from it every bar
/   --distractor          a dog wanders like the mouse; it enters the same vision input with its own label
/                         (6), moving towards it when near is punished, and touching it (path within 2
/                         units) punishes the last 20 bars' routes ("bitten"); it then respawns
/   --layout-every N      with --world random: draw a new random layout every N bars (training on many)
/   --lesion R:C[,R:C..]  no route leads into stacked cell (R,C) (needs --no-learning; I.4)
/   --log-state           save each bar's brain state with what was last seen (state_log.bin; I.2)
/   --sat-edges a,b,c,d   satiety scale edges (default 0.5,2,5,10; obstacles plan O.4)
/   --random-tie          ties between output slots are broken at random, not towards slot 0 (G.3c, F11)
/   --mouse-step S        the mouse's random step, every 10 bars (default 2.0; G.5)
/   --field-size F        side of the (square) field (default 100; wall layouts scale with it; G.5)
/   --two-timescale       G.R5: fast weights relax towards a slow memory M instead of decaying to the floor,
/                         and M follows ln W slowly. Saves M as an 11th weights-file component
/   --tau-fast N          fast relaxation time constant in decay periods (5 bars; default 80, today's decay)
/   --tau-slow N          slow memory time constant in decay periods (default 200)
/   --world LAYOUT        obstacle worlds (OBSTACLES_PLAN.md): none (default), pillars, wall, rooms or random (6 blocks per
/                         seed). Walls block the cat and the mouse and hide the mouse. Needs --wrap-world
/   --see-through         walls block movement but not sight (W.2)
/   --blink V:B           the mouse is visible for V bars, then hidden for B bars, repeating (W.1)
/   --hidden-reward R     direct reward while the mouse is hidden: none (default since 2026-11-02, the
/                         user's decision: usage reinforcement and the catch boost still apply) or cos
/                         (the true direction, through walls)
/   --policy P            who moves the cat: brain (default) or a scripted baseline (W.0): random, greedy
/                         (true direction, through walls), reactive (best memoryless: the seen slice,
/                         random while hidden), persist (reactive, but keeps its last seen heading).
/                         planner (shortest path around walls, through doors: the ceiling with walls).
/                         Scripted policies move 5 units a bar and turn learning off. Writes world_log.csv
/   --efference           internal input: last bar's motor command (output slot a) lights row 1, column
/                         OUT_OFF+a (an efference copy; W.1b, user 2026-10-29: a brain always has input)
/   --relearn-schedule S  re-learning test (G.4): flick between worlds, e.g. "A:10000,L180:5000,A:5000"
/                         (A normal, L180/L90 legs rotated, E180 eyes rotated, FLEE goal flipped).
/                         Sets the run length; writes relearn_log.csv and switches.csv
/   --viz-port N          publish energy/cat/mouse frames to q/programs/viz/viz_server.q on port N (off by default)
/   --viz-delay-ms N      sleep N ms after each step so the energy flow is watchable (only with --viz-port)
/ Headless: append < /dev/null so q exits when the run ends instead of staying in the REPL.

/ Load all modules first
\l q/modules/brain/trinary.q
\l q/modules/brain/directions.q
\l q/modules/brain/synapses.q
\l q/modules/brain/brain.q
\l q/modules/world/vision.q
\l q/modules/brain/brain_engine.q
\l q/modules/viz/viz.q
\l q/modules/world/relearn.q
\l q/modules/world/world.q
\l q/modules/brain/modular.q

/ ================================================================
/ The run, in five parts (q/programs/cat_mouse/). All state is global, so loading them in order is the
/ same as one script; a runtime error stops the whole load with the failing part and line in the trace.
/ ================================================================
\l q/programs/cat_mouse/config.q
\l q/programs/cat_mouse/params.q
\l q/programs/cat_mouse/state.q
\l q/programs/cat_mouse/bar_loop.q
\l q/programs/cat_mouse/outputs.q
