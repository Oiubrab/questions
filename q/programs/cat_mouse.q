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
/                         every route explorable (MAX-MIN Ant System); R >= 40 so initial weights fit
/   --wiring W            output slot -> move direction: angle (default; slot s -> direction at 45s degrees,
/                         the vision slices' order), grid (the original NW N NE W E SW S SE)
/                         or random (a fixed permutation per seed). REVIVAL_PLAN.md §7 decision 6
/ Brain architecture (default since 2026-11-02, the user's design): separate sight, touch and reward
/ brains feed a central brain, whose output moves the cat (q/modules/modular.q).
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
/   --spontaneous P       internal input: every bar each cell gains one unit with probability P, from a
/                         random direction (resting activity). All three add energy, capped at HIGH
/   --relearn-schedule S  re-learning test (G.4): flick between worlds, e.g. "A:10000,L180:5000,A:5000"
/                         (A normal, L180/L90 legs rotated, E180 eyes rotated, FLEE goal flipped).
/                         Sets the run length; writes relearn_log.csv and switches.csv
/   --viz-port N          publish energy/cat/mouse frames to q/programs/viz_server.q on port N (off by default)
/   --viz-delay-ms N      sleep N ms after each step so the energy flow is watchable (only with --viz-port)
/ Headless: append < /dev/null so q exits when the run ends instead of staying in the REPL.

/ Load all modules first
\l q/modules/trinary.q
\l q/modules/directions.q
\l q/modules/synapses.q
\l q/modules/brain.q
\l q/modules/vision.q
\l q/modules/brain_engine.q
\l q/modules/viz.q
\l q/modules/relearn.q
\l q/modules/world.q
\l q/modules/modular.q

/ ================================================================
/ PARSE COMMAND LINE ARGUMENTS (flags documented at top of file)
/ ================================================================
args: .z.x
KNOWN_FLAGS: ("--seed";"--bars";"--output-dir";"--load-weights";
              "--meta-only";"--no-direct-rewards";"--no-meta-rewards";"--ablate-meta";"--no-learning";"--cos-reward";"--reward-scale";"--pg-update";"--pg-eta";"--rpe";"--rpe-rate";"--hebb";"--relearn-schedule";"--wrap-world";"--weight-ratio";"--wiring";"--world";"--see-through";"--blink";"--hidden-reward";"--policy";"--efference";"--spontaneous";
              "--raw-dot-reward";"--walled-world";"--random-tie";"--mice";"--sat-edges";"--evasive";"--distractor";"--layout-every";"--lesion";"--log-state";"--single-brain";"--central-rows";"--no-touch";"--meta-entry";"--mouse-step";"--field-size";
              "--two-timescale";"--tau-fast";"--tau-slow";"--viz-port";"--viz-delay-ms")
/ Fail on typos: a misspelt ablation flag would otherwise silently run the control arm
badFlags: (args where args like "--*") except KNOWN_FLAGS
if[count badFlags; -2 "Unknown flag(s): ",", " sv badFlags; exit 1]
flagVal:{[f] i: first where args like f; $[null i; ""; args i+1]}
hasFlag:{[f] any args like f}
/ Seed: --seed N (preferred) or first positional arg that is not a flag
userSeed: $[count flagVal "--seed"; "I"$flagVal "--seed";
  (0<count args) & not (first args) like "--*"; "I"$first args;
  `int$.z.t]
/ Bars override
MAX_BARS_ARG: $[count flagVal "--bars"; "I"$flagVal "--bars"; 0Ni]
loadWeights: hasFlag "--load-weights"
weightFile: flagVal "--load-weights"
noDirectRewards: (hasFlag "--no-direct-rewards") | hasFlag "--meta-only"
noMetaRewards: hasFlag "--no-meta-rewards"
ablateMeta: hasFlag "--ablate-meta"
noLearning: hasFlag "--no-learning"
/ Obstacle worlds (plans/inProgress/OBSTACLES_PLAN.md): harder worlds and scripted baseline agents. All off by default.
POLICY: $[count flagVal "--policy"; `$flagVal "--policy"; `brain]
if[not POLICY in `brain`random`greedy`reactive`persist`planner; -2 "--policy must be brain, random, greedy, reactive, persist or planner"; exit 1]
scripted: POLICY<>`brain
if[scripted; noLearning: 1b]                  / a baseline agent is measured, never trained
/ G.3(a) cosine reward: the default since 2026-10-29 (user-approved defaults milestone)
if[(hasFlag "--cos-reward") & hasFlag "--raw-dot-reward"; -2 "--cos-reward and --raw-dot-reward conflict"; exit 1]
cosReward: not hasFlag "--raw-dot-reward"
/ Toroidal world (user decision, REVIVAL_PLAN.md §7 decision 1): the cat wraps around the edges like
/ the mouse, and everything that asks "where is the mouse from here?" (vision, direction reward,
/ alignment, the catch test) uses the nearest copy of the mouse across the edges. The default since
/ 2026-10-29; --walled-world clamps the cat to the box and uses raw differences, as before (F12).
if[(hasFlag "--wrap-world") & hasFlag "--walled-world"; -2 "--wrap-world and --walled-world conflict"; exit 1]
wrapWorld: not hasFlag "--walled-world"
/ Output wiring (§7 decision 6). The grid order sends an untrained brain's vision slices 0/1 the
/ most wrong way, which the wrapped world exposes (REVIVAL_PLAN.md §6).
WIRING: $[count flagVal "--wiring"; `$flagVal "--wiring"; `angle]
if[not WIRING in `grid`angle`random; -2 "--wiring must be grid, angle or random"; exit 1]
WORLD: $[count flagVal "--world"; `$flagVal "--world"; `none]
if[not WORLD in `none`pillars`wall`rooms`random; -2 "--world must be none, pillars, wall, rooms or random"; exit 1]
worldOn: WORLD<>`none
if[worldOn & not wrapWorld; -2 "--world needs --wrap-world (walls are laid out on the wrapped field)"; exit 1]
seeThrough: hasFlag "--see-through"
blinkSpec: flagVal "--blink"
blink: 0<count blinkSpec
BLINK: $[blink; "I"$":" vs blinkSpec; 0 0i]
if[blink & (2<>count BLINK) | (any null BLINK) | any BLINK<=0i; -2 "--blink must be V:B with positive bar counts"; exit 1]
/ Reward while the mouse is hidden: none (user, 2026-11-02, obstacles plan §7 decision 1)
HIDDEN_REWARD: $[count flagVal "--hidden-reward"; `$flagVal "--hidden-reward"; `none]
if[not HIDDEN_REWARD in `cos`none; -2 "--hidden-reward must be cos or none"; exit 1]
/ W.1b internal input (user, 2026-10-29): "a real organic brain is always receiving input ... not
/ just from the outside world, but from itself internally." Without it, a cat that can't see the
/ mouse and has no recent catch gets no input at all and stops (F22). Each channel is opt-in so it
/ can be measured and ablated on its own.
efference: hasFlag "--efference"
SPONT: $[count flagVal "--spontaneous"; "F"$flagVal "--spontaneous"; 0f]
if[(null SPONT)|(SPONT<0f)|SPONT>1f; -2 "--spontaneous must be a probability in [0,1]"; exit 1]
/ G.3(c): break ties between output slots at random (otherwise the lowest slot wins, F11)
randomTie: hasFlag "--random-tie"
/ W.5 harder goals, W.6 layout redraws, I.2/I.4 probes
NMICE: $[count flagVal "--mice"; "I"$flagVal "--mice"; 1i]
if[(null NMICE)|NMICE<1i; -2 "--mice must be a positive integer"; exit 1]
evasive: hasFlag "--evasive"; EVADE_R: 15f
distractor: hasFlag "--distractor"; DOG_LABEL: 6i; DOG_NEAR: 25f
LAYOUT_EVERY: $[count flagVal "--layout-every"; "I"$flagVal "--layout-every"; 0Ni]
lesionSpec: flagVal "--lesion"
logState: hasFlag "--log-state"
/ O.4: satiety scale edges between the 5 meta input cells (starving .. full)
if[count flagVal "--sat-edges";
  .brain.SAT_EDGES: "F"$"," vs flagVal "--sat-edges";
  if[(4<>count .brain.SAT_EDGES)|(any null .brain.SAT_EDGES)|(any .brain.SAT_EDGES<=0f)|any 0>=1_deltas .brain.SAT_EDGES;
    -2 "--sat-edges must be 4 increasing positive numbers"; exit 1]]
/ Modular brain (obstacles plan W.4, user 2026-11-02): sight, touch and reward brains feed a central brain
MODULAR: not hasFlag "--single-brain"
SENSE_ROWS: 3i
CENTRAL_ROWS: $[count flagVal "--central-rows"; "I"$flagVal "--central-rows"; 3i]   / 3: best of 3/4/6 (obstacles plan §6)
if[(null CENTRAL_ROWS)|CENTRAL_ROWS<2i; -2 "--central-rows must be at least 2"; exit 1]
noTouch: hasFlag "--no-touch"
META_ENTRY: $[count flagVal "--meta-entry"; `$flagVal "--meta-entry"; `right]
if[not META_ENTRY in `right`top`none; -2 "--meta-entry must be right, top or none"; exit 1]
if[(hasFlag "--layout-every") & ((null LAYOUT_EVERY)|LAYOUT_EVERY<1i) | not WORLD=`random; -2 "--layout-every needs a positive bar count and --world random"; exit 1]
if[(count lesionSpec) & not noLearning; -2 "--lesion needs --no-learning (learning would restore the routes)"; exit 1]
if[(not MODULAR) & any hasFlag each ("--central-rows";"--no-touch";"--meta-entry"); -2 "--central-rows, --no-touch and --meta-entry need the modular brain (not --single-brain)"; exit 1]
WHISKER: 3f                                   / touch range in field units
SIGHT_LABEL: 7i; TOUCH_LABEL: 6i; META_LABEL: 8i   / incoming-direction labels into the central brain
/ G.5 generalisation: mouse speed and field size
MOUSE_STEP: $[count flagVal "--mouse-step"; "F"$flagVal "--mouse-step"; 2f]
if[(null MOUSE_STEP)|MOUSE_STEP<0f; -2 "--mouse-step must be a non-negative number"; exit 1]
FIELD_ARG: $[count flagVal "--field-size"; "F"$flagVal "--field-size"; 100f]
if[(null FIELD_ARG)|FIELD_ARG<20f; -2 "--field-size must be at least 20"; exit 1]
/ G.R5 two-timescale weights (synaptic consolidation): see .syn.relaxDecay / .syn.consolidate
twoTS: hasFlag "--two-timescale"
TAU_FAST: $[count flagVal "--tau-fast"; "F"$flagVal "--tau-fast"; 80f]
TAU_SLOW: $[count flagVal "--tau-slow"; "F"$flagVal "--tau-slow"; 200f]
if[(null TAU_FAST)|TAU_FAST<2f; -2 "--tau-fast must be at least 2 decay periods"; exit 1]
if[(null TAU_SLOW)|TAU_SLOW<1f; -2 "--tau-slow must be at least 1 decay period"; exit 1]
if[((hasFlag "--tau-fast")|hasFlag "--tau-slow") & not twoTS; -2 "--tau-fast/--tau-slow need --two-timescale"; exit 1]
/ G.R4 bounded weights (MAX-MIN Ant System): cap/floor ratio. With the default 80,000:1 a floor route
/ next to a capped one is chosen ~0.001% of the time, so old habits are slow to un-learn. Lowering R
/ guarantees every route a minimum chance. Initial weights are 25-1000, so R must be >= 40.
WEIGHT_RATIO: $[count flagVal "--weight-ratio"; "F"$flagVal "--weight-ratio"; 80000f]
if[(null WEIGHT_RATIO)|WEIGHT_RATIO<40f; -2 "--weight-ratio must be >= 40 (initial weights go up to 1000)"; exit 1]
.syn.MAX_STRENGTH: `int$WEIGHT_RATIO*.syn.MIN_STRENGTH
/ G.3(b): rewards multiply weights, so they are steps in log space (reinforce +0.80, punish -0.84,
/ catch boost +0.41 per bar, strategic +0.095*m per lookback bar). REWARD_SCALE multiplies every
/ primary-brain step by L, keeping the same RNG draws; 1 is the original behaviour exactly.
REWARD_SCALE: $[count flagVal "--reward-scale"; "F"$flagVal "--reward-scale"; 0.25]
if[(null REWARD_SCALE)|REWARD_SCALE<=0f; -2 "--reward-scale must be a positive number"; exit 1]
CATCH_MULT: .syn.scaleMult[1.5;REWARD_SCALE]   / catch boost per history bar
/ G.R1/G.R2: the route choice is a softmax over ln W, so REINFORCE gives the principled update:
/ every used (cell, inDir) moves its 8 routes by PG_ETA*delta*(used - p). delta is the move's
/ cosine to the mouse (R), or with --rpe, R minus its running average rBar (better than expected).
pgUpdate: hasFlag "--pg-update"
PG_ETA: $[count flagVal "--pg-eta"; "F"$flagVal "--pg-eta"; 0.3]
rpe: hasFlag "--rpe"
RPE_RATE: $[count flagVal "--rpe-rate"; "F"$flagVal "--rpe-rate"; 0.01]
if[rpe & not pgUpdate; -2 "--rpe needs --pg-update (the baseline only applies to the policy-gradient update)"; exit 1]
if[(null PG_ETA)|PG_ETA<=0f; -2 "--pg-eta must be a positive number"; exit 1]
if[(null RPE_RATE)|(RPE_RATE<=0f)|RPE_RATE>1f; -2 "--rpe-rate must be in (0,1]"; exit 1]
rBar: 0f                                      / running average of the move cosine (--rpe)
/ G.R3: unconditional usage reinforcement per route use. It must stay > 0 (REVIVAL_PLAN.md §2
/ point 7); the tunable is its ratio to the directed reward reinforcement (--reward-scale).
.brain.HEBB: $[count flagVal "--hebb"; "I"$flagVal "--hebb"; 250i]
if[(null .brain.HEBB)|.brain.HEBB<=0i; -2 "--hebb must be a positive integer (unconditional reinforcement is never removed)"; exit 1]
.brain.hebbEnabled: not noLearning
outputDir: flagVal "--output-dir"
if[0=count outputDir; outputDir: "results/seed",string userSeed]
if[not last[outputDir]="/"; outputDir,:"/"]

/ ================================================================
/ SIMULATION PARAMETERS (match Fortran exactly)
/ ================================================================
/ ROWS is the stacked primary state: sight, touch and central brains (modular) or the single brain
ROWS: $[MODULAR; (2i*SENSE_ROWS)+CENTRAL_ROWS; 6i]; COLS: 12i
IN_OFF: 2i; IN_LEN: 8i      / inputter offset (0-indexed) and length
OUT_OFF: 2i; OUT_LEN: 8i    / outputter offset and length
MAX_BARS: $[not null MAX_BARS_ARG; MAX_BARS_ARG; 20000i]
/ G.4 re-learning schedule: the run is a sequence of worlds (q/modules/relearn.q). Each world
/ changes the cat's legs (output slot -> move direction), its eyes (vision slice -> input column)
/ or its goal (chase or flee). Off by default; then behaviour is unchanged.
relearnSpec: flagVal "--relearn-schedule"
relearn: 0<count relearnSpec
if[relearn;
  relearnSched: @[.relearn.parseSchedule;relearnSpec;{-2 "--relearn-schedule: ",x; exit 1}];
  relearnEnds: sums relearnSched 1;
  if[(not null MAX_BARS_ARG)&MAX_BARS_ARG<>last relearnEnds;
    -2 "--bars must equal the schedule total (",(string last relearnEnds),") or be omitted"; exit 1];
  MAX_BARS: last relearnEnds;
  relearnGA: MAX_BARS#0n]
  / goal alignment per bar (null = no move)
if[relearn & scripted; -2 "--policy baselines don't take --relearn-schedule"; exit 1]
/ Current world's legs, eyes and goal sign (identity / chase when no schedule)
legPerm: `int$til 8; eyePerm: `int$til 8; goalSign: 1f; fleeNow: 0b; worldNow: `A
STEPS_PER_BAR: 12i
SNAPSHOT_INTERVAL: 1000i
MOUSE_MOVE_INTERVAL: 10i
FIELD_SIZE: FIELD_ARG                          / 100 unless --field-size (G.5)
STEP_SIZE: 5.0
EPOCH_SIZE: 2000i

META_ROWS: 3i; META_COLS: 5i
META_IN_LEN: 5i; META_OUT_LEN: 5i
META_IN_OFF: 0i; META_OUT_OFF: 0i

/ Meta cross-flow: metaBr output enters rows 2-6 of primary brain (0-indexed: rows 1-5)
META_INPUT_OFFSET: 1i          / row offset into primary brain (0-indexed)
META_INPUT_MAP_LEN: 5i
OVERFLOW_OFFSET: 1i            / row offset into primary brain (0-indexed)
OVERFLOW_MAP_LEN: 4i

RATE_WINDOW: 200i
MAX_CATCHES_IN_WINDOW: 200i
HISTORY_SIZE: 100i             / synapse history circular buffer depth

/ Movement deltas (0-indexed directions 0-7 matching .dirs)
/ move_directions in Fortran: row=y, col=x
/ cat_move_x = moveDirCol[d]*dist*5, cat_move_y = moveDirRow[d]*dist*5
moveDirRow: `int$-1 -1 -1 0 0 1 1 1
moveDirCol: `int$-1 0 1 -1 1 -1 0 1

/ ================================================================
/ SEED RANDOM NUMBER GENERATOR
/ ================================================================
system "S ",string userSeed
/ Output wiring: slot -> .dirs move direction. random draws its permutation from a seed of its own and
/ then restores the run seed, so the initial weights stay paired with the other wirings' runs.
wiringPerm: $[WIRING=`angle; .relearn.ANGLE_ORDER;
  WIRING=`random; [system "S ",string 7919+userSeed; p: `int$neg[8]?8; system "S ",string userSeed; p];
  .relearn.I8]
/ Phase W walls, copied into the 8 neighbouring tiles of the wrapped field (.world.tile). The random
/ layout, like random wiring, draws from a seed of its own so the initial weights stay paired.
wallsRaw: $[WORLD=`random; [system "S ",string 104729+userSeed; r: .world.randomLayout[6;FIELD_SIZE]; system "S ",string userSeed; r];
  (FIELD_SIZE%100f)*.world.LAYOUTS WORLD]       / layouts are drawn on a 100-unit field
WT: .world.tile[wallsRaw;FIELD_SIZE]
/ Spawns in a walled world: a point 30-45 units from c (third argument unused, so g[] can retry)
ringPos:{[c;f;x] a: 2f*.vision.PI*rand 1f; r: 30f+15f*rand 1f; .vision.wrapPos[c+r*(cos a;sin a);f]}
/ Scripted policies (W.0): unit move vectors in .dirs order, and the direction closest to v
/ planner: free-cell grid (2-unit cells, clear of walls by 1 unit), and the distance field towards the
/ mouse, recomputed whenever the mouse has moved
PLAN_G: `long$FIELD_SIZE%2f
if[POLICY=`planner; planFree: .world.freeGrid[WT;FIELD_SIZE;PLAN_G;1f]; planFor: 0n 0n; planD: ()]

if[count flagVal "--viz-port";
  .viz.init["I"$flagVal "--viz-port"; $[count flagVal "--viz-delay-ms"; "I"$flagVal "--viz-delay-ms"; 0i]]]

/ ================================================================
/ INITIALIZE BRAIN SYSTEMS
/ ================================================================
/ Primary brain (6x12)
prim: .engine.initSystem[ROWS;COLS;IN_LEN;OUT_LEN;IN_OFF;OUT_OFF]
prim[`su]: .syn.emptySparseUsage[]

/ Meta-brain (3x5)
metaBr: .engine.initSystem[META_ROWS;META_COLS;META_IN_LEN;META_OUT_LEN;META_IN_OFF;META_OUT_OFF]
metaBr[`su]: .syn.emptySparseUsage[]

/ Sparse synapse history: one tagged flat-index set per bar
hist: .syn.initSparseHist HISTORY_SIZE
histIdx: 0i    / write pointer into circular buffer (0-indexed)

/ Side I/O: metaBr-brain output will be stored here between bars
sideMetaInp: `int$META_INPUT_MAP_LEN#0i    / metaBr output → right col of primary brain
overflow: `int$OVERFLOW_MAP_LEN#0i        / left col of primary brain

/ ================================================================
/ CATCH RATE SLIDING WINDOW (proper windowed counter)
/ ================================================================
catchTs: RATE_WINDOW#-999999i    / circular buffer of bar numbers when catches occurred
catchHead: 0i                    / write pointer
catchCount: 0i                   / number of valid entries in buffer

/ ================================================================
/ LOAD WEIGHTS (if --load-weights provided)
/ ================================================================
/ Format (written by SAVE BINARY WEIGHTS at the end of a run):
/   (prim syns; meta syns; prim br; prim inc0; prim inc1; meta br; meta inc0; meta inc1; hist; histIdx)
/ Old Fortran .bin files are a different format and are not supported.
/ Not restored: catch timestamps, so rateCounter starts at 0 on a warm start.
/ dims: shape of a nested list, following first elements (atom -> empty)
dims:{$[0>type x; `long$(); (count x),.z.s first x]}
if[loadWeights;
  show "Loading weights from: ",weightFile;
  w: get hsym `$weightFile;
  denseHistShape: .syn.initHist[HISTORY_SIZE;ROWS;COLS];
  expect: (prim`syns; metaBr`syns; prim`br; prim`inc0; prim`inc1;
           metaBr`br; metaBr`inc0; metaBr`inc1; denseHistShape; histIdx);
  / 11 components: the 11th is the G.R5 slow memory M (only written with --two-timescale)
  if[not (count w) in 10 11; -2 "Weight file must have 10 or 11 components"; exit 1];
  if[not (dims each 10#w)~dims each expect;
    -2 "Weight file shape mismatch: got ",(.Q.s1 dims each w),", want ",.Q.s1 dims each expect;
    exit 1];
  prim[`syns`br`inc0`inc1]: w 0 2 3 4;
  metaBr[`syns`br`inc0`inc1]: w 1 5 6 7;
  hist: .syn.denseHistToSparse w 8; histIdx: w 9;
  if[(11=count w) & not twoTS; show "Note: the weights file holds a slow memory (G.R5); ignored without --two-timescale"];
  show "Loaded warm state; primary brain energy: ",string sum raze prim`br]
/ G.R5 slow memory: log-weights, flat in the order of raze raze raze syns. Starts at the initial (or
/ loaded) weights, or at the saved memory when the weights file has one.
/ I.4 lesion: zero every route that leads into each listed stacked cell, from neighbours in the same
/ brain (modular brains are stacked, so the brain boundary isn't a neighbour boundary)
if[count lesionSpec;
  blockOf:{[r] $[MODULAR; $[r<SENSE_ROWS; (0;SENSE_ROWS); r<2*SENSE_ROWS; (SENSE_ROWS;2*SENSE_ROWS); (2*SENSE_ROWS;ROWS)]; (0;ROWS)]};
  lesion1:{[syns;rc] R: rc 0; C: rc 1; bk: blockOf R;
    {[R;C;bk;syns;d] r: R-.dirs.dr d; c: C-.dirs.dc d;
      $[(r>=bk 0)&(r<bk 1)&(c>=0)&c<COLS; .[syns;(r;c;::;d);:;0i]; syns]}[R;C;bk]/[syns;til 8]};
  prim[`syns]: lesion1/[prim`syns;{"I"$":" vs x} each "," vs lesionSpec];
  show "Lesioned cells: ",lesionSpec]
if[twoTS; slowM: $[loadWeights; $[11=count w; w 10; log `float$raze raze raze prim`syns]; log `float$raze raze raze prim`syns]]

/ ================================================================
/ INITIALIZE POSITIONS
/ ================================================================
/ Mouse starts at center
mousePos: FIELD_SIZE*0.5 0.5

/ Cat starts 30-45 units from center in random direction
dx: (2*(rand 1.0))-1.0; dy: (2*(rand 1.0))-1.0
d:.vision.norm (dx;dy)
if[d>0.0; dx:dx%d; dy:dy%d]
catDist: 30.0 + 15.0*(rand 1.0)
catPos: $[wrapWorld;
  .vision.wrapPos[(mousePos[0]+dx*catDist; mousePos[1]+dy*catDist);FIELD_SIZE];
  (0.0 | FIELD_SIZE & mousePos[0]+dx*catDist;
   0.0 | FIELD_SIZE & mousePos[1]+dy*catDist)]

/ Walled world: re-draw any spawn that lands in (or within 2 units of) a wall
if[worldOn;
  if[.world.inWall[WT;mousePos;2f]; mousePos: .world.clearPos[WT;2f;{FIELD_SIZE*2?1f}]];
  if[.world.inWall[WT;catPos;2f]; catPos: .world.clearPos[WT;2f;ringPos[mousePos;FIELD_SIZE]]]]
prevDist: .vision.toroidalDist[catPos;mousePos;FIELD_SIZE]
bestDist: prevDist
/ W.5: extra mice and the dog are placed from a seed of their own, so one mouse leaves the run unchanged
mice: enlist mousePos; mouseIdx: 0
if[(NMICE>1i)|distractor;
  system "S ",string 31337+userSeed;
  if[NMICE>1i; mice,: {[x] p: FIELD_SIZE*2?1f; $[worldOn; .world.clearPos[WT;2f;{FIELD_SIZE*2?1f}]; p]} each til NMICE-1];
  dogPos: $[worldOn; .world.clearPos[WT;2f;ringPos[catPos;FIELD_SIZE]]; ringPos[catPos;FIELD_SIZE;0]];
  system "S ",string 977+userSeed]                  / a fresh stream, not a replay of the run seed's
dogContacts: 0i
if[logState; stBr: (MAX_BARS;ROWS*COLS)#0i; stInc: (MAX_BARS;ROWS*COLS)#0i; stSeen: MAX_BARS#-1i; lastSeenSlice: -1i]

/ ================================================================
/ ANTI-OSCILLATION STATE
/ ================================================================
movHist: 10#0i; movHistIdx: 0i
adaptThresh: 0.7; THRESH_MIN: 0.6; THRESH_MAX: 0.85
lastRewDir: 0i; momStreak: 0i
barsSinceProgress: 0i; PROGRESS_INTERVAL: 50i

/ ================================================================
/ TRACKING COUNTERS
/ ================================================================
catchesCount: 0i
/ Phase W per-bar record (always kept; world_log.csv is written only when a W flag is on)
wActive: worldOn | blink | scripted | (NMICE>1i) | evasive | distractor
wVis: MAX_BARS#1b; wSince: MAX_BARS#0i; wBlocked: MAX_BARS#0b; wCaught: MAX_BARS#0b
sinceSeen: 0i; lastSeenDir: 0Ni; visible: 1b; blocked: 0b
lastMoveSlot: 0Ni                             / for --efference
lastBlockedDir: 0Ni                           / touch: angle index of last bar's blocked move (or null)
/ Satiety (the meta-brain's input; .brain.satietyInput): a leaky store of food, +1 per catch,
/ decaying with a ~RATE_WINDOW-bar time constant. The cat starts starving.
satiety: 0f; SAT_DECAY: exp neg 1%RATE_WINDOW
satCells: MAX_BARS#0Ni
movesTowards: 0i; movesAway: 0i; movesPerp: 0i; totalMoves: 0i
numEpochs: MAX_BARS div EPOCH_SIZE
/ +1 to hold the partial final epoch (bars numEpochs*EPOCH_SIZE..MAX_BARS)
epochCatches: `int$(numEpochs+1i)#0i
epochTowards: `int$(numEpochs+1i)#0i
epochAway:    `int$(numEpochs+1i)#0i
epochPerp:    `int$(numEpochs+1i)#0i
epochAlignSum: (numEpochs+1i)#0f
epochAlignCount: `int$(numEpochs+1i)#0i
catchesThisEpoch: 0i; towardsEpoch: 0i; awayEpoch: 0i; perpEpoch: 0i
currentEpoch: 0i    / 0-indexed
alignmentWindow: 500#0n
alignmentWindowSum: 0f; alignmentWindowCount: 0i
acquisitionTime: 0Ni

brainPressure: 0.0

/ ================================================================
/ CSV LOG: use kdb-x tables for simulation log
/ ================================================================
/ Simulation log as a kdb-x table — this is the killer feature vs Fortran
/ We build it incrementally and can query it in real time
simLog: flip `bar`mouseX`mouseY`catX`catY`visionSlice`brainEnergy`outEnergy`outAction`moveDist`alignment`catches`pressure`overflow!
  (`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`float$();`int$();`float$();`int$())
/ Note: we append each bar's data; at end we write CSV

/ ================================================================
/ MAIN SIMULATION LOOP
/ ================================================================
show "=== CAT & MOUSE CONTINUOUS HUNTING SIMULATION (q/kdb-x) ==="
show "Max bars: ",string MAX_BARS
show "Steps per bar: ",string STEPS_PER_BAR
show "Seed: ",string userSeed
show "Learning enabled: ",string not noLearning
show "Direct rewards: ",string (not noDirectRewards)&(not noLearning)
show "Strategic (meta) rewards: ",string (not noMetaRewards)&(not noLearning)
show "Meta-brain ablated: ",string ablateMeta
show "Cosine direction reward: ",string cosReward
show "Reward scale (log-step multiplier): ",string REWARD_SCALE
show "Unconditional usage reinforcement (hebb): ",string .brain.HEBB
show "Re-learning schedule: ",$[relearn; relearnSpec; "none"]
show "Wrapped (toroidal) world for the cat: ",string wrapWorld
show "World: ",(string WORLD),$[seeThrough; " (see-through)"; ""],"; blink: ",$[blink; blinkSpec; "off"],"; policy: ",(string POLICY),"; hidden reward: ",string HIDDEN_REWARD
show "Brain: ",$[MODULAR; "modular (sight 3x12, touch 3x12",$[noTouch; ", no input"; ""],", central ",(string CENTRAL_ROWS),"x12; reward brain enters ",(string META_ENTRY),")"; "single 6x12"]
show "Output wiring: ",(string WIRING)," (slot -> direction ",(" " sv string wiringPerm),")"
show "Weight cap (ratio x floor): ",(string .syn.MAX_STRENGTH)," (ratio ",(string WEIGHT_RATIO),")"
show "Policy-gradient direct reward: ",(string pgUpdate),$[pgUpdate; " (eta ",(string PG_ETA),"; rpe ",(string rpe),$[rpe; ", rate ",string RPE_RATE; ""],")"; ""]
show "Weights: ",$[loadWeights; weightFile; "fresh"]
show "Output dir: ",outputDir
show ""

bar: 1i
while[bar<=MAX_BARS;

  / ---- RE-LEARNING WORLD (G.4) ----
  if[relearn;
    worldNow: (relearnSched 0) relearnEnds binr bar;
    wd: .relearn.WORLDS worldNow;
    legPerm: wd 0; eyePerm: wd 1; goalSign: wd 2; fleeNow: goalSign<0f];

  / ---- EPOCH BOUNDARY ----
  if[(bar>1i)&0i=(`int$bar-1i) mod EPOCH_SIZE;
    epochCatches[currentEpoch]: catchesThisEpoch;
    epochTowards[currentEpoch]: towardsEpoch;
    epochAway[currentEpoch]: awayEpoch;
    epochPerp[currentEpoch]: perpEpoch;
    show "Epoch ",(string currentEpoch+1i)," complete: ",(string catchesThisEpoch)," catches";
    currentEpoch+:1i;
    catchesThisEpoch: 0i; towardsEpoch: 0i; awayEpoch: 0i; perpEpoch: 0i];

  / ---- RATE COUNTER (windowed catch count) ----
  rateCounter: `int$sum (bar - catchTs[til catchCount]) <= RATE_WINDOW;

  / ---- ENCODE CATCH RATE INTO META INPUTTER ----
  / Positional encoding: rate 1-5 → MEDIUM at positions 5,4,3,2,1
  /                      rate 6-10 → HIGH at positions 5,4,3,2,1
  /                      rate >10 → saturate position 0 with HIGH
  / ---- SATIETY INTO THE META INPUTTER: starving enters on the left, full on the right ----
  satiety*: SAT_DECAY;
  satCell: .brain.satietyCell satiety; satCells[bar-1]: satCell;
  metaInp: .brain.satietyInput[satiety;META_IN_LEN];
  metaBr[`inp]: metaInp;

  / ---- MEASURE DISTANCE BEFORE ACTION ----
  prevDist: .vision.toroidalDist[catPos;mousePos;FIELD_SIZE];

  / ---- RESET SYNAPSE USAGE ----
  prim[`su]: .syn.emptySparseUsage[];
  / Modular brain: split the stacked state into the sight, touch and central brains for this bar;
  / until they are stacked again after propagation, prim is the central brain
  if[MODULAR;
    sightB: .mod.sub[prim;0;SENSE_ROWS;IN_OFF;IN_LEN;OUT_OFF;OUT_LEN];
    touchB: .mod.sub[prim;SENSE_ROWS;SENSE_ROWS;IN_OFF;IN_LEN;OUT_OFF;OUT_LEN];
    prim: .mod.sub[prim;2i*SENSE_ROWS;CENTRAL_ROWS;IN_OFF;IN_LEN;OUT_OFF;OUT_LEN]];
  metaBr[`su]: .syn.emptySparseUsage[];

  / ---- W.6: a new random layout every LAYOUT_EVERY bars (cat, mice and dog moved clear of walls) ----
  if[(not null LAYOUT_EVERY) & (bar>1i) & 0i=(bar-1i) mod LAYOUT_EVERY;
    system "S ",string 104729+userSeed+(bar-1i) div LAYOUT_EVERY;
    wallsRaw: .world.randomLayout[6;FIELD_SIZE]; WT: .world.tile[wallsRaw;FIELD_SIZE];
    clr:{[p] $[.world.inWall[WT;p;2f]; .world.clearPos[WT;2f;{FIELD_SIZE*2?1f}]; p]};
    catPos: clr catPos; mice: clr each mice; if[distractor; dogPos: clr dogPos];
    if[POLICY=`planner; planFree: .world.freeGrid[WT;FIELD_SIZE;PLAN_G;1f]; planFor: 0n 0n];
    system "S ",string userSeed+bar];
  / ---- MOUSE MOVEMENT (every MOUSE_MOVE_INTERVAL bars) ----
  / Walled world: the mouse's 2-unit step is blocked by walls like the cat's
  stepM:{[p] $[worldOn;
      [ma: 2f*.vision.PI*rand 1.0; .vision.wrapPos[first .world.move[WT;p;MOUSE_STEP*(cos ma;sin ma)];FIELD_SIZE]];
      .vision.moveMouse[p;FIELD_SIZE;MOUSE_STEP]]};
  if[0i=bar mod MOUSE_MOVE_INTERVAL;
    mousePos: stepM mousePos;
    if[NMICE>1i; mice[mouseIdx]: mousePos; mice: @[mice;(til count mice) except mouseIdx;stepM]];
    if[distractor; dogPos: stepM dogPos]];
  / W.5 evasive: a mouse within EVADE_R steps away from the cat (blocked by walls like any step)
  if[evasive;
    if[NMICE>1i; mice[mouseIdx]: mousePos];
    flee:{[p] v: .vision.torDelta[catPos;p;FIELD_SIZE]; n: .vision.norm v;
      $[(n>=EVADE_R)|n<0.001; p; worldOn; .vision.wrapPos[first .world.move[WT;p;MOUSE_STEP*v%n];FIELD_SIZE]; .vision.wrapPos[p+MOUSE_STEP*v%n;FIELD_SIZE]]};
    mice: flee each mice; mousePos: mice mouseIdx];
  / W.5 several mice: the one the cat sees (and is rewarded on) this bar is the nearest visible one,
  / or the nearest if none is visible
  if[NMICE>1i;
    mice[mouseIdx]: mousePos;
    md: {.vision.toroidalDist[catPos;x;FIELD_SIZE]} each mice;
    mv: $[worldOn & not seeThrough; {.world.visible[WT;catPos;catPos+.vision.torDelta[catPos;x;FIELD_SIZE]]} each mice; (count mice)#1b];
    mouseIdx: $[any mv; first (where mv) iasc md where mv; first iasc md];
    mousePos: mice mouseIdx];

  / ---- VISION: compute and set primary brain inputter ----
  / In a wrapped world the cat sees the nearest copy of the mouse across the edges
  mouseSeen: $[wrapWorld; catPos+.vision.torDelta[catPos;mousePos;FIELD_SIZE]; mousePos];
  visionSlice: .vision.visionSlice[catPos;mouseSeen;IN_LEN];
  / Phase W: the mouse is hidden in the blink cycle's hidden bars, or behind a wall. Hidden means no
  / vision input at all (the slice is logged as -1)
  visible: $[blink; BLINK[0]>(bar-1i) mod sum BLINK; 1b];
  if[worldOn & visible & not seeThrough; visible: .world.visible[WT;catPos;mouseSeen]];
  if[not visible; visionSlice: -1i];
  sinceSeen: $[visible; 0i; sinceSeen+1i];
  wVis[bar-1]: visible; wSince[bar-1]: sinceSeen;
  inp: `int$IN_LEN#0i;
  / Eyes: in a remapped world the true slice lights a different input column
  if[visionSlice>=0i; inp[$[relearn; eyePerm visionSlice; visionSlice]]: 1i];   / MEDIUM = 1i
  $[MODULAR; sightB[`inp]: inp; prim[`inp]: inp];
  / ---- W.5 DOG: its slice enters the same vision input with its own label (6), if visible ----
  if[distractor;
    dogSeen: catPos+.vision.torDelta[catPos;dogPos;FIELD_SIZE];
    dogVis: $[worldOn & not seeThrough; .world.visible[WT;catPos;dogSeen]; 1b];
    dogSlice: $[dogVis; .vision.visionSlice[catPos;dogSeen;IN_LEN]; -1i];
    if[dogSlice>=0i;
      dv: @[`int$IN_LEN#0i;dogSlice;:;1i];
      $[MODULAR; [r0: .mod.transferIn[sightB`br;sightB`inc0;sightB`inc1;IN_OFF;dv;DOG_LABEL]; sightB[`br`inc0`inc1]: 3#r0];
        [r0: .mod.transferIn[prim`br;prim`inc0;prim`inc1;IN_OFF;dv;DOG_LABEL]; prim[`br`inc0`inc1]: 3#r0]]]];
  / ---- TOUCH (modular brain): whiskers light MEDIUM, last bar's blocked direction HIGH ----
  if[MODULAR;
    tin: `int$8#0i;
    if[worldOn & not noTouch;
      tin: `int$.world.whiskers[WT;catPos;WHISKER];
      if[not null lastBlockedDir; tin[lastBlockedDir]: .trinary.HIGH]];
    touchB[`inp]: tin];

  / ---- META-BRAIN CROSS-FLOW: apply previous bar's metaBr output ----
  / right (default): overwrite the right column, rows 1-5 (rows beyond the brain are dropped).
  / top (modular test): add into the free top-row cells beside sight/touch, cols 0 1 10 11, from
  / outputs 0 1 3 4 (output 2 has no entry cell). none: no energy; the reward brain then acts only
  / through the strategic scope and magnitude.
  $[META_ENTRY=`right;
    [r0: .brain.applyThrottledMetaInput[sideMetaInp;META_INPUT_OFFSET;brainPressure;prim`br;prim`inc0];
     prim[`br]: r0 0; prim[`inc0]: r0 1];
    META_ENTRY=`top;
    [mv: sideMetaInp 0 1 3 4;
     r0: .mod.transferIn[prim`br;prim`inc0;prim`inc1;0i;(2#mv),8#0i;META_LABEL];
     r0: .mod.transferIn[r0 0;r0 1;r0 2;10i;2#2_mv;META_LABEL];
     prim[`br`inc0`inc1]: 3#r0];
    0];

  / ---- INTERNAL INPUT (W.1b; each channel opt-in) ----
  / Efference copy: the slot that moved the cat last bar, entering from above (label 7, as vision)
  if[efference & not null lastMoveSlot;
    r0: .brain.injectAdd[prim`br;prim`inc0;enlist 1i;enlist OUT_OFF+lastMoveSlot;.trinary.MEDIUM;7i];
    prim[`br]: r0 0; prim[`inc0]: r0 1];
  / Spontaneous activity: each cell gains a unit with probability SPONT, from a random direction
  / (in every brain of the modular architecture)
  spont:{[b] sp: where SPONT>(COLS*count b`br)?1f;
    r: .brain.injectAdd[b`br;b`inc0;sp div COLS;sp mod COLS;.trinary.MEDIUM;1i+(count sp)?8i];
    b[`br]: r 0; b[`inc0]: r 1; b};
  if[SPONT>0f; prim: spont prim; if[MODULAR; sightB: spont sightB; touchB: spont touchB]];

  / ---- ACCUMULATE OUTPUT ACROSS STEPS_PER_BAR CYCLES ----
  accOut: `int$OUT_LEN#0i;
  accMetaOut: `int$META_OUT_LEN#0i;

  step: 0i;
  while[step<STEPS_PER_BAR;
    / Modular: sight and touch step first, and what leaves their bottom rows enters the central
    / brain's top row this step (sight and touch share a column per direction, told apart by label)
    if[MODULAR;
      sightB: .engine.runCycle[sightB]; touchB: .engine.runCycle[touchB];
      r0: .mod.transferIn[prim`br;prim`inc0;prim`inc1;IN_OFF;sightB`out;SIGHT_LABEL];
      r0: .mod.transferIn[r0 0;r0 1;r0 2;IN_OFF;touchB`out;TOUCH_LABEL];
      prim[`br`inc0`inc1]: 3#r0];
    / Primary (single) or central (modular) brain step
    prim: .engine.runCycle[prim];
    accOut: accOut | prim`out;

    / Meta-brain: run TWICE per primary step (smaller brain, needs more steps)
    metaBr: .engine.runCycleSimple[metaBr];
    metaBr: .engine.runCycleSimple[metaBr];
    accMetaOut: accMetaOut | metaBr`out;
    / the viewer shows the whole primary side: sight, touch and central stacked (modular brain)
    if[.viz.enabled; .viz.pubStep[bar;step;$[MODULAR; sightB[`br],touchB[`br],prim`br; prim`br];metaBr`br]];

    step+:1i];

  / Copy accumulated outputs back
  prim[`out]: accOut;
  metaBr[`out]: accMetaOut;

  / Save metaBr output as side input for next bar's cross-flow
  sideMetaInp: $[ablateMeta; `int$META_INPUT_MAP_LEN#0i; accMetaOut];

  / ---- PRESSURE & DRAIN ----
  brainPressure: .brain.pressure[prim`br];
  overflow: .brain.captureOverflow[prim`br;OVERFLOW_OFFSET;OVERFLOW_MAP_LEN];

  if[brainPressure>0.1;
    drainProb: 0.15 & brainPressure*0.3;
    r1: .brain.drain[drainProb;prim`br;prim`inc0;prim`inc1];
    prim[`br]: r1 0; prim[`inc0]: r1 1; prim[`inc1]: r1 2];

  / Modular: stack the three brains again, so rewards, history and decay reach all of them
  if[MODULAR; prim: .mod.stack (sightB;touchB;prim)];

  / ---- META-BRAIN DIAGNOSTIC (every 1000 bars) ----
  if[0i=bar mod 1000i;
    show "BAR ",(string bar)," | META ENERGY: brain=",(string sum raze metaBr`br),
         " rate=",string rateCounter];

  / ---- META-BRAIN STRATEGY REINFORCEMENT ----
  / temporal scope: output[0]*40 clamped to [20,120]
  / magnitude:      output[1] clamped to [1,2]
  temporalScope: `int$ 20i | 120i & (metaBr`out)[0] * 40i;
  magnitude:     `int$ 1i  | (metaBr`out)[1];
  / --ablate-meta: behave as if meta output were all-zero (the clamps above give 20 and 1)
  if[ablateMeta; temporalScope: 20i; magnitude: 1i];

  / Strategic reinforcement rewards catching, so it is off in a FLEE world (G.4)
  if[(not noLearning)&(not fleeNow)&((noDirectRewards & rateCounter>=1i) | rateCounter>=3i);
    / Reinforce primary brain synapses active during lookback window
    / lookback: bars bar-1, bar-2, ..., bar-temporalScope (clamped to history buffer depth)
    lookback: $[noMetaRewards; `int$(); .syn.lookbackBars[bar;temporalScope;HISTORY_SIZE]];
    / Fortran applied x1.1 `magnitude` times (x1.1 or x1.21); the original q port
    / used magnitude*1.1 (x2.2) — restored to Fortran (REVIVAL_PLAN.md F17)
    mag: 1.1 xexp magnitude;
    lookbackSlots: .syn.histSlot[histIdx;bar;lookback;HISTORY_SIZE];
    prim[`syns]: .syn.applySparseLookbackScale[prim`syns;hist[lookbackSlots];.syn.scaleMult[mag;REWARD_SCALE]]];

  / ---- META-BRAIN REWARD (every bar): be full, not starving ----
  / User, 2026-10-29: "the motivation should be to ... change the system so that energy comes in on
  / the right and avoid energy coming in on the left." The meta-brain's routes used this bar are
  / reinforced when its input enters on the full side and punished on the starving side, graded:
  / full x2 reinforce, cell 3 reinforce, cell 2 neutral, cell 1 punish, starving x2 punish. Its output
  / reaches the primary brain (cross-flow, strategic scope and magnitude), so routes that help the
  / cat eat are the ones that survive. This is the meta-brain's only reward.
  if[not noLearning;
    $[satCell=4i; [metaBr[`syns]: .syn.applySparseReinforceN[metaBr`syns;metaBr`su;STEPS_PER_BAR;2i]];
      satCell=3i; [metaBr[`syns]: .syn.applySparseReinforce[metaBr`syns;metaBr`su;STEPS_PER_BAR]];
      satCell=1i; [metaBr[`syns]: .syn.applySparsePunish[metaBr`syns;metaBr`su;STEPS_PER_BAR]];
      satCell=0i; [metaBr[`syns]: .syn.applySparsePunishN[metaBr`syns;metaBr`su;STEPS_PER_BAR;2i]];
      0]];

  / ---- DECAY (every 5 bars) ----
  / G.R5 (--two-timescale): the primary brain's weights relax towards the slow memory e^M instead of
  / decaying towards the floor, then M moves a 1/TAU_SLOW step towards ln W (consolidation). After a
  / world switch, rewards move W quickly while M still holds the old world, so returning is faster.
  if[(not noLearning)&(0i=bar mod 5i);
    $[twoTS;
      [prim[`syns]: .syn.relaxDecay[prim`syns;slowM;TAU_FAST]; slowM: .syn.consolidate[prim`syns;slowM;TAU_SLOW]];
      prim[`syns]: .syn.applyDecay[prim`syns]];
    metaBr[`syns]: .syn.applyDecay[metaBr`syns]];

  / ---- STORE SYNAPSE USAGE IN HISTORY BUFFER ----
  hist[histIdx]: prim`su;
  histIdx+:1i;
  if[histIdx>=HISTORY_SIZE; histIdx: 0i];

  / ---- DETERMINE OUTPUT ACTION ----
  / Ties go to the lowest slot unless --random-tie (G.3c: with grid wiring the lowest slot was NW, F11)
  outAction: `int$ $[randomTie; {x rand count x} where (prim`out) = max prim`out; first where (prim`out) = max prim`out];
  moveDist:  (prim`out)[outAction];
  / ---- SCRIPTED BASELINE POLICY (W.0): replaces the brain's choice; one 5-unit move a bar ----
  if[scripted;
    mouseV: $[wrapWorld; .vision.torDelta[catPos;mousePos;FIELD_SIZE]; mousePos-catPos];
    polDir: $[POLICY=`planner;
      [if[not planFor~mousePos; planD: .world.distField[planFree;mousePos;FIELD_SIZE]; planFor: mousePos];
       .world.plannerDir[WT;planD;catPos;mouseV;5f;FIELD_SIZE]];
      .world.policyDir[POLICY;visible;visionSlice;mouseV;lastSeenDir;rand 2i;`int$rand 8]];
    if[visible & POLICY in `reactive`persist; lastSeenDir: polDir];
    moveDist: 1i];
  brainEnergy: `int$sum raze prim`br;
  outEnergy:   `int$sum prim`out;

  / ---- MOVE CAT ----
  currDist: prevDist;
  moveAlignment: 0n;
  if[moveDist>0i;
    / Store old position for path check
    oldCat: catPos;
    targetPos: mousePos;
    / Record movement direction in history
    movHist[movHistIdx mod 10i]: outAction;
    movHistIdx+:1i;

    / Compute movement vector
    / Wiring maps the output slot to a move direction; legs (re-learning worlds) then remap that
    / direction. Both are the identity by default.
    moveDir: $[scripted; polDir; legPerm wiringPerm outAction];
    catDx: `float$moveDirCol[moveDir] * moveDist * 5.0;
    catDy: `float$moveDirRow[moveDir] * moveDist * 5.0;
    / Wrapped world: the cat wraps; otherwise it is clamped to the box (F12)
    / Walled world: the move stops just short of the first wall in its way (end unwrapped)
    wMove: $[worldOn; .world.move[WT;oldCat;(catDx;catDy)]; (oldCat;0b)];
    blocked: wMove 1; wBlocked[bar-1]: blocked;
    lastBlockedDir: $[blocked; `int$.relearn.ANGLE_ORDER?moveDir; 0Ni];   / touch: felt on the next bar
    newX: $[worldOn; wMove[0;0] mod FIELD_SIZE; wrapWorld; (catPos[0]+catDx) mod FIELD_SIZE; 0.0 | FIELD_SIZE & catPos[0]+catDx];
    newY: $[worldOn; wMove[0;1] mod FIELD_SIZE; wrapWorld; (catPos[1]+catDy) mod FIELD_SIZE; 0.0 | FIELD_SIZE & catPos[1]+catDy];
    actualMove: $[worldOn; wMove[0]-oldCat; wrapWorld; (catDx;catDy); (newX-oldCat[0];newY-oldCat[1])];
    targetMove: $[wrapWorld; .vision.torDelta[oldCat;targetPos;FIELD_SIZE]; (targetPos[0]-oldCat[0];targetPos[1]-oldCat[1])];
    if[((.vision.norm actualMove)>0.001)&((.vision.norm targetMove)>0.001);
      moveAlignment: .vision.alignment[actualMove;targetMove];
      epochAlignSum[currentEpoch]+:moveAlignment;
      epochAlignCount[currentEpoch]+:1i];

    / ---- CATCH CHECK: path to point distance ----
    / Wrapped world: test the unwrapped path against the nearest copy of the mouse
    closestDist: $[worldOn;
      .vision.pathDist[oldCat;wMove 0;oldCat+.vision.torDelta[oldCat;mousePos;FIELD_SIZE]];
      wrapWorld;
      .vision.pathDist[oldCat;oldCat+(catDx;catDy);oldCat+.vision.torDelta[oldCat;mousePos;FIELD_SIZE]];
      .vision.pathDist[oldCat;(newX;newY);mousePos]];
    if[closestDist<2.0;
      / CATCH!
      catchesCount+:1i;
      catchesThisEpoch+:1i;
      wCaught[bar-1]: 1b;
      satiety+: 1f;                               / food: enters the meta input from next bar

      / Record catch timestamp in circular buffer
      catchTs[catchHead]: bar;
      catchHead: `int$(catchHead+1i) mod MAX_CATCHES_IN_WINDOW;
      if[catchCount<MAX_CATCHES_IN_WINDOW; catchCount+:1i];

      show "CATCH #",(string catchesCount)," at bar ",(string bar)," (dist: ",(string closestDist),")";

      / SUCCESS PATHWAY BOOST: reinforce last 20 bars in history (fold to accumulate)
      / History was already written for this bar, so slots (histIdx-1)..(histIdx-20)
      / hold the current bar and the 19 before it. Off with --no-learning (F21: it used to
      / run anyway) and in a FLEE world, where catching is the opposite of the goal (G.4).
      if[(not noLearning)&not fleeNow;
        prim[`syns]: ({[histBuf;hi;syns;j]
          / j is slots back from the write head (1 = current bar); q's mod is floored
          i: `int$(hi-j) mod HISTORY_SIZE;
          .syn.applySparseScale[syns;histBuf[i];CATCH_MULT]
        }[hist;histIdx]/)[prim`syns;1i+til 20i]];

      / Respawn mouse 30-45 units from cat
      rdx: (2*(rand 1.0))-1.0; rdy: (2*(rand 1.0))-1.0;
      rd: .vision.norm (rdx;rdy);
      if[rd>0.0; rdx:rdx%rd; rdy:rdy%rd];
      rDist: 30.0 + 15.0*(rand 1.0);
      mousePos: $[wrapWorld;
        .vision.wrapPos[(newX+rdx*rDist; newY+rdy*rDist);FIELD_SIZE];
        (0.0 | FIELD_SIZE & newX+rdx*rDist;
         0.0 | FIELD_SIZE & newY+rdy*rDist)];
      if[worldOn; if[.world.inWall[WT;mousePos;2f]; mousePos: .world.clearPos[WT;2f;ringPos[(newX;newY);FIELD_SIZE]]]];
      bestDist: .vision.toroidalDist[(newX;newY);mousePos;FIELD_SIZE];
      barsSinceProgress: 0i];

    / ---- W.5 DOG: touching it punishes the last 20 bars' routes (inverse of the catch boost) and it
    / respawns; moving towards it while it is within DOG_NEAR is punished like a wrong move ----
    if[distractor;
      dogV: .vision.torDelta[oldCat;dogPos;FIELD_SIZE];
      if[2f>.vision.pathDist[oldCat;oldCat+actualMove;oldCat+dogV];
        dogContacts+:1i;
        if[not noLearning;
          prim[`syns]: ({[histBuf;hi;syns;j] .syn.applySparseScale[syns;histBuf[`int$(hi-j) mod HISTORY_SIZE];1f%CATCH_MULT]}[hist;histIdx]/)[prim`syns;1i+til 20i]];
        dogPos: $[worldOn; .world.clearPos[WT;2f;ringPos[(newX;newY);FIELD_SIZE]]; ringPos[(newX;newY);FIELD_SIZE;0]]];
      dn: .vision.norm dogV;
      if[(not noLearning) & (dn<DOG_NEAR) & (dn>0.001) & dogSlice>=0i;
        if[0.5<.vision.dot[actualMove%.vision.norm actualMove;dogV%dn];
          prim[`syns]: .syn.applySparsePunishNL[prim`syns;prim`su;STEPS_PER_BAR;1i;REWARD_SCALE]]]];

    / Update cat position
    catPos: (newX;newY);
    currDist: .vision.toroidalDist[catPos;mousePos;FIELD_SIZE];

    / ---- DIRECTION-BASED REWARD ----
    desV: $[wrapWorld; .vision.torDelta[oldCat;mousePos;FIELD_SIZE]; mousePos-oldCat];
    desX: desV 0; desY: desV 1;
    desLen: .vision.norm (desX;desY);
    if[desLen>0.001;
      desX: desX%desLen; desY: desY%desLen;
      / Default: the move vector is not normalised (length 5-14), so dotP is not a cosine,
      / adaptThresh (0.6-0.85) is not a real cone, and one good move fires ~15 reinforce calls
      / (F10). --cos-reward normalises the move, so dotP is the cosine of the angle to the mouse:
      / adaptThresh becomes a true cone and round(1+3*dotP) gives 1-4 calls.
      dotP: $[cosReward;
              .vision.dot[(catDx;catDy)%.vision.norm (catDx;catDy);(desX;desY)];
              .vision.dot[(catDx;catDy);(desX;desY)]];

      / Track directionality
      totalMoves+:1i;
      $[dotP>0.01;  [movesTowards+:1i; towardsEpoch+:1i];
        dotP<-0.01; [movesAway+:1i;    awayEpoch+:1i];
                    [movesPerp+:1i;    perpEpoch+:1i]];

      / Adaptive threshold update
      barsSinceProgress+:1i;
      if[barsSinceProgress>=PROGRESS_INTERVAL;
        $[currDist<bestDist*0.9;
          [adaptThresh: THRESH_MAX & adaptThresh+0.03;
           bestDist: currDist; barsSinceProgress: 0i];
          barsSinceProgress>=2i*PROGRESS_INTERVAL;
          [adaptThresh: THRESH_MIN | adaptThresh-0.02;
           barsSinceProgress: PROGRESS_INTERVAL];
          0]];

      / Momentum tracking
      $[outAction=lastRewDir; [momStreak+:1i]; [momStreak: 0i]];
      momBonus: `int$ 4i & momStreak+1i;

      / Direction multiplier
      / Goal-relative direction score: equals dotP when chasing; negated in a FLEE world (G.4)
      goalDot: $[relearn; goalSign*dotP; dotP];
      dirMult: $[goalDot>adaptThresh; 0.5+1.5*goalDot; 1.0];

      / Progressive punishment epoch scaling
      epochProg: `float$currentEpoch % numEpochs;

      / Phase W: with --hidden-reward none there is no direct reward while the mouse is hidden
      dirReward: (not noLearning)&(not noDirectRewards)&visible|HIDDEN_REWARD=`cos;

      / G.R2 policy-gradient direct reward (replaces the threshold/count rule below when on)
      if[dirReward&pgUpdate;
        pgR: .vision.dot[(catDx;catDy)%.vision.norm (catDx;catDy);(desX;desY)];   / move cosine
        if[relearn; pgR*: goalSign];                                               / G.4 goal
        pgDelta: $[rpe; pgR-rBar; pgR];
        prim[`syns]: .syn.applySparsePG[prim`syns;prim`su;.dirs.vBias;PG_ETA;pgDelta];
        if[rpe; rBar+: RPE_RATE*pgR-rBar]];

      if[dirReward&not pgUpdate;
        / Reward: accurate direction; Punish: wrong/perpendicular direction
        $[goalDot>adaptThresh;
          [reinforceCalls:`int$`float$dirMult*2.0;
           reinforceCalls+:momBonus-1i;
           prim[`syns]: .syn.applySparseReinforceNL[prim`syns;prim`su;STEPS_PER_BAR;reinforceCalls;REWARD_SCALE];
           lastRewDir: outAction;
           ];
          goalDot<-0.01;
          [punishCalls:`int$1.0+epochProg*2.0;
           prim[`syns]: .syn.applySparsePunishNL[prim`syns;prim`su;STEPS_PER_BAR;punishCalls;REWARD_SCALE];
           lastRewDir: 0i; momStreak: 0i];
          currentEpoch>0i;
          [prim[`syns]: .syn.applySparsePunishNL[prim`syns;prim`su;STEPS_PER_BAR;1i;REWARD_SCALE];
           lastRewDir: 0i; momStreak: 0i];
          0]]]];

  / G.4: goal alignment this bar (null if no move) = move cosine x goal sign
  if[relearn; relearnGA[bar-1]: goalSign*moveAlignment];
  lastMoveSlot: $[moveDist>0i; outAction; 0Ni];   / --efference: what the brain commanded this bar
  if[moveDist<=0i; lastBlockedDir: 0Ni];           / no move, nothing blocked

  if[NMICE>1i; mice[mouseIdx]: mousePos];
  / I.2: brain state at the end of the bar, with what was last seen
  if[logState;
    if[visible; lastSeenSlice: visionSlice];
    stBr[bar-1]: raze prim`br; stInc[bar-1]: raze prim`inc0; stSeen[bar-1]: lastSeenSlice];

  / ---- APPEND TO LOG TABLE ----
  alignSlot: (bar-1i) mod 500i;
  if[not null alignmentWindow[alignSlot];
    alignmentWindowSum-:alignmentWindow[alignSlot];
    alignmentWindowCount-:1i];
  alignmentWindow[alignSlot]:moveAlignment;
  if[not null moveAlignment;
    alignmentWindowSum+:moveAlignment;
    alignmentWindowCount+:1i];
  / Record only the first crossing (acquisitionTime starts null)
  if[(bar>=500i)&(null acquisitionTime)&(alignmentWindowCount>0i)&((alignmentWindowSum%alignmentWindowCount)>0.6);
    acquisitionTime:bar];

  simLog,:flip `bar`mouseX`mouseY`catX`catY`visionSlice`brainEnergy`outEnergy`outAction`moveDist`alignment`catches`pressure`overflow!
    (enlist bar; enlist `int$mousePos[0]; enlist `int$mousePos[1];
     enlist `int$catPos[0]; enlist `int$catPos[1];
     enlist visionSlice;
     enlist brainEnergy; enlist outEnergy;
     enlist outAction; enlist moveDist;
     enlist moveAlignment;
     enlist catchesCount;
     enlist `float$brainPressure;
     enlist `int$sum overflow);

  if[.viz.enabled;
    .viz.pubBar[bar;mousePos;catPos;(visionSlice;brainEnergy;outAction;catchesCount;moveAlignment;`float$brainPressure)]];

  / ---- PERIODIC SNAPSHOT ----
  if[0i=bar mod SNAPSHOT_INTERVAL;
    show "========================================";
    show "SNAPSHOT AT BAR ",string bar;
    show "========================================";
    show "Mouse: ",", " sv string `int$mousePos;
    show "Cat:   ",", " sv string `int$catPos;
    show "Brain state (6x12):";
    show prim`br;
    show "Outputter: ",", " sv string prim`out;
    show "Brain energy: ",string brainEnergy;
    show "Catches: ",string catchesCount;
    show "Rate: ",string rateCounter;
    show "Meta output: ",", " sv string metaBr`out;
    show "Pressure: ",string brainPressure];

  if[0i=bar mod 1000i; show "Progress: ",(string bar),"/",(string MAX_BARS)];

  bar+:1i];

.viz.close[]

/ ================================================================
/ SAVE FINAL EPOCH
/ ================================================================
/ Always save (previously skipped when the last epoch had 0 catches, losing its move counts)
epochCatches[currentEpoch]: catchesThisEpoch;
epochTowards[currentEpoch]: towardsEpoch;
epochAway[currentEpoch]: awayEpoch;
epochPerp[currentEpoch]: perpEpoch;

/ ================================================================
/ SAVE RESULTS (kdb-x tables → CSV)
/ ================================================================
/ Helper: write table t to file dir/name.csv
saveCsv:{[dir;name;t] (hsym `$":",dir,name) 0: csv 0: t}

/ Simulation log (bar-by-bar timeseries — kdb-x native table)
saveCsv[outputDir;"simulation_log.csv";simLog]

/ Brain state (row-major order matching raze prim`br)
brState: flip `row`col`state!
  (`int$raze {COLS # x} each `int$til ROWS;
   `int$raze ROWS # enlist `int$til COLS;
   raze prim`br)
saveCsv[outputDir;"brain_state.csv";brState]

/ Synapse state (max strength across incoming directions for each connection)
synRows: raze {[r]
  raze {[r;c]
    {[r;c;d]
      nr:r+`int$.dirs.dr[d]; nc:c+`int$.dirs.dc[d];
      sc: (prim`syns)[r;c];
      valid: ((nr>=0i)&(nr<ROWS)&(nc>=0i)&(nc<COLS)) | (r=ROWS-1i)&(nr=ROWS);
      if[valid;
        maxStr: max sc[;d];                        / max over incoming directions
        domInDir: `int$ 1+first where sc[;d]=maxStr;
        enlist `from_row`from_col`to_row`to_col`strength`dom_in_dir!(r;c;nr;nc;maxStr;domInDir)]
    } each til 8i}[r;] each til COLS
  } each til ROWS
/ Filter out :: (null) entries from invalid directions, build table from row-dicts
synRows: synRows where 99h = type each synRows;
synState: flip (flip synRows)
saveCsv[outputDir;"synapse_state.csv";synState]

/ Meta-brain state (row-major order matching raze metaBr`br)
metaBrState: flip `row`col`state!
  (`int$raze {META_COLS # x} each `int$til META_ROWS;
   `int$raze META_ROWS # enlist `int$til META_COLS;
   raze metaBr`br)
saveCsv[outputDir;"meta_brain_state.csv";metaBrState]

/ ================================================================
/ SAVE BINARY WEIGHTS (kdb-x serialization — loads with get)
/ ================================================================
weightPath: hsym `$":",outputDir,"weights_seed",(string userSeed),".bin"
histDisk: .syn.sparseHistToDense[hist;ROWS;COLS];
weightPath set (prim`syns; metaBr`syns; prim`br; prim`inc0; prim`inc1;
                metaBr`br; metaBr`inc0; metaBr`inc1; histDisk; histIdx),$[twoTS; enlist slowM; ()]
show "Binary weights saved to: ",string weightPath

/ ================================================================
/ SUMMARY OUTPUT
/ ================================================================
show "=== SIMULATION COMPLETE ==="
show "Total catches: ",string catchesCount
show "Catch rate per bar: ",string `float$catchesCount % MAX_BARS

/ Epoch breakdown (using kdb-x query!)
/ numEpochs+1 slots: full epochs 0..numEpochs-1, plus partial final epoch at slot numEpochs.
/ Keep only epochs that actually ran; `bars` is the epoch length (the last may be partial).
nRun: (MAX_BARS+EPOCH_SIZE-1i) div EPOCH_SIZE
epochSummary: nRun#([] epoch:1i+til numEpochs+1i;
               bars:`int$EPOCH_SIZE & MAX_BARS-EPOCH_SIZE*til numEpochs+1i;
               catches:epochCatches; towards:epochTowards; away:epochAway; perp:epochPerp;
               meanAlignment:epochAlignSum%epochAlignCount)
show "=== TEMPORAL LEARNING (EPOCH BREAKDOWN) ==="
show epochSummary
saveCsv[outputDir;"epoch_summary.csv";epochSummary]

/ Learning trend: early third vs late third of the full epochs (needs >= 3 full epochs)
third: numEpochs div 3i
earlyAvg: 0n; lateAvg: 0n
if[third>0i;
  earlyAvg: avg epochCatches til third;
  lateAvg:  avg epochCatches (2i*third)+til numEpochs-2i*third;
  show "Early third avg: ",string earlyAvg;
  show "Late third avg:  ",string lateAvg;
  show $[lateAvg>earlyAvg*1.2; "TEMPORAL IMPROVEMENT DETECTED";
         lateAvg>earlyAvg;      "Slight improvement";
                                 "No temporal improvement"]]

show ""
show "=== MOVEMENT DIRECTIONALITY ==="
show "Total moves:   ",string totalMoves
show "Towards mouse: ",string movesTowards
show "Away:          ",string movesAway
show "Perpendicular: ",string movesPerp
if[totalMoves>0i;
  show "% towards: ",string `float$100i*movesTowards%totalMoves]

/ kdb-x analytics bonus: in-session queries on the log
show ""
show "=== kdb-x ANALYTICS (live on simulation_log table) ==="
/ Catches per epoch: `catches` is cumulative, so sum its per-bar deltas within each epoch
/ (epoch numbering matches epochSummary: bars 1..EPOCH_SIZE are epoch 1)
catchPerEpoch: select catches:sum d by epoch:1i+(bar-1i) div EPOCH_SIZE from update d:deltas catches from simLog
show "Catches per epoch (from simLog):"
show catchPerEpoch
/ Average pressure
show "Average brain pressure: ",string avg simLog`pressure

/ One-row machine-readable run summary, read by q/programs/compare.q
summary: enlist `seed`bars`directRewards`metaRewards`ablateMeta`noLearning`weightFile`catches`totalMoves`towards`away`perp`meanPressure`meanAlignment`acquisitionTime`cosReward`rewardScale`pgUpdate`pgEta`rpe`rpeRate`hebb`relearnSchedule`wrapWorld`weightRatio`wiring`wiringPerm`world`blink`policy`hiddenReward`seeThrough`visibleFrac`alignVisible`alignHidden`blockedFrac`catchTimeMed`efference`spontaneous`meanSatietyCell`randomTie`mouseStep`fieldSize`twoTimescale`tauFast`tauSlow`architecture`centralRows`touch`metaEntry`mice`evasive`distractor`dogContacts`layoutEvery`satEdges!
  (userSeed; MAX_BARS; (not noDirectRewards)&(not noLearning); (not noMetaRewards)&(not noLearning); ablateMeta; noLearning; `$weightFile;
  catchesCount; totalMoves; movesTowards; movesAway; movesPerp; avg simLog`pressure; avg simLog`alignment; acquisitionTime; cosReward; REWARD_SCALE; pgUpdate; PG_ETA; rpe; RPE_RATE; .brain.HEBB; `$relearnSpec; wrapWorld; WEIGHT_RATIO; WIRING; `$raze string wiringPerm;
  WORLD; `$blinkSpec; POLICY; HIDDEN_REWARD; seeThrough; avg wVis; avg (simLog`alignment) where wVis; avg (simLog`alignment) where not wVis;
  avg wBlocked where 0i<simLog`moveDist; $[any wCaught; `float$med deltas 0,1+where wCaught; 0n];
  efference; SPONT; avg `float$satCells; randomTie; MOUSE_STEP; FIELD_SIZE; twoTS; TAU_FAST; TAU_SLOW; $[MODULAR; `modular; `single]; $[MODULAR; CENTRAL_ROWS; 0Ni]; MODULAR & not noTouch; $[MODULAR; META_ENTRY; `right]; NMICE; evasive; distractor; dogContacts; LAYOUT_EVERY; `$"|" sv string .brain.SAT_EDGES)
saveCsv[outputDir;"summary.csv";summary]

/ G.4 re-learning outputs: the per-bar goal-alignment log and one metrics row per block
/ (row 0 = learning the first world from scratch; later rows = each switch)
if[relearn;
  relearnLog: ([] bar:1i+til MAX_BARS; world:(relearnSched 0) relearnEnds binr 1i+til MAX_BARS; goalAlignment:relearnGA);
  saveCsv[outputDir;"relearn_log.csv";relearnLog];
  switchesT: .relearn.switchTable[relearnGA;relearnSched];
  saveCsv[outputDir;"switches.csv";switchesT];
  -1 "=== RE-LEARNING SWITCHES (G.4) ===";
  show switchesT]
/ Phase W per-bar record: visibility, bars since the mouse was last seen, blocked moves, catches
if[logState;
  (hsym `$outputDir,"state_log.bin") set `bar`visible`sinceSeen`lastSeen`br`inc0!(1i+til MAX_BARS;wVis;wSince;stSeen;stBr;stInc)]
if[wActive;
  worldLog: ([] bar:1i+til MAX_BARS; visible:wVis; sinceSeen:wSince; moved:0i<simLog`moveDist;
    blocked:wBlocked; alignment:simLog`alignment; caught:wCaught);
  saveCsv[outputDir;"world_log.csv";worldLog]]
show "Summary written to: ",outputDir,"summary.csv"
