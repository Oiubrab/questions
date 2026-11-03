/ cat_mouse.q part: command-line flags: parse, validate, defaults
/ Loaded by q/programs/cat_mouse.q (run from the repo root); not a standalone program. The run's state is global,
/ so the parts share variables exactly as the single script did.

/ ================================================================
/ PARSE COMMAND LINE ARGUMENTS (flags documented at top of file)
/ ================================================================
args: .z.x
KNOWN_FLAGS: ("--seed";"--bars";"--output-dir";"--load-weights";
              "--meta-only";"--no-direct-rewards";"--no-meta-rewards";"--ablate-meta";"--no-learning";"--cos-reward";"--reward-scale";"--pg-update";"--pg-eta";"--rpe";"--rpe-rate";"--hebb";"--relearn-schedule";"--wrap-world";"--weight-ratio";"--wiring";"--world";"--see-through";"--blink";"--hidden-reward";"--policy";"--efference";
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
/ Obstacle worlds (plans/completed/OBSTACLES_PLAN.md): harder worlds and scripted baseline agents. All off by default.
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
/ --spontaneous (random resting activity) was removed on 2026-11-03 at the user's request: at P = 0.05 it
/ swamped vision (researchNotes/OBSTACLES_RESULTS.md). Its replacement, a sparse clock-like input at a
/ fixed spot, is plans/future/METRONOME_PLAN.md. An old --spontaneous now aborts as an unknown flag.
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
