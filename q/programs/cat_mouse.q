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
/   --cos-reward          direction reward uses the cosine of the move angle (REVIVAL_PLAN.md G.3a)
/   --reward-scale L      multiply every primary-brain reward's log-step by L (default 1; G.3b)
/   --pg-update           direct reward becomes a policy-gradient update in log space (G.R2)
/   --pg-eta E            step size for --pg-update (default 0.3)
/   --rpe                 with --pg-update: reward = cosine minus its running average (G.R1)
/   --rpe-rate B          running-average rate for --rpe (default 0.01, ~100 moves)
/   --hebb N              unconditional per-use reinforcement of a route (default 1000; G.R3)
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

/ ================================================================
/ PARSE COMMAND LINE ARGUMENTS (flags documented at top of file)
/ ================================================================
args: .z.x
KNOWN_FLAGS: ("--seed";"--bars";"--output-dir";"--load-weights";
              "--meta-only";"--no-direct-rewards";"--no-meta-rewards";"--ablate-meta";"--no-learning";"--cos-reward";"--reward-scale";"--pg-update";"--pg-eta";"--rpe";"--rpe-rate";"--hebb";"--relearn-schedule";
              "--viz-port";"--viz-delay-ms")
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
cosReward: hasFlag "--cos-reward"
/ G.3(b): rewards multiply weights, so they are steps in log space (reinforce +0.80, punish -0.84,
/ catch boost +0.41 per bar, strategic +0.095*m per lookback bar). REWARD_SCALE multiplies every
/ primary-brain step by L, keeping the same RNG draws; 1 is the original behaviour exactly.
REWARD_SCALE: $[count flagVal "--reward-scale"; "F"$flagVal "--reward-scale"; 1f]
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
.brain.HEBB: $[count flagVal "--hebb"; "I"$flagVal "--hebb"; 1000i]
if[(null .brain.HEBB)|.brain.HEBB<=0i; -2 "--hebb must be a positive integer (unconditional reinforcement is never removed)"; exit 1]
.brain.hebbEnabled: not noLearning
outputDir: flagVal "--output-dir"
if[0=count outputDir; outputDir: "results/seed",string userSeed]
if[not last[outputDir]="/"; outputDir,:"/"]

/ ================================================================
/ SIMULATION PARAMETERS (match Fortran exactly)
/ ================================================================
ROWS: 6i; COLS: 12i
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
  relearnGA: MAX_BARS#0n]                     / goal alignment per bar (null = no move)
/ Current world's legs, eyes and goal sign (identity / chase when no schedule)
legPerm: `int$til 8; eyePerm: `int$til 8; goalSign: 1f; fleeNow: 0b; worldNow: `A
STEPS_PER_BAR: 12i
SNAPSHOT_INTERVAL: 1000i
MOUSE_MOVE_INTERVAL: 10i
FIELD_SIZE: 100.0
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
  if[not (dims each w)~dims each expect;
    -2 "Weight file shape mismatch: got ",(.Q.s1 dims each w),", want ",.Q.s1 dims each expect;
    exit 1];
  prim[`syns`br`inc0`inc1]: w 0 2 3 4;
  metaBr[`syns`br`inc0`inc1]: w 1 5 6 7;
  hist: .syn.denseHistToSparse w 8; histIdx: w 9;
  show "Loaded warm state; primary brain energy: ",string sum raze prim`br]

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
catPos: (0.0 | FIELD_SIZE & mousePos[0]+dx*catDist;
         0.0 | FIELD_SIZE & mousePos[1]+dy*catDist)

prevDist: .vision.toroidalDist[catPos;mousePos;FIELD_SIZE]
bestDist: prevDist

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
  metaInp: `int$META_IN_LEN#0i;
  $[rateCounter>=1i;
    $[rateCounter<=5i;
      [metaInp[META_IN_LEN-rateCounter]: 1i];    / MEDIUM
      rateCounter<=10i;
      [metaInp[META_IN_LEN-(rateCounter-5i)]: 2i]; / HIGH
      [metaInp[0i]: 2i]];                          / saturate
    0];
  metaBr[`inp]: metaInp;

  / ---- MEASURE DISTANCE BEFORE ACTION ----
  prevDist: .vision.toroidalDist[catPos;mousePos;FIELD_SIZE];

  / ---- RESET SYNAPSE USAGE ----
  prim[`su]: .syn.emptySparseUsage[];
  metaBr[`su]: .syn.emptySparseUsage[];

  / ---- MOUSE MOVEMENT (every MOUSE_MOVE_INTERVAL bars) ----
  if[0i=bar mod MOUSE_MOVE_INTERVAL;
    mousePos: .vision.moveMouse[mousePos;FIELD_SIZE;2.0]];

  / ---- VISION: compute and set primary brain inputter ----
  visionSlice: .vision.visionSlice[catPos;mousePos;IN_LEN];
  inp: `int$IN_LEN#0i;
  / Eyes: in a remapped world the true slice lights a different input column
  if[visionSlice>=0i; inp[$[relearn; eyePerm visionSlice; visionSlice]]: 1i];   / MEDIUM = 1i
  prim[`inp]: inp;

  / ---- META-BRAIN CROSS-FLOW: apply previous bar's metaBr output to right column ----
  r0: .brain.applyThrottledMetaInput[sideMetaInp;META_INPUT_OFFSET;brainPressure;prim`br;prim`inc0];
  prim[`br]: r0 0; prim[`inc0]: r0 1;

  / ---- ACCUMULATE OUTPUT ACROSS STEPS_PER_BAR CYCLES ----
  accOut: `int$OUT_LEN#0i;
  accMetaOut: `int$META_OUT_LEN#0i;

  step: 0i;
  while[step<STEPS_PER_BAR;
    / Primary brain step
    prim: .engine.runCycle[prim];
    accOut: accOut | prim`out;

    / Meta-brain: run TWICE per primary step (smaller brain, needs more steps)
    metaBr: .engine.runCycleSimple[metaBr];
    metaBr: .engine.runCycleSimple[metaBr];
    accMetaOut: accMetaOut | metaBr`out;
    if[.viz.enabled; .viz.pubStep[bar;step;prim`br;metaBr`br]];

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
    prim[`syns]: .syn.applySparseLookbackScale[prim`syns;hist[lookbackSlots];.syn.scaleMult[mag;REWARD_SCALE]];

    / Reward metaBr-brain for triggering successful strategy reinforcement
    metaBr[`syns]: .syn.applySparseReinforceN[metaBr`syns;metaBr`su;STEPS_PER_BAR;rateCounter]];

  / ---- META-BRAIN INPUT-BASED REWARD (every bar) ----
  if[not noLearning;
    $[rateCounter>=3i;
      [metaBr[`syns]: .syn.applySparseReinforceN[metaBr`syns;metaBr`su;STEPS_PER_BAR;2i]];
      rateCounter>=1i;
      [metaBr[`syns]: .syn.applySparseReinforce[metaBr`syns;metaBr`su;STEPS_PER_BAR]];
      not noDirectRewards;
      [metaBr[`syns]: .syn.applySparsePunish[metaBr`syns;metaBr`su;STEPS_PER_BAR]];
      0]];

  / ---- DECAY (every 5 bars) ----
  if[(not noLearning)&(0i=bar mod 5i);
    prim[`syns]: .syn.applyDecay[prim`syns];
    metaBr[`syns]: .syn.applyDecay[metaBr`syns]];

  / ---- STORE SYNAPSE USAGE IN HISTORY BUFFER ----
  hist[histIdx]: prim`su;
  histIdx+:1i;
  if[histIdx>=HISTORY_SIZE; histIdx: 0i];

  / ---- DETERMINE OUTPUT ACTION ----
  outAction: `int$ first where (prim`out) = max prim`out;
  moveDist:  (prim`out)[outAction];
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
    / Legs: in a remapped world the output slot drives a different move direction
    moveDir: $[relearn; legPerm outAction; outAction];
    catDx: `float$moveDirCol[moveDir] * moveDist * 5.0;
    catDy: `float$moveDirRow[moveDir] * moveDist * 5.0;
    newX: 0.0 | FIELD_SIZE & catPos[0]+catDx;
    newY: 0.0 | FIELD_SIZE & catPos[1]+catDy;
    actualMove: (newX-oldCat[0];newY-oldCat[1]);
    targetMove: (targetPos[0]-oldCat[0];targetPos[1]-oldCat[1]);
    if[((.vision.norm actualMove)>0.001)&((.vision.norm targetMove)>0.001);
      moveAlignment: .vision.alignment[actualMove;targetMove];
      epochAlignSum[currentEpoch]+:moveAlignment;
      epochAlignCount[currentEpoch]+:1i];

    / ---- CATCH CHECK: path to point distance ----
    closestDist: .vision.pathDist[oldCat;(newX;newY);mousePos];
    if[closestDist<2.0;
      / CATCH!
      catchesCount+:1i;
      catchesThisEpoch+:1i;

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

      / Extra metaBr reward on hunting streak
      if[(not noLearning)&(rateCounter>=5i);
        metaBr[`syns]: .syn.applySparseReinforceN[metaBr`syns;metaBr`su;STEPS_PER_BAR;3i]];

      / Respawn mouse 30-45 units from cat
      rdx: (2*(rand 1.0))-1.0; rdy: (2*(rand 1.0))-1.0;
      rd: .vision.norm (rdx;rdy);
      if[rd>0.0; rdx:rdx%rd; rdy:rdy%rd];
      rDist: 30.0 + 15.0*(rand 1.0);
      mousePos: (0.0 | FIELD_SIZE & newX+rdx*rDist;
                 0.0 | FIELD_SIZE & newY+rdy*rDist);
      bestDist: .vision.toroidalDist[(newX;newY);mousePos;FIELD_SIZE];
      barsSinceProgress: 0i];

    / Update cat position
    catPos: (newX;newY);
    currDist: .vision.toroidalDist[catPos;mousePos;FIELD_SIZE];

    / ---- DIRECTION-BASED REWARD ----
    desX: mousePos[0]-oldCat[0]; desY: mousePos[1]-oldCat[1];
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

      / G.R2 policy-gradient direct reward (replaces the threshold/count rule below when on)
      if[(not noLearning)&(not noDirectRewards)&pgUpdate;
        pgR: .vision.dot[(catDx;catDy)%.vision.norm (catDx;catDy);(desX;desY)];   / move cosine
        if[relearn; pgR*: goalSign];                                               / G.4 goal
        pgDelta: $[rpe; pgR-rBar; pgR];
        prim[`syns]: .syn.applySparsePG[prim`syns;prim`su;.dirs.vBias;PG_ETA;pgDelta];
        if[rpe; rBar+: RPE_RATE*pgR-rBar]];

      if[(not noLearning)&(not noDirectRewards)&not pgUpdate;
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
                metaBr`br; metaBr`inc0; metaBr`inc1; histDisk; histIdx)
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
summary: enlist `seed`bars`directRewards`metaRewards`ablateMeta`noLearning`weightFile`catches`totalMoves`towards`away`perp`meanPressure`meanAlignment`acquisitionTime`cosReward`rewardScale`pgUpdate`pgEta`rpe`rpeRate`hebb`relearnSchedule!
  (userSeed; MAX_BARS; (not noDirectRewards)&(not noLearning); (not noMetaRewards)&(not noLearning); ablateMeta; noLearning; `$weightFile;
  catchesCount; totalMoves; movesTowards; movesAway; movesPerp; avg simLog`pressure; avg simLog`alignment; acquisitionTime; cosReward; REWARD_SCALE; pgUpdate; PG_ETA; rpe; RPE_RATE; .brain.HEBB; `$relearnSpec)
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
show "Summary written to: ",outputDir,"summary.csv"
