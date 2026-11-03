/ cat_mouse.q part: brains, history, warm start, lesions, positions, counters, per-bar helpers
/ Loaded by q/programs/cat_mouse.q (run from the repo root); not a standalone program. The run's state is global,
/ so the parts share variables exactly as the single script did.

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
/ I.4 lesion: zero every route that leads into each listed stacked cell (.mod.lesion)
if[count lesionSpec;
  prim[`syns]: .mod.lesion[MODULAR;SENSE_ROWS;ROWS;COLS;prim`syns;lesionSpec];
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
/ PER-BAR HELPERS (defined once; they read the run's globals when called)
/ ================================================================
/ The mouse's random step. In a walled world it is blocked by walls like the cat's
stepM:{[p] $[worldOn;
    [ma: 2f*.vision.PI*rand 1.0; .vision.wrapPos[first .world.move[WT;p;MOUSE_STEP*(cos ma;sin ma)];FIELD_SIZE]];
    .vision.moveMouse[p;FIELD_SIZE;MOUSE_STEP]]};
/ W.5 evasive: a mouse within EVADE_R steps away from the cat (blocked by walls like any step)
flee:{[p] v: .vision.torDelta[catPos;p;FIELD_SIZE]; n: .vision.norm v;
  $[(n>=EVADE_R)|n<0.001; p; worldOn; .vision.wrapPos[first .world.move[WT;p;MOUSE_STEP*v%n];FIELD_SIZE]; .vision.wrapPos[p+MOUSE_STEP*v%n;FIELD_SIZE]]};
