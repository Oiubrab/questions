/ Cat-Mouse Learning Simulation — q/kdb-x port
/ Mirrors src/programs/cat_mouse_learning.f90

/ Usage:
/   q q/programs/cat_mouse.q              (random seed)
/   q q/programs/cat_mouse.q 42           (seed 42)
/   q q/programs/cat_mouse.q 42 --no-direct-rewards
/   q q/programs/cat_mouse.q 42 --load-weights weights_seed42.bin
/   q q/programs/cat_mouse.q 42 --output-dir results/run1/

/ Load all modules first
\l q/modules/trinary.q
\l q/modules/directions.q
\l q/modules/synapses.q
\l q/modules/brain.q
\l q/modules/vision.q
\l q/modules/brain_engine.q

/ ================================================================
/ PARSE COMMAND LINE ARGUMENTS
/ Usage: q q/load.q q/programs/cat_mouse.q -- --seed 42 --bars 20000
/        q q/load.q q/programs/cat_mouse.q -- --seed 42 --meta-only
/        q q/load.q q/programs/cat_mouse.q -- --seed 42 --load-weights results/w.q
/ ================================================================
args: .z.x
/ Seed: --seed N (preferred) or first positional arg that is not a flag
seedIdx: first where args like "--seed"
userSeed: $[not null seedIdx;
  "I"$args seedIdx+1;
  (0<count args) & not (first args) like "--*";
  "I"$first args;
  `int$.z.t]
/ Bars override
barsIdx: first where args like "--bars"
MAX_BARS_ARG: $[not null barsIdx; "I"$args barsIdx+1; 0Ni]
loadWeights: any args like "--load-weights"
noDirectRewards: (any args like "--no-direct-rewards") | any args like "--meta-only"
weightFile: $[loadWeights; args 1+first where args like "--load-weights"; ""]
outputDir: $[any args like "--output-dir"; args 1+first where args like "--output-dir"; ""]
if[(count outputDir)&not last[outputDir]="/"; outputDir,:"/"]

/ ================================================================
/ SIMULATION PARAMETERS (match Fortran exactly)
/ ================================================================
ROWS: 6i; COLS: 12i
IN_OFF: 2i; IN_LEN: 8i      / inputter offset (0-indexed) and length
OUT_OFF: 2i; OUT_LEN: 8i    / outputter offset and length
MAX_BARS: $[not null MAX_BARS_ARG; MAX_BARS_ARG; 20000i]
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

/ ================================================================
/ INITIALIZE BRAIN SYSTEMS
/ ================================================================
/ Primary brain (6x12)
prim: .engine.initSystem[ROWS;COLS;IN_LEN;OUT_LEN;IN_OFF;OUT_OFF]

/ Meta-brain (3x5)
metaBr: .engine.initSystem[META_ROWS;META_COLS;META_IN_LEN;META_OUT_LEN;META_IN_OFF;META_OUT_OFF]

/ Synapse history: HISTORY_SIZE x rows x cols x 8 x 8 boolean
hist: .syn.initHist[HISTORY_SIZE;ROWS;COLS]
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

calcRateCounter:{[bar;catchTs;catchCount;window]
  / Count how many catch timestamps are within window bars of current bar
  sum[bar - catchTs[til catchCount] <= window]}

/ ================================================================
/ LOAD WEIGHTS (if --load-weights provided)
/ ================================================================
if[loadWeights;
  show "Loading weights from: ",weightFile;
  / kdb-x binary read: load from file using get
  / Format: (syns; meta_syns; br; inc0_combined; meta_br; meta_inc0_combined; hist; histIdx)
  / Note: Fortran binary format is not directly compatible; placeholder for future work
  show "WARNING: Fortran binary weight format not yet supported — starting fresh"
  ]

/ ================================================================
/ INITIALIZE POSITIONS
/ ================================================================
/ Mouse starts at center
mousePos: FIELD_SIZE*0.5 0.5

/ Cat starts 30-45 units from center in random direction
dx: (2*(rand 1.0))-1.0; dy: (2*(rand 1.0))-1.0
d:sqrt[dx*dx+dy*dy]
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
catchesThisEpoch: 0i; towardsEpoch: 0i; awayEpoch: 0i; perpEpoch: 0i
currentEpoch: 0i    / 0-indexed

brainPressure: 0.0

/ ================================================================
/ CSV LOG: use kdb-x tables for simulation log
/ ================================================================
/ Simulation log as a kdb-x table — this is the killer feature vs Fortran
/ We build it incrementally and can query it in real time
simLog: flip `bar`mouseX`mouseY`catX`catY`visionSlice`brainEnergy`outEnergy`outAction`moveDist`catches`pressure`overflow!
  (`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`int$();`float$();`int$())
/ Note: we append each bar's data; at end we write CSV

/ ================================================================
/ MAIN SIMULATION LOOP
/ ================================================================
show "=== CAT & MOUSE CONTINUOUS HUNTING SIMULATION (q/kdb-x) ==="
show "Max bars: ",string MAX_BARS
show "Steps per bar: ",string STEPS_PER_BAR
show "Seed: ",string userSeed
show "Direct rewards: ",string not noDirectRewards
show ""

bar: 1i
while[bar<=MAX_BARS;

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
  prim[`su]: .syn.resetSU[prim`su];
  metaBr[`su]: .syn.resetSU[metaBr`su];

  / ---- MOUSE MOVEMENT (every MOUSE_MOVE_INTERVAL bars) ----
  if[0i=bar mod MOUSE_MOVE_INTERVAL;
    mousePos: .vision.moveMouse[mousePos;FIELD_SIZE;2.0]];

  / ---- VISION: compute and set primary brain inputter ----
  visionSlice: .vision.visionSlice[catPos;mousePos;IN_LEN];
  inp: `int$IN_LEN#0i;
  if[visionSlice>=0i; inp[visionSlice]: 1i];   / MEDIUM = 1i
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

    step+:1i];

  / Copy accumulated outputs back
  prim[`out]: accOut;
  metaBr[`out]: accMetaOut;

  / Save metaBr output as side input for next bar's cross-flow
  sideMetaInp: accMetaOut;

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

  if[(noDirectRewards & rateCounter>=1i) | rateCounter>=3i;
    / Reinforce primary brain synapses active during lookback window
    / lookback: bars bar-1, bar-2, ..., bar-temporalScope (clamped to history buffer depth)
    lookback: `int$bar-1i - til temporalScope;
    lookback: lookback where (lookback>=1i) & ((bar-lookback)<HISTORY_SIZE);
    mag: `float$magnitude * 1.1;
    / Fold reinforcement across all lookback bars — must fold to accumulate syns updates
    prim[`syns]: ({[mag;histBuf;hi;barN;syns;b]
      / 0-indexed history slot for bar b:
      /   hi is the NEXT write slot, so bar b is at slot (hi - (barN-b)) mod HISTORY_SIZE
      j: `int$ barN - b;
      i: `int$(hi - j + HISTORY_SIZE) mod HISTORY_SIZE;
      hSlice: histBuf[i];
      / Apply mag*1.0 reinforcement to synapses active (hc=1b) in this bar
      {[sc;hc;m] sc + `int$hc*(.syn.MAX_STRENGTH & `int$`float$sc*m) - `int$hc*sc}
      [;;mag]''[syns;hSlice]
    }[mag;hist;histIdx;bar]/) [prim`syns; lookback];

    / Reward metaBr-brain for triggering successful strategy reinforcement
    do[rateCounter;
      metaBr[`syns]: .syn.applyReinforce[metaBr`syns;metaBr`su;STEPS_PER_BAR]]];

  / ---- META-BRAIN INPUT-BASED REWARD (every bar) ----
  $[rateCounter>=3i;
    [do[2; metaBr[`syns]: .syn.applyReinforce[metaBr`syns;metaBr`su;STEPS_PER_BAR]]];
    rateCounter>=1i;
    [metaBr[`syns]: .syn.applyReinforce[metaBr`syns;metaBr`su;STEPS_PER_BAR]];
    not noDirectRewards;
    [metaBr[`syns]: .syn.applyPunish[metaBr`syns;metaBr`su;STEPS_PER_BAR]];
    0];

  / ---- DECAY (every 5 bars) ----
  if[0i=bar mod 5i;
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
  if[moveDist>0i;
    / Store old position for path check
    oldCat: catPos;
    / Record movement direction in history
    movHist[movHistIdx mod 10i]: outAction;
    movHistIdx+:1i;

    / Compute movement vector
    catDx: `float$moveDirCol[outAction] * moveDist * 5.0;
    catDy: `float$moveDirRow[outAction] * moveDist * 5.0;
    newX: 0.0 | FIELD_SIZE & catPos[0]+catDx;
    newY: 0.0 | FIELD_SIZE & catPos[1]+catDy;

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
      / Bars ago: 1..20, corresponding to history slots (histIdx-1)..(histIdx-20)
      prim[`syns]: ({[histBuf;hi;syns;j]
        / j is bars-ago (1..20), slot = (hi - j + HISTORY_SIZE) mod HISTORY_SIZE
        i: `int$(hi - j + HISTORY_SIZE) mod HISTORY_SIZE;
        hSlice: histBuf[i];
        {[sc;hc] sc + `int$hc*(.syn.MAX_STRENGTH & `int$`float$sc*1.5) - `int$hc*sc}
        ''[syns;hSlice]
      }[hist;histIdx]/) [prim`syns; 1i+til 20i];

      / Extra metaBr reward on hunting streak
      if[rateCounter>=5i;
        do[3; metaBr[`syns]: .syn.applyReinforce[metaBr`syns;metaBr`su;STEPS_PER_BAR]]];

      / Respawn mouse 30-45 units from cat
      rdx: (2*(rand 1.0))-1.0; rdy: (2*(rand 1.0))-1.0;
      rd: sqrt[rdx*rdx+rdy*rdy];
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
    desLen: sqrt[desX*desX+desY*desY];
    if[desLen>0.001;
      desX: desX%desLen; desY: desY%desLen;
      dotP: catDx*desX + catDy*desY;

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
      dirMult: $[dotP>adaptThresh; 0.5+1.5*dotP; 1.0];

      / Progressive punishment epoch scaling
      epochProg: `float$currentEpoch % numEpochs;

      if[not noDirectRewards;
        / Reward: accurate direction; Punish: wrong/perpendicular direction
        $[dotP>adaptThresh;
          [do[`int$`float$dirMult*2.0;
             prim[`syns]: .syn.applyReinforce[prim`syns;prim`su;STEPS_PER_BAR]];
           lastRewDir: outAction;
           do[momBonus-1i;
             prim[`syns]: .syn.applyReinforce[prim`syns;prim`su;STEPS_PER_BAR]]];
          dotP<-0.01;
          [do[`int$1.0+epochProg*2.0;
             prim[`syns]: .syn.applyPunish[prim`syns;prim`su;STEPS_PER_BAR]];
           lastRewDir: 0i; momStreak: 0i];
          currentEpoch>0i;
          [prim[`syns]: .syn.applyPunish[prim`syns;prim`su;STEPS_PER_BAR];
           lastRewDir: 0i; momStreak: 0i];
          0]]]];

  / ---- APPEND TO LOG TABLE ----
  simLog,:flip `bar`mouseX`mouseY`catX`catY`visionSlice`brainEnergy`outEnergy`outAction`moveDist`catches`pressure`overflow!
    (enlist bar; enlist `int$mousePos[0]; enlist `int$mousePos[1];
     enlist `int$catPos[0]; enlist `int$catPos[1];
     enlist visionSlice;
     enlist brainEnergy; enlist outEnergy;
     enlist outAction; enlist moveDist;
     enlist catchesCount;
     enlist `float$brainPressure;
     enlist `int$sum overflow);

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

/ ================================================================
/ SAVE FINAL EPOCH
/ ================================================================
if[catchesThisEpoch>0i;
  epochCatches[currentEpoch]: catchesThisEpoch;
  epochTowards[currentEpoch]: towardsEpoch;
  epochAway[currentEpoch]: awayEpoch;
  epochPerp[currentEpoch]: perpEpoch];

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
weightPath set (prim`syns; metaBr`syns; prim`br; prim`inc0; prim`inc1;
                metaBr`br; metaBr`inc0; metaBr`inc1; hist; histIdx)
show "Binary weights saved to: ",string weightPath

/ ================================================================
/ SUMMARY OUTPUT
/ ================================================================
show "=== SIMULATION COMPLETE ==="
show "Total catches: ",string catchesCount
show "Catch rate per bar: ",string `float$catchesCount % MAX_BARS

/ Epoch breakdown (using kdb-x query!)
/ numEpochs+1 slots: full epochs 0..numEpochs-1, plus partial final epoch at slot numEpochs
epochSummary: ([] epoch:1+til numEpochs+1i; catches:epochCatches;
               towards:epochTowards; away:epochAway; perp:epochPerp)
show "=== TEMPORAL LEARNING (EPOCH BREAKDOWN) ==="
show epochSummary

/ Learning trend: early third vs late third
earlyAvg: `float$(sum epochCatches[til numEpochs div 3i]) % numEpochs div 3i
lateAvg:  `float$(sum epochCatches[(2i*numEpochs div 3i)+til (numEpochs - 2i*numEpochs div 3i)]) % numEpochs - 2i*numEpochs div 3i
show "Early third avg: ",string earlyAvg
show "Late third avg:  ",string lateAvg
show $[lateAvg>earlyAvg*1.2; "TEMPORAL IMPROVEMENT DETECTED";
       lateAvg>earlyAvg;      "Slight improvement";
                               "No temporal improvement"]

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
/ Catches per epoch
catchPerEpoch: select catches:max catches - prev catches by epoch:bar div EPOCH_SIZE from simLog
show "Catch rate acceleration (last epoch vs first):"
show select first catches, last catches from catchPerEpoch
/ Average pressure
show "Average brain pressure: ",string avg simLog`pressure
