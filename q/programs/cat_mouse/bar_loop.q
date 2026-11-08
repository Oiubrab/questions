/ cat_mouse.q part: the bar loop: world, vision, touch, brain steps, rewards, move, catch, log
/ Loaded by q/programs/cat_mouse.q (run from the repo root); not a standalone program. The run's state is global,
/ so the parts share variables exactly as the single script did.

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
  / ---- MOUSE MOVEMENT (every MOUSE_MOVE_INTERVAL bars; stepM is defined in state.q) ----
  if[0i=bar mod MOUSE_MOVE_INTERVAL;
    mousePos: stepM mousePos;
    if[NMICE>1i; mice[mouseIdx]: mousePos; mice: @[mice;(til count mice) except mouseIdx;stepM]];
    if[distractor; dogPos: stepM dogPos]];
  / W.5 evasive: a mouse within EVADE_R steps away from the cat (flee, defined in state.q)
  / The current mouse's position (its random step, or its respawn after a catch) is copied into mice
  / first, for one mouse as for several: otherwise a single evasive mouse kept fleeing from where it
  / was caught, next to the cat, and was caught again every bar (fixed 2026-11-03)
  if[evasive;
    mice[mouseIdx]: mousePos;
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

  / Strategic reinforcement rewards catching, so it is off in a FLEE world (G.4). It needs STRAT_GATE catches in the
  / last 200 bars (--strategic-gate; default 3); a high gate keeps it from cancelling punishment while hunting is poor
  if[(not noLearning)&(not fleeNow)&((noDirectRewards & rateCounter>=1i) | rateCounter>=STRAT_GATE);
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
      prim[`syns]: .syn.applyDecayBy[prim`syns;PRIM_DECAY 0;PRIM_DECAY 1]];   / --decay-mult scales the loss
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
          / Bad move. --punish-calls N (catch-rate plan): N calls from bar 1, so a wrong route's punishment
          / can beat the reinforcement every used route gets (nP > C, CATCH_RATE_ANALYSIS.md §3.1)
          goalDot<-0.01;
          [punishCalls:$[null PUNISH_CALLS; `int$1.0+epochProg*2.0; PUNISH_CALLS];
           prim[`syns]: .syn.applySparsePunishNL[prim`syns;prim`su;STEPS_PER_BAR;punishCalls;REWARD_SCALE];
           lastRewDir: 0i; momStreak: 0i];
          / Neutral move: 1 call from bar 2,001 by default; with --punish-calls, N calls from bar 1
          (currentEpoch>0i)|not null PUNISH_CALLS;
          [prim[`syns]: .syn.applySparsePunishNL[prim`syns;prim`su;STEPS_PER_BAR;$[null PUNISH_CALLS;1i;PUNISH_CALLS];REWARD_SCALE];
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
