/ cat_mouse.q part: end of run: CSVs, weights file, summary, logs
/ Loaded by q/programs/cat_mouse.q (run from the repo root); not a standalone program. The run's state is global,
/ so the parts share variables exactly as the single script did.

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

/ One-row machine-readable run summary, read by q/programs/analysis/compare.q
summary: enlist `seed`bars`directRewards`metaRewards`ablateMeta`noLearning`weightFile`catches`totalMoves`towards`away`perp`meanPressure`meanAlignment`acquisitionTime`cosReward`rewardScale`pgUpdate`pgEta`rpe`rpeRate`hebb`relearnSchedule`wrapWorld`weightRatio`wiring`wiringPerm`world`blink`policy`hiddenReward`seeThrough`visibleFrac`alignVisible`alignHidden`blockedFrac`catchTimeMed`efference`meanSatietyCell`randomTie`mouseStep`fieldSize`twoTimescale`tauFast`tauSlow`architecture`centralRows`touch`metaEntry`mice`evasive`distractor`dogContacts`layoutEvery`satEdges`punishCalls`strategicGate`floor`decayMult`stepsPerBar!
  (userSeed; MAX_BARS; (not noDirectRewards)&(not noLearning); (not noMetaRewards)&(not noLearning); ablateMeta; noLearning; `$weightFile;
  catchesCount; totalMoves; movesTowards; movesAway; movesPerp; avg simLog`pressure; avg simLog`alignment; acquisitionTime; cosReward; REWARD_SCALE; pgUpdate; PG_ETA; rpe; RPE_RATE; .brain.HEBB; `$relearnSpec; wrapWorld; WEIGHT_RATIO; WIRING; `$raze string wiringPerm;
  WORLD; `$blinkSpec; POLICY; HIDDEN_REWARD; seeThrough; avg wVis; avg (simLog`alignment) where wVis; avg (simLog`alignment) where not wVis;
  avg wBlocked where 0i<simLog`moveDist; $[any wCaught; `float$med deltas 0,1+where wCaught; 0n];
  efference; avg `float$satCells; randomTie; MOUSE_STEP; FIELD_SIZE; twoTS; TAU_FAST; TAU_SLOW; $[MODULAR; `modular; `single]; $[MODULAR; CENTRAL_ROWS; 0Ni]; MODULAR & not noTouch; $[MODULAR; META_ENTRY; `right]; NMICE; evasive; distractor; dogContacts; LAYOUT_EVERY; `$"|" sv string .brain.SAT_EDGES; PUNISH_CALLS; STRAT_GATE; FLOOR; DECAY_MULT; STEPS_PER_BAR)
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

