/ cat_mouse.q part: constants, RNG seed, output wiring, walls, viewer
/ Loaded by q/programs/cat_mouse.q (run from the repo root); not a standalone program. The run's state is global,
/ so the parts share variables exactly as the single script did.

/ ================================================================
/ SIMULATION PARAMETERS (match Fortran exactly)
/ ================================================================
/ ROWS is the stacked primary state: sight, touch and central brains (modular) or the single brain
ROWS: $[MODULAR; (2i*SENSE_ROWS)+CENTRAL_ROWS; 6i]; COLS: 12i
IN_OFF: 2i; IN_LEN: 8i      / inputter offset (0-indexed) and length
OUT_OFF: 2i; OUT_LEN: 8i    / outputter offset and length
MAX_BARS: $[not null MAX_BARS_ARG; MAX_BARS_ARG; 20000i]
/ G.4 re-learning schedule: the run is a sequence of worlds (q/modules/world/relearn.q). Each world
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

/ Strategic reinforcement (meta-brain outputs -> scope and magnitude) and the catch boost window, in bars
STRAT_SCOPE_MIN: 20i; STRAT_SCOPE_MAX: 120i; STRAT_SCOPE_PER_OUT: 40i   / scope = MIN | MAX & out[0] * PER_OUT
STRAT_MAG_MIN: 1i              / magnitude = MIN | out[1]
STRAT_MULT: 1.1                / strategic multiplier per lookback bar, per magnitude step, before the reward scale
CATCH_WINDOW: 20i              / bars of history the catch boost reaches back
/ The meta-brain keeps its own clock whatever the primary brain's steps per bar: 24 cycles a bar, spread evenly over
/ the primary steps (2 per step at 12 steps), and its rewards use the step count 12, as when both were tied to 12.
META_STEPS_PER_BAR: 24i
META_REWARD_STEPS: 12i
RATE_WINDOW: 200i
MAX_CATCHES_IN_WINDOW: 200i
HISTORY_SIZE: 100i             / synapse history circular buffer depth

/ ================================================================
/ CONSTANTS FROM THE EQUATIONS (Phase R; researchNotes/CONSTANT_SCALING.md; revival plan §2 point 10)
/ Each is derived from this brain's configuration unless its own flag was given. Today's brain gives
/ 12 steps per bar, 4 punish calls, hebb 252, primary decay x0.5.
/ ================================================================
/ Steps per bar: a bar passes the same share of an untrained vision unit's energy as the tested brain does in 12 steps
SCALE_GEOM: `modular`senseRows`centRows`nC`inOff`inLen`outOff`outLen!(MODULAR;SENSE_ROWS;$[MODULAR;CENTRAL_ROWS;ROWS];COLS;IN_OFF;IN_LEN;OUT_OFF;OUT_LEN)
SCALE_FREF: (.scale.exitCdf[.scale.REF;.dirs.vBias;.scale.REF_STEPS]) .scale.REF_STEPS-1
STEPS_PER_BAR: $[null STEPS_ARG; .scale.steps[.scale.exitCdf[SCALE_GEOM;.dirs.vBias;.scale.HORIZON];SCALE_FREF]; STEPS_ARG]
SCALE_FBAR: last .scale.exitCdf[SCALE_GEOM;.dirs.vBias;STEPS_PER_BAR]   / share this brain passes per bar (recorded)
/ Punish calls: punishment per wrong move beats the undirected gain every used route gets (nP >= margin x C)
PUNISH_STEP: .scale.punishStep[STEPS_PER_BAR;REWARD_SCALE]
COMMON_MODE: .scale.commonMode[REWARD_SCALE;STRAT_SCOPE_MIN;STRAT_MAG_MIN;STRAT_MULT;CATCH_WINDOW;CATCH_REF;CATCH_BOOST]
if[PUNISH_RULE=`derived; PUNISH_CALLS: .scale.punishCalls[PUNISH_STEP;COMMON_MODE;SEP_MARGIN]]
/ Hebb: lifts a punished route off the floor by HEBB_MARGIN x its punishment (re-learning limit); 250 with the legacy rule
if[null .brain.HEBB; .brain.HEBB: $[PUNISH_RULE=`legacy; 250i; .scale.hebb[.syn.MIN_STRENGTH;PUNISH_CALLS*PUNISH_STEP;HEBB_MARGIN]]]
/ Primary decay: 0.5 of the original loss per slice visit, so 0.5 x 8 / vision slices per bar
if[null DECAY_MULT; DECAY_MULT: 0.5*8f%IN_LEN]
PRIM_DECAY: $[DECAY_MULT=1f; (.syn.DECAY_LO;.syn.DECAY_SPAN); (1f-DECAY_MULT*1f-.syn.DECAY_LO;DECAY_MULT*.syn.DECAY_SPAN)]
/ Meta cycles in each primary step: 24 spread evenly over the steps (2 each at 12 steps)
META_SCHED: `int$deltas floor (META_STEPS_PER_BAR*1+til STEPS_PER_BAR)%STEPS_PER_BAR
-1 "Derived constants: steps per bar ",(string STEPS_PER_BAR)," (passes ",(string SCALE_FBAR)," of input per bar; reference ",(string SCALE_FREF),
  "), punish calls ",$[null PUNISH_CALLS;"legacy rule";string PUNISH_CALLS]," (nP ",(string PUNISH_CALLS*PUNISH_STEP)," vs common mode ",(string COMMON_MODE),
  "), hebb ",(string .brain.HEBB),", decay x",string DECAY_MULT;

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
