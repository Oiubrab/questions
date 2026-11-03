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
