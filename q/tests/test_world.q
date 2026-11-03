/ Obstacle worlds (.world, OBSTACLES_PLAN.md): wall hits, blocked moves, line of sight,
/ wrap-around copies, clear spawns.
/ Run from repo root: q q/tests/test_world.q

\l q/load.q
\l q/modules/world/world.q
\l q/tests/harness.q

-1 "test_world";
tol:1e-9;
B: .world.tile[.world.rects enlist 40 40 60 60;100f];     / one 20x20 block in the middle
P: .world.tile[.world.LAYOUTS`pillars;100f];

/ Segment-wall hits
.t.near["hit: straight at the block's face";.world.hitT[B;30 50f;20 0f];0.5;tol];
.t.eq["miss: passes beside the block";.world.hitT[B;30 30f;40 0f];0w];
.t.eq["miss: stops before the block";.world.hitT[B;30 50f;5 0f];0w];
.t.eq["sliding along a face is not a hit";.world.hitT[B;30 40f;40 0f];0w];
.t.near["diagonal hit at the corner region";.world.hitT[B;30 30f;20 20f];0.5;tol];
.t.near["vertical move (zero x component) hits";.world.hitT[B;50 30f;0 20f];0.5;tol];
.t.eq["vertical move beside the block misses";.world.hitT[B;35 30f;0 40f];0w];
.t.eq["no walls: never hits";.world.hitT[.world.tile[.world.NONE;100f];10 10f;50 50f];0w];
/ Wrap: the wall's copy one tile over is hit by a segment that leaves the field
W: .world.tile[.world.rects enlist 0 40 4 60;100f];
.t.near["a segment crossing the east edge hits the wall's wrapped copy";.world.hitT[W;98 50f;10 0f];0.2;tol];

/ Blocked moves stop short of the wall and stay outside it
m: .world.move[B;30 50f;20 0f];
.t.ok["blocked move is flagged";m 1];
.t.near["blocked move stops 0.05 short of the wall";m 0;39.95 50f;1e-9];
.t.ok["stopped cat is outside the wall";not .world.inWall[B;m 0;0f]];
.t.ok["next move into the wall is blocked at once";.world.hitT[B;m 0;5 0f]<0.02];
.t.eq["free move is unchanged";.world.move[B;30 30f;5 5f];(35 35f;0b)];

/ Line of sight
.t.ok["block hides a mouse behind it";not .world.visible[B;30 50f;70 50f]];
.t.ok["clear line is visible";.world.visible[B;30 30f;70 30f]];
.t.ok["pillars hide along the diagonal";not .world.visible[P;10 10f;40 40f]];
.t.ok["pillars: open corridor is visible";.world.visible[P;10 50f;90 50f]];

/ Walls and spawns
.t.ok["inWall inside";.world.inWall[B;50 50f;0f]];
.t.ok["inWall margin";.world.inWall[B;39 50f;2f]];
.t.ok["inWall outside";not .world.inWall[B;30 50f;2f]];
.t.ok["inWall margin reaches a wall across the edge (tiled copy)";.world.inWall[W;99 50f;2f]];
.t.ok["rooms: the edge door at (25, 2) is open";not .world.inWall[.world.tile[.world.LAYOUTS`rooms;100f];25 2f;0f]];
system "S 3";
cs: {.world.clearPos[P;2f;{100*2?1f}]} each til 200;
.t.ok["clearPos never lands in or near a wall";not any .world.inWall[P;;2f] each cs];
.t.ok["layouts are all 4 columns of equal length";all {(4=count x)&1=count distinct count each x} each value .world.LAYOUTS];
.t.ok["layout walls lie inside the field";all {all (raze x) within 0 100} each value .world.LAYOUTS];
system "S 5"; rl: .world.randomLayout[6;100f];
.t.ok["random layout: 6 blocks inside the field, sides 6-20";(6=count rl 0)&(all (raze rl) within 0 100)&all ((rl[2]-rl 0),rl[3]-rl 1) within 6 20];

/ Rooms: a door lets the cat through; the wall beside it doesn't
R: .world.tile[.world.LAYOUTS`rooms;100f];
.t.eq["rooms: through the door at y 50";.world.hitT[R;20 50f;10 0f];0w];
.t.ok["rooms: wall at y 30 blocks";.world.hitT[R;20 30f;10 0f]<1f];
.t.eq["rooms: the door at the field edge (y 0) is open";.world.hitT[R;20 0.5f;10 0f];0w];

/ Whiskers (touch): which of the 8 angle-ordered directions has a wall within 3 units
.t.eq["whiskers: a wall 2 units east lights E, and the diagonals SE/NE that also reach it";where .world.whiskers[B;38 50f;3f];0 1 7];
.t.eq["whiskers: a wall exactly at reach doesn't count (touching isn't a hit)";where .world.whiskers[B;37 50f;3f];`long$()];
.t.eq["whiskers: nothing within reach in open space";any .world.whiskers[B;20 20f;3f];0b];

/ Scripted policies (W.0): .world.policyDir[pol;visible;slice;displacement;last;u2;u8]
pd: .world.policyDir;
.t.eq["greedy heads at the mouse, even when hidden (east -> .dirs 4)";pd[`greedy;0b;-1i;10 0f;0Ni;0i;3i];4i];
.t.eq["greedy: south-east displacement -> SE (.dirs 7)";pd[`greedy;1b;1i;10 10f;0Ni;0i;3i];7i];
.t.eq["reactive: visible slice 0 -> E or SE (u2 picks)";(pd[`reactive;1b;0i;10 1f;0Ni;0i;3i];pd[`reactive;1b;0i;10 1f;0Ni;1i;3i]);4 7i];
.t.eq["reactive: slice 7 wraps to NE then E";(pd[`reactive;1b;7i;10 -1f;0Ni;0i;3i];pd[`reactive;1b;7i;10 -1f;0Ni;1i;3i]);2 4i];
.t.eq["reactive: hidden -> the random draw";pd[`reactive;0b;-1i;10 0f;5i;0i;3i];3i];
.t.eq["persist: hidden -> keeps its last heading";pd[`persist;0b;-1i;10 0f;5i;0i;3i];5i];
.t.eq["persist: hidden with no heading yet -> random";pd[`persist;0b;-1i;10 0f;0Ni;0i;3i];3i];
.t.eq["random -> the draw";pd[`random;1b;0i;10 0f;0Ni;1i;6i];6i];

/ Planner: geodesic distance field around walls, and the move it picks
FR: .world.freeGrid[B;100f;50;1f];
.t.ok["free grid: a cell inside the block is blocked, one outside is free";(not FR[25;25])&FR[10;10]];
D: .world.distField[FR;30 50f;100f];
.t.near["distance field: 0 at the goal";.world.fieldAt[D;30 50f;100f];0f;1e-9];
.t.ok["distance field: around the block is longer than the straight line";.world.fieldAt[D;70 50f;100f]>40f];
.t.ok["distance field: open straight line ~ its length (cell size 2)";abs[.world.fieldAt[D;30 20f;100f]-30f]<3f];
.t.eq["distance field: blocked cells are unreachable";.world.fieldAt[D;50 50f;100f];0w];
pdir: .world.plannerDir[.world.tile[.world.rects enlist 40 40 60 60;100f];D;70 50f;-40 0f;5f;100f];
.t.ok["planner behind the block doesn't push into it (east of it, mouse west: never W)";not pdir=3i];
.t.eq["planner catches a mouse within reach";.world.plannerDir[B;D;30 30f;4 0f;5f;100f];4i];

.t.done[];
