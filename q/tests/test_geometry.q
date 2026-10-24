/ Geometry, vision and history-index maths — guards against the right-to-left
/ precedence bugs found in the 2026-10 audit (REVIVAL_PLAN.md F1-F3)
/ Run from repo root: q q/tests/test_geometry.q

\l q/load.q
\l q/tests/harness.q

-1 "test_geometry";
tol:1e-9;

/ Vector helpers
.t.near["norm 3-4-5";.vision.norm 3 4f;5f;tol];
.t.near["dot";.vision.dot[1 2f;3 4f];11f;tol];

/ Toroidal distance
.t.near["toroidalDist 3-4-5";.vision.toroidalDist[0 0f;3 4f;100f];5f;tol];
.t.near["toroidalDist wraps both axes";.vision.toroidalDist[1 1f;99 99f;100f];sqrt 8f;tol];
.t.near["toroidalDist symmetric";.vision.toroidalDist[99 99f;1 1f;100f];sqrt 8f;tol];

/ Path-to-point distance (catch test). The old version returned 0 for a far mouse.
.t.near["pathDist far mouse is far";.vision.pathDist[0 0f;10 0f;5 30f];30f;tol];
.t.near["pathDist near mouse";.vision.pathDist[0 0f;10 0f;5 1f];1f;tol];
.t.near["pathDist clamps before start";.vision.pathDist[0 0f;10 0f;-3 4f];5f;tol];
.t.near["pathDist clamps past end";.vision.pathDist[0 0f;10 0f;13 4f];5f;tol];
.t.near["pathDist zero-length path";.vision.pathDist[0 0f;0 0f;3 4f];5f;tol];
.t.near["pathDist diagonal";.vision.pathDist[0 0f;10 10f;0 10f];sqrt 50f;tol];
.t.ok["pathDist never null";not null .vision.pathDist[50 50f;55 55f;20 80f]];

/ Vision slices: every octant reachable, including the SW quadrant (old bug: -1)
pts:(60 50f;60 60f;50 60f;40 60f;40 50f;40 40f;50 40f;60 40f);
.t.eq["visionSlice centred in each octant";.vision.visionSlice[50 50f;;8] each 50 50f+/:{10*(cos x;sin x)} each (0.5+til 8)*2*.vision.PI%8;`int$til 8];
.t.eq["visionSlice on octant boundaries";.vision.visionSlice[50 50f;;8] each pts;0 1 2 3 4 5 6 7i];
.t.eq["visionSlice just below 2*PI is last slice (old bug: 8 -> crash)";.vision.visionSlice[50 50f;60 49.9999999f;8];7i];
.t.eq["visionSlice identical positions";.vision.visionSlice[50 50f;50 50f;8];-1i];
rs:.vision.visionSlice[50 50f;;8] each 100*1000 2#2000?1.0;
.t.ok["visionSlice in 0..7 over random points";rs within 0 7];
.t.ok["visionSlice reaches every slice over random points";all (til 8) in rs];
.t.eq["buildInputter one-hot";.vision.buildInputter[50 50f;60 60f;8];0 1 0 0 0 0 0 0i];

/ Direction reward sign: a NW move towards a NE target must score negative (old bug: +14)
.t.ok["dot of opposed vectors is negative";0>.vision.dot[-5 -5f;(sqrt 0.5)*1 1f]];

/ History buffer indexing
.t.eq["lookbackBars recent first";.syn.lookbackBars[500i;5i;100i];499 498 497 496 495i];
.t.eq["lookbackBars stops at bar 1";.syn.lookbackBars[3i;5i;100i];2 1i];
.t.eq["lookbackBars limited by history depth";count .syn.lookbackBars[500i;120i;100i];99];
.t.eq["histSlot previous bar is head-1";.syn.histSlot[10i;500i;499i;100i];9i];
.t.eq["histSlot wraps below 0";.syn.histSlot[0i;500i;499i;100i];99i];
.t.eq["histSlot oldest kept bar";.syn.histSlot[10i;500i;401i;100i];11i];

.t.done[];
