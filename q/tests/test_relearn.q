/ Re-learning test machinery (REVIVAL_PLAN.md G.4): worlds, schedule parsing, switch metrics
/ Run from repo root: q q/tests/test_relearn.q

\l q/load.q
\l q/modules/relearn.q
\l q/tests/harness.q

-1 "test_relearn";
W:.relearn.WORLDS; dr:.dirs.dr; dc:.dirs.dc;

/ Worlds
.t.eq["A is the identity on legs and eyes, chase goal";W`A;(`int$til 8;`int$til 8;1f)];
.t.ok["L180 sends every slot the opposite way";(dc W[`L180;0];dr W[`L180;0])~(neg dc;neg dr)];
.t.ok["L90 rotates every move by exactly 90 degrees";(dc W[`L90;0];dr W[`L90;0])~(neg dr;dc)];
.t.ok["L90 is a permutation";all (asc W[`L90;0])=til 8];
.t.eq["E180 shifts each vision slice by 4 (180 degrees)";W[`E180;1];`int$(4+til 8) mod 8];
.t.eq["FLEE flips the goal sign only";W`FLEE;(`int$til 8;`int$til 8;-1f)];

/ Schedule parsing
.t.eq["parse a schedule";.relearn.parseSchedule "A:100,L180:50,FLEE:25";(`A`L180`FLEE;100 50 25i)];
bad:{@[.relearn.parseSchedule;x;{`bad}]};
.t.eq["unknown world is rejected";bad "A:100,XYZ:5";`bad];
.t.eq["missing length is rejected";bad "A:100,L180";`bad];
.t.eq["non-positive length is rejected";bad "A:0";`bad];

/ Metrics on a synthetic series: block 1 = 1000 bars at 0.7; block 2 = 300 bars at -0.5 then 0.7
ga: (1000#0.7),(300#-0.5),1700#0.7;
m: .relearn.blockMetrics[ga;1001;3000;1;1000];
.t.near["pre-switch level";m`pre;0.7;1e-9];
.t.near["trough (first 200 bars after the switch)";m`trough;-0.5;1e-9];
.t.near["drop";m`drop;1.2;1e-9];
.t.eq["half-recovery time: rolling-100 reaches 0.1 after 50 good bars";m`tHalf;350i];
.t.eq["time to 0.6: rolling-500 has at most 41 bad bars";m`t06;759i];
.t.near["recovered level and fraction";(m`recovered;m`recFrac);(0.7;1f);1e-9];
/ never recovers within the block -> censored (null)
m2: .relearn.blockMetrics[(1000#0.7),2000#-0.5;1001;3000;1;1000];
.t.ok["no recovery gives null tHalf and t06";(null m2`tHalf)&null m2`t06];
/ a switch that doesn't hurt is flagged, with no recovery time
m3: .relearn.blockMetrics[3000#0.7;1001;3000;1;1000];
.t.ok["small drop is flagged and tHalf is null";(m3`smallDrop)&null m3`tHalf];
/ nulls (non-move bars) are ignored by the rolling means
.t.near["rollMean ignores nulls";last .relearn.rollMean[4;1 0n 3 0n 5f];4f;1e-9];
/ first-block row (learning from scratch) has no pre-switch level
st: .relearn.switchTable[ga;(`A`L180;1000 2000i)];
.t.eq["switch table has one row per block";count st;2];
.t.ok["block 0 row: no from-world, no pre level";(null first st`fromWorld)&null first st`pre];
.t.eq["block 1 row records the transition";(st[1]`fromWorld;st[1]`toWorld;st[1]`bar);(`A;`L180;1001i)];

.t.done[];
