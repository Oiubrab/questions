/ Re-learning report (REVIVAL_PLAN.md G.4): how well and how fast each configuration re-learns.
/ Usage (repo root), one group per configuration, groups separated by "+" (q itself rejects a bare "--"):
/   q q/programs/relearn_report.q SCHED_ARM [FRESH_ARM ...] + SCHED_ARM2 [FRESH_ARM ...] < /dev/null
/ SCHED_ARM: runs made with a multi-block --relearn-schedule. FRESH_ARMs: same configuration,
/ single-world schedules (e.g. "L180:10000"); they give learning-from-scratch times per world.
/ The first group is the reference for paired comparisons (matched by seed and switch number).

\c 60 250
system "S 42"   / fixed seed: reproducible bootstrap CIs

a: .z.x
if[0=count a; -2 "usage: q q/programs/relearn_report.q SCHED_ARM [FRESH_ARM ...] [+ SCHED_ARM2 ...]"; exit 1]
groups: {x where not x~\:enlist "+"} each (0,where a~\:enlist "+") cut a
groups: groups where 0<count each groups
strip: {$["/"=last x; -1_x; x]}
cfgName: {[g] `$last "/" vs strip first g}

SW_TYPES: `switch`bar`fromWorld`toWorld`pre`trough`drop`smallDrop`tHalf`t06`recovered`recFrac!"IISSFFFBIIFF"
loadArm:{[d]
  d: strip d;
  subs: {x where x like "s*"} string key hsym `$d;
  subs: subs where {[d;s] `switches.csv in key hsym `$d,"/",s}[d] each subs;
  if[0=count subs; -2 "no switches.csv under ",d; exit 1];
  t: raze {[d;s] f: hsym `$d,"/",s,"/switches.csv";
     update seed:"I"$1_s from ((SW_TYPES `$"," vs first read0 f);enlist ",") 0: f}[d] each subs;
  update arm:`$last "/" vs d from t}

mednn: {med x where not null x}     / median ignoring nulls (censored / undefined)

-1 "pre = level before the switch; trough = first 200 bars after (against the new goal); drop = pre - trough";
-1 "tHalf = bars to win back half the drop (rolling 100; floor 100); t06 = bars until rolling-500 >= 0.6";
-1 "recovered = last 1,000 bars of the block; recFrac = share of the drop won back; Cens = not reached in the block";

report:{[g]
  sw: loadArm first g;
  fresh: $[1<count g; raze loadArm each 1_g; 0#sw];
  -1 ""; -1 "################ CONFIGURATION ",(string cfgName g)," ################";
  -1 "=== Every switch (time medians over seeds; level means) ===";
  show select seeds:count i, pre:avg pre, trough:avg trough, drop:avg drop, smallDrop:sum smallDrop,
     tHalfMed:mednn tHalf, tHalfCens:sum (null tHalf)&(not smallDrop)&not null drop, t06Med:mednn t06, t06Cens:sum null t06,
     recovered:avg recovered, recFrac:avg recFrac
     by switch, fromWorld, toWorld from sw;
  / Learning each world from scratch = row 0 of every arm in the group (schedule arm + controls)
  frT: 0!select seeds:count i, t06Med:mednn t06, t06Cens:sum null t06, recovered:avg recovered by toWorld from (sw,fresh) where switch=0;
  -1 "=== Learning each world from scratch (row 0: a fresh brain) ===";
  show frT;
  freshT06: exec toWorld!t06Med from frT;
  -1 "=== Re-learning ratio = median t06 after the switch / median t06 learning that world fresh ===";
  -1 "    (> 1: old habits interfere; < 1: the brain re-uses what it knows; null: no fresh control)";
  rr: 0!select t06Med:mednn t06 by switch, fromWorld, toWorld from sw where switch>0;
  show update freshT06:freshT06 toWorld, ratio:t06Med%freshT06 toWorld from rr;
  -1 "=== Savings: successive returns to A vs learning A the first time ===";
  show select switch:first switch, tHalfMed:mednn tHalf, t06Med:mednn t06, t06Cens:sum null t06, recovered:avg recovered
     by returnNo:1+{(distinct x)?x} switch from `switch xasc select from sw where (toWorld=`A)&switch>0;
  -1 "    first acquisition of A (row 0): t06 median ",string exec mednn t06 from sw where switch=0;
  sw}

swAll: report each groups;

/ Paired comparison of each configuration against the first, matched by seed and switch.
/ Only pairs where both values are defined are compared; censoring counts are in the tables above.
boot:{[d] asc {[d;x] avg d (count d)?count d}[d] each til 5000}
cmp:{[ref;oth;col]
  r: `seed`switch xkey ?[ref;enlist (>;`switch;0);0b;`seed`switch`toWorld`r!(`seed;`switch;`toWorld;col)];
  o: `seed`switch xkey ?[oth;enlist (>;`switch;0);0b;`seed`switch`o!(`seed;`switch;col)];
  j: 0!r ij o;
  {[j;s] k: j where j[`switch]=s; d: `float$k[`o]-k`r; d: d where not null d;
    b: $[count d; boot d; enlist 0n];
    `switch`toWorld`pairs`meanDiff`ci95lo`ci95hi!(s; first k`toWorld; count d; avg d; b `long$0.025*count b; b `long$0.975*count b)}[j] each asc distinct j`switch}
if[1<count groups;
  -1 ""; -1 "################ PAIRED vs ",(string cfgName first groups)," (other minus reference) ################";
  {[ref;oth]
    -1 "--- ",string first oth`arm;
    -1 "tHalf (bars; negative = faster re-learning; small drops excluded):";
    show cmp[select from ref where not smallDrop;select from oth where not smallDrop;`tHalf];
    -1 "t06 (bars; negative = faster; pairs where both reached 0.6):";
    show cmp[ref;oth;`t06];
    -1 "recovered level (positive = better):";
    show cmp[ref;oth;`recovered]}[first swAll] each 1_swAll];
