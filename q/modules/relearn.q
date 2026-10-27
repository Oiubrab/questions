/ Re-learning test (.relearn) — REVIVAL_PLAN.md G.4
/ Worlds change one thing about the cat's situation; a schedule flicks between them, and the
/ switch metrics measure how well and how fast the brain re-learns after each change.
/ Loaded by cat_mouse.q (only active with --relearn-schedule) and the tests.

\d .relearn

/ Move directions in angle order (E SE S SW W NW N NE), as .dirs indices: rotating by one
/ position turns a move by 45 degrees (x right, y down = "S"), so +2 positions is +90 degrees
ANGLE_ORDER: 4 7 6 5 3 0 1 2i
I8: `int$til 8                    / til returns longs; keep permutations int
L90: ANGLE_ORDER (2i+`int$ANGLE_ORDER?I8) mod 8i

/ world -> (legs: output slot -> move direction; eyes: true vision slice -> input slice; goal sign)
WORLDS: `A`L180`L90`E180`FLEE!(
  (I8;           I8;                 1f);     / normal
  (7i-I8;        I8;                 1f);     / legs reversed: every slot moves the opposite way
  (L90;          I8;                 1f);     / legs rotated 90 degrees
  (I8;           (4i+I8) mod 8i;     1f);     / eyes rotated 180 degrees
  (I8;           I8;                -1f))     / goal flipped: reward moving away from the mouse

/ Parse "A:10000,L180:5000,..." into (worlds; block lengths). Signals on malformed specs.
parseSchedule:{[spec]
  kv: ":" vs/: "," vs spec;
  if[any 2<>count each kv; '"relearn schedule: each block must be WORLD:BARS"];
  w: `$kv[;0]; n: "I"$kv[;1];
  if[any not w in key WORLDS; '"relearn schedule: unknown world (use ",(", " sv string key WORLDS),")"];
  if[any (null n)|n<=0i; '"relearn schedule: block lengths must be positive integers"];
  (w;n)}

/ Mean over the last w bars of the non-null values in x (null where the window has none)
rollMean:{[w;x] s: w msum 0f^x; c: w msum `float$not null x; @[s%c; where c=0; :; 0n]}

/ Mean of the non-null values of x between 1-based bars a..b (inclusive; clipped to x)
span:{[x;a;b] a: 1|a; b: (count x)&b; $[b<a; 0n; avg x (a-1)+til 1+b-a]}

/ Metrics for one block of goal-alignment series ga (1-based bars). Block k runs from bar s to
/ bar e; the previous block (null for the first block) ran from ps to pe.
/   pre        mean over the last 1,000 bars of the previous block (the cat's own competence)
/   trough     mean over the first 200 bars after the switch, against the new world's goal
/   drop       pre - trough; smallDrop flags drop < 0.1 (recovery time is then meaningless)
/   tHalf      bars after the switch until the rolling 100-bar mean first reaches
/              trough + drop/2 (window must be full, so the resolution floor is 100 bars)
/   t06        bars until the rolling 500-bar mean reaches 0.6 (window full), as for acquisition
/   recovered  mean over the last 1,000 bars of the block; recFrac = (recovered-trough)/drop
/ Times are null when not reached within the block.
blockMetrics:{[ga;s;e;ps;pe]
  seg: ga (s-1)+til 1+e-s;
  pre: $[null ps; 0n; span[ga;pe-999;pe]];
  trough: span[ga;s;s+199];
  drop: pre-trough;
  small: $[null drop; 0b; drop<0.1];
  r100: rollMean[100;seg]; r500: rollMean[500;seg];
  i: til count seg;
  hitHalf: where (i>=99)&r100>=trough+0.5*drop;
  hit06: where (i>=499)&r500>=0.6;
  tHalf: $[(null drop)|small|0=count hitHalf; 0Ni; `int$1+first hitHalf];
  t06: $[0=count hit06; 0Ni; `int$1+first hit06];
  rec: span[ga;e-999;e];
  `pre`trough`drop`smallDrop`tHalf`t06`recovered`recFrac!(pre;trough;drop;small;tHalf;t06;rec;$[null drop; 0n; (rec-trough)%drop])}

/ One row per block (row 0 = the first block, i.e. learning from scratch) for schedule (w;n)
switchTable:{[ga;sched]
  w: sched 0; n: sched 1; ends: sums n; starts: 1i+ends-n;
  rs: {[ga;w;starts;ends;k]
    m: blockMetrics[ga;starts k;ends k;$[k=0; 0Ni; starts k-1];$[k=0; 0Ni; ends k-1]];
    (`switch`bar`fromWorld`toWorld!(k;starts k;$[k=0; `; w k-1];w k)),m}[ga;w;starts;ends] each til count w;
  rs}

\d .
