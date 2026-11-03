/ Internal structure from saved weights (OBSTACLES_PLAN.md I.1 and I.3), per arm.
/ Usage (repo root): q q/programs/analysis/structure.q results/ARM [results/ARM2 ...] < /dev/null
/ Each ARM holds s<id>/ runs with weights_seed<id>.bin and summary.csv. Modular brains are read as
/ sight then central (one joint chain, .theory.chainMod); the touch brain is left out, since it gets
/ no input without walls.
/ I.1 carry-over: expected steps an injected vision unit stays in the brain (fundamental matrix), and
/   the share still inside after 12 / 24 / 60 steps (1, 2, 5 bars). Memory needs this well above a bar.
/ I.3 dominant-route graph: every directed state (cell, incoming label) follows its most likely route.
/   That graph has one successor per state, so its cycles are the recurrent loops the weights favour.
/   Reported: states on cycles, number of cycles, and from each slice's entry state, the length of
/   the dominant path and whether it reaches an output (exits) or falls into a loop (trapped).

\l q/load.q
\l q/modules/analysis/theory.q
\l q/modules/analysis/arms.q
\c 50 220
a: .z.x
if[0=count a; -2 "usage: q q/programs/analysis/structure.q results/ARM [...]"; exit 1]

/ Dominant successor of every state: index of the most likely next state, or n (the exit) when an
/ outputter slot is at least as likely
succ:{[QR] Q: QR 0; R: QR 1; n: count Q;
  qm: max each Q; rm: max each R; nx: {first idesc x} each Q;
  (?[rm>=qm; n; nx]),n}
/ States on cycles of a functional graph s (the exit n is a fixed point and doesn't count)
cycleStates:{[s] n: -1+count s; k: (n+1) {[s;x] s x}[s]/til n+1; c: distinct k; c where c<n}
cycleCount:{[s;c] if[0=count c; :0]; count distinct {[s;c;x] min (count c) {[s;x] s x}[s]\ x}[s;c] each c}
/ From slice s's entry state: dominant-path length to the exit, or null if it loops (trapped)
pathLen:{[s;st] n: -1+count s; seen: (); x: st;
  while[(x<n) & not x in seen; seen,: x; x: s x];
  $[x=n; count seen; 0N]}
entry: (8*2+til 8)+6                          / cell (0, 2+s), label 7

seedRow:{[d;sd]
  id: "I"$1_sd;
  w: get hsym `$d,"/",sd,"/weights_seed",(string id),".bin";
  sf: hsym `$d,"/",sd,"/summary.csv"; hdr: `$"," vs first read0 sf;
  sm: first (((count hdr)#"*";enlist ",") 0: sf);
  modular: (`architecture in hdr) & "modular"~sm`architecture;
  QR: $[modular; .theory.chainMod[3#w 0;6_w 0;.dirs.vBias]; .theory.chain[w 0;.dirs.vBias]];
  cv: .theory.carry[QR 0;12 24 60];
  s: succ QR; c: cycleStates s; pl: pathLen[s] each entry;
  (`id`bars!(id;"I"$sm`bars)),cv,
    `cycleStates`cycles`pathLen`trapped!(count c; cycleCount[s;c]; avg pl where not null pl; sum null pl)}

arm:{[d] d: .arm.strip d;
  subs: .arm.runs[d;"summary.csv"];
  t: seedRow[d] each subs;
  `arm`n`bars`residence`alive12`alive24`alive60`cycleStates`cycles`pathLen`trapped!
    (`$last "/" vs d; count t; first t`bars),avg each t`residence`alive12`alive24`alive60`cycleStates`cycles`pathLen`trapped}

-1 "I.1 residence: expected steps a vision unit stays (12 steps = 1 bar); aliveK: share still inside after K steps";
-1 "I.3 cycleStates/cycles: states on loops of the dominant-route graph; pathLen: steps of a slice's dominant path";
-1 "    to an output; trapped: slices (of 8) whose dominant path ends in a loop instead of an output";
show arm each a;
