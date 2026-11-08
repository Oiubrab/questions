/ Ledger wrappers: no RNG use, so a run must reproduce the plain run exactly.
.p.NF: ROWS*COLS*64
.p.L: `dpos`dneg`strat`catch`decay`hebb!6#enlist .p.NF#0f
.p.flat:{raze raze raze x}
.p.add:{[k;old;new] .p.L[k]: .p.L[k]+(log `float$.p.flat new)-log `float$.p.flat old;}
.p.c: `rein`pun`strat`catch!0 0 0 0
.p.orig: `rein`pun`look`scale`decay!(.syn.applySparseReinforceNL;.syn.applySparsePunishNL;.syn.applySparseLookbackScale;.syn.applySparseScale;.syn.applyDecay)
.syn.applySparseReinforceNL:{[s;u;n;c;l] r:.p.orig[`rein][s;u;n;c;l]; .p.add[`dpos;s;r]; .p.c[`rein]:c; r}
.syn.applySparsePunishNL:{[s;u;n;c;l] r:.p.orig[`pun][s;u;n;c;l]; .p.add[`dneg;s;r]; .p.c[`pun]:c; r}
.syn.applySparseLookbackScale:{[s;us;m] r:.p.orig[`look][s;us;m]; .p.add[`strat;s;r]; .p.c[`strat]:count us; r}
.syn.applySparseScale:{[s;u;m] r:.p.orig[`scale][s;u;m]; .p.add[`catch;s;r]; r}
.syn.applyDecay:{[s] r:.p.orig[`decay][s]; if[.p.NF=count .p.flat s; .p.add[`decay;s;r]]; r}
.p.U: 8 cut (8*.p.NF)#0f            / uses by slice (row 8 = meta-only / no vision is dropped)
.p.U: 8#enlist .p.NF#0f
.p.C: .p.NF#0f; .p.N: .p.NF#0f     / sum of move cosine over uses, and uses (bars with a move)
.p.W0: ()
.p.B: ()
.p.snaps: ()
.p.phase: ()
.p.barStart:{.p.W0: prim`syns; .p.c: `rein`pun`strat`catch!0 0 0 0}
.p.afterProp:{.p.add[`hebb;.p.W0;prim`syns]}
.p.barEnd:{
  idx: `long$(prim`su) 1;
  if[(visionSlice>=0)&0<count idx; .p.U[visionSlice]: @[.p.U visionSlice;idx;+;1f]];
  if[not null moveAlignment; .p.N: @[.p.N;idx;+;1f]; .p.C: @[.p.C;idx;+;moveAlignment]];
  .p.B,: enlist (bar;adaptThresh;temporalScope;magnitude;rateCounter;.p.c`rein;.p.c`pun;.p.c`strat;satCell;visionSlice;moveAlignment;count idx);
  if[0=bar mod 500; .p.snaps,: enlist .p.flat prim`syns];
  if[bar=1000; .p.phase: enlist .p.L];
  }
