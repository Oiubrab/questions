/ Numerical checks for THEORY.md — reproduces every "Verified" number in it.
/ Usage (repo root): q q/programs/theory_checks.q < /dev/null
/ Sections 4 and 5 need the Phase 3 outputs (results/p3_train, results/p3_A); they are
/ skipped if absent (regenerate with scripts/phase3_experiment.sh).

\l q/load.q
\c 50 220
\l q/modules/theory.q
/ Shorter local names for the .theory functions used below
NS:.theory.NS; dr:.theory.dr; dc:.theory.dc; b:.dirs.vBias;
chain:.theory.chain; policy:.theory.policy; stats:.theory.stats; surv:.theory.surv; top:.theory.top;

-1 "=== 1. One-unit step statistics (interior, vBias, exchangeable weights) ===";
pb:b%sum b;
-1 "drift v (rows/step) = ",string sum pb*dr;
-1 "lateral variance/step = ",(string sum pb*dc*dc),"   vertical variance/step = ",string (sum pb*dr*dr)-(sum pb*dr) xexp 2;

-1 "";
-1 "=== 2. Energy remaining vs steps: linear theory vs real .brain.propagate (fresh weights, walls+sink) ===";
system "S 11"; fresh:.syn.init[6;12]; QRf:chain[fresh;b];
x:NS#1f%NS; lin:(); do[50; x:x mmu QRf 0; lin,:sum x];
st:{[p] n:72; br:(6;12)#`int$((n?1.0)<p)+((n?1.0)<p); (br;(6;12)#(1i+n?8i)*0<raze br;(6;12)#(1i+n?8i)*2=raze br)};
simFrac:{[syns;p;reps]
  fs:{[syns;p;x] s:st p; e0:sum raze s 0; if[e0=0; :50#0n];
      acc:(s 0;syns;.syn.initSU[6;12];s 1;s 2;8#0i); f:();
      do[50; acc:.brain.propagate[(6;12;2;8;2;8);acc 0;acc 1;acc 2;acc 3;acc 4;8#0i]; f,:(sum raze acc 0)%e0]; f}[syns;p] each til reps;
  avg fs where not null first each fs}
system "S 12"; lo:simFrac[fresh;0.03;300]; hi:simFrac[fresh;0.5;60];
ix:0 4 11 23 49;
show ([] t:1+ix; linearTheory:lin ix; simLowDensity:lo ix; simHalfFull:hi ix);
-1 "mean residence time, unit entering top row (steps) = ",string avg {[Q;s] 1+sum surv[Q;top s;400]}[QRf 0] each til 8;
-1 "P(still in brain after 12 steps | entered top row) = ",string avg {[Q;s] last surv[Q;top s;12]}[QRf 0] each til 8;

-1 "";
-1 "=== 3. Untrained policy and output wiring ===";
Af:policy QRf;
-1 "untrained policy x100 (rows slice 0-7; cols slot 0-7 = NW N NE W E SW S SE):"; show `int$100*Af;
perm: 4 7 6 5 3 0 1 2;                                 / slot s -> direction at angle 45*s
remap:{[A;perm] {[perm;row] @[8#0f;perm;:;row]}[perm] each A};
-1 "current wiring:  ",.Q.s1 stats Af;
-1 "remapped wiring: ",.Q.s1 stats remap[Af;perm];

haveP3: all {`summary.csv in key hsym `$x} each ("results/p3_train/s101";"results/p3_A/s101");
if[not haveP3; -1 ""; -1 "(sections 4 and 5 skipped: run scripts/phase3_experiment.sh first)"; exit 0];

-1 "";
-1 "=== 4. Policy predicted from weights alone vs measured behaviour (16 trained brains) ===";
r: {[id] w:get hsym `$"results/p3_train/s",(string id),"/weights_seed",(string id),".bin";
     pr: stats policy chain[w 0;b];
     s:("IIBBBSIIIIIF";enlist ",") 0: hsym `$"results/p3_A/s",(string id),"/summary.csv";
     pr,`measTowards`catchesA!(first s[`towards]%s`totalMoves; first s`catches)} each 101+til 16;
t:([] id:101+til 16),'r;
show t;
-1 "corr(predicted towards, measured towards) = ",string t[`towards] cor t`measTowards;
-1 "corr(predicted E[cos], catches in arm A)   = ",string t[`expCos] cor `float$t`catchesA;
-1 "corr(predicted MI, catches in arm A)       = ",string t[`MIbits] cor `float$t`catchesA;

-1 "";
-1 "=== 5. Weight distribution in trained brains (fraction of 4608 primary synapses) ===";
f:{[id] s: raze raze/[(get hsym `$"results/p3_train/s",(string id),"/weights_seed",(string id),".bin") 0]; (avg s=2000000; avg s>=1000000; avg s<=100; avg (s>100)&s<1000000)};
q5: f each 101+til 16;
show ([] band:("at cap 2,000,000";">= 1,000,000";"<= 100 (floor region)";"between 100 and 1,000,000"); mean:avg q5; minimum:min q5; maximum:max q5);
