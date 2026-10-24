/ Substrate lab — run the brain's energy rule on its own (no cat, no rewards) and
/ measure whether activity persists, jams, or builds and reorganises routes.
/ REVIVAL_PLAN.md Phase S. Every option is optional (defaults in `opt` below):
/   q q/programs/substrate_lab.q --rows 16 --cols 16 --wrap 1 --bias flat --temp 0.3 \
/     --hebb 1000 --sink 0 --order random --decay 60 --fill 0.3 --steps 3000 \
/     --sample 50 --seed 1 --out results/lab/x/ < /dev/null
/ Options:
/   --rows --cols     grid size
/   --wrap 0|1        toroidal edges
/   --bias down|flat  .dirs.vBias (current downward drift) or no drift
/   --temp T          route randomness: 1 current, <1 sharper, 0 strongest route
/   --hebb N          strengthening per route use (0 = no learning)
/   --sink 0|1        bottom-row outputter absorbs energy (current brain: 1)
/   --order rowmajor|random  neuron processing order per step
/   --decay N         apply synapse decay every N steps (0 = never; the cat applies it every 60)
/   --fill P          starting pressure: each cell gets 0/1/2 units, mean 2P
/   --steps N  --sample N  --seed N  --out DIR
/ Writes DIR/metrics.csv (one row per sample) and DIR/summary.csv (config + means over the
/ second half of the run).

\l q/load.q
\l q/modules/substrate.q

/ ---- options ----
opt: `rows`cols`wrap`bias`temp`hebb`sink`order`decay`fill`steps`sample`seed`out!
  ("16";"16";"1";"flat";"1";"1000";"0";"rowmajor";"60";"0.3";"3000";"50";"1";"results/lab/run/")
a: .z.x
k: where a like "--*"
given: (`$2_'a k)!a k+1
badOpts: (key given) except key opt
if[count badOpts; -2 "Unknown option(s): ",", " sv string badOpts; exit 1]
opt,: given

biasVec: $["down"~opt`bias; .dirs.vBias; "flat"~opt`bias; 8#1f; [-2 "--bias must be down or flat"; exit 1]]
cfg: .sub.defaults,`nR`nC`wrap`bias`temp`hebb`sink`order!
  ("I"$opt`rows; "I"$opt`cols; "1"=first opt`wrap; biasVec;
   "F"$opt`temp; "I"$opt`hebb; "1"=first opt`sink; `$opt`order)
/ Outputter spans the bottom row minus 2 cells each side (12 cols -> cols 2-9, as in the cat)
cfg[`outOff]: 2i
cfg[`outLen]: $[cfg`sink; cfg[`nC]-4i; 0i]
decayEvery: "I"$opt`decay
fillP: "F"$opt`fill
nSteps: "I"$opt`steps
nSamp: "I"$opt`sample
outDir: opt`out
if[not "/"=last outDir; outDir,: "/"]
system "S ",opt`seed

/ ---- initial state: random energy at the requested pressure ----
nR: cfg`nR; nC: cfg`nC; n: nR*nC
br0: (nR;nC)#`int$((n?1.0)<fillP)+((n?1.0)<fillP)
acc: (br0; .syn.init[nR;nC]; .syn.initSU[nR;nC];
      (nR;nC)#(1i+n?8i)*0<raze br0;           / incoming direction for active cells
      (nR;nC)#(1i+n?8i)*2=raze br0;           / second direction for HIGH cells
      `int$cfg[`outLen]#0i)

/ ---- metrics ----
cvar:{$[0=avg x; 0n; (sdev x)%avg x]}          / spread of energy across rows or columns
/ Probability of the most likely route for each (cell, inDir), from weights x bias at
/ this temperature (ignores edge and saturation masks)
peakP:{[b;t;cells] raze {[b;t;m] {[b;t;w] w:w*b; $[t=0f; 1f; [p:(w%max w) xexp 1%t; (max p)%sum p]]}[b;t] each m}[b;t] each cells}
/ Index of the strongest route for each (cell, inDir)
bestR:{[b;cells] raze {[b;m] {[b;w] first idesc w*b}[b] each m}[b] each cells}

-1 "substrate lab: ",.Q.s1 (enlist[`grid]!enlist (nR;nC)),(`wrap`temp`hebb`sink`order#cfg),`bias`decay`fill`steps`seed!(`$opt`bias;decayEvery;fillP;nSteps;"I"$opt`seed);
t0: .z.p
e0: sum raze acc 0
outflow: 0
samples: ()
past: ()                                        / last 11 states, for 10-step persistence
lastBest: bestR[cfg`bias;raze acc 1]
i: 1
while[i<=nSteps;
  pb: raze acc 0;
  acc[5]: `int$cfg[`outLen]#0i;
  acc: .sub.propagate[cfg;acc];
  outflow+: sum acc 5;
  if[(decayEvery>0) & 0=i mod decayEvery; acc[1]: .syn.applyDecay acc 1];
  past: -11#past,enlist raze acc 0;
  if[0=i mod nSamp;
    b: raze acc 0;
    cells: raze acc 1;
    bst: bestR[cfg`bias;cells];
    samples,: enlist `step`energy`activity`saturated`empty`rowSpread`colSpread`persist10`sharp`turnover!
      (i; sum b; avg b<>pb; avg b=2; avg b=0;
       cvar sum each acc 0; cvar sum acc 0;
       $[11=count past; (`float$b) cor `float$first past; 0n];
       avg peakP[cfg`bias;cfg`temp;cells]; avg bst<>lastBest);
    lastBest: bst];
  i+: 1]
secs: 1e-9*`long$.z.p-t0

m: samples
saveCsv:{[name;t] (hsym `$outDir,name) 0: csv 0: t}
saveCsv["metrics.csv";m]
h: (floor 0.5*count m)_m
conf: `nRows`nCols`wrap`bias`temp`hebb`sink`order`decay`fill`seed!
  (nR;nC;cfg`wrap;`$opt`bias;cfg`temp;cfg`hebb;cfg`sink;cfg`order;decayEvery;fillP;"I"$opt`seed)
res: exec avg energy, avg activity, avg saturated, avg empty, avg rowSpread, avg colSpread,
          avg persist10, avg sharp, avg turnover from h
summary: enlist conf,res,`e0`eEnd`outflow`secs!(e0; sum raze acc 0; outflow; secs)
saveCsv["summary.csv";summary]
-1 "second-half means: ",.Q.s1 res;
-1 "energy start/end: ",(string e0),"/",(string sum raze acc 0),"  outflow: ",(string outflow),"  secs: ",string secs;
