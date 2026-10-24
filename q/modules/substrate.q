/ Substrate lab module (.sub) — REVIVAL_PLAN.md Phase S
/ A configurable copy of the .brain propagation rule, used only by the lab
/ (q/programs/substrate_lab.q); cat_mouse.q does not load it.
/ The core rule is unchanged: each active neuron moves one energy unit along a
/ usage-weighted random route, and the route used is strengthened. Settings only
/ change the rule's surroundings. With .sub.defaults it reproduces
/ .brain.propagate bit for bit (q/tests/test_substrate.q).
/ Requires .dirs and .syn (load after directions.q and synapses.q).

\d .sub

/ cfg keys:
/   nR nC          grid size
/   wrap           1b: edges wrap (toroidal). Wrapped targets are never off-grid, so
/                  the bottom-row sink can't fire either
/   bias           8 direction multipliers (NW N NE W E SW S SE); .dirs.vBias = downward drift
/   temp           route-choice randomness: 1 = weighted random (current), <1 sharper,
/                  0 = always the strongest route (ties: lowest direction index)
/   hebb           strengthening added to a route each time it's used (current 1000i; 0 = none)
/   sink           1b: bottom row can pass energy into the outputter (current behaviour)
/   outOff outLen  outputter position under the bottom row
/   order          `rowmajor (current) or `random processing order each step
defaults:`nR`nC`wrap`bias`temp`hebb`sink`outOff`outLen`order!
  (6i;12i;0b;.dirs.vBias;1f;1000i;1b;2i;8i;`rowmajor)

/ Process one neuron at flat index idx. acc: (br;syns;su;inc0;inc1;out). Mirrors .brain.stepNeuron.
step:{[cfg;acc;idx]
  br:acc 0; syns:acc 1; su:acc 2; inc0:acc 3; inc1:acc 4; out:acc 5;
  nR:cfg`nR; nC:cfg`nC; outOff:cfg`outOff; outLen:cfg`outLen;
  r:`int$idx div nC;
  c:`int$idx mod nC;
  v:br[r;c];
  if[v=0i; :acc];
  d1:inc0[r;c];
  if[d1=0i; :acc];
  d2:inc1[r;c];

  / Outgoing weights for the incoming direction; HIGH with two directions averages both
  w:`float$syns[r;c;d1-1];
  if[(v=2i)&(d2>0i); w:`float$(syns[r;c;d1-1]+syns[r;c;d2-1])%2];
  wb: w * cfg`bias;

  / Candidate targets in all 8 directions, wrapped if toroidal
  nrs: r+.dirs.dr;
  ncs: c+.dirs.dc;
  if[cfg`wrap; nrs: nrs mod nR; ncs: ncs mod nC];
  inBrain8: (nrs>=0i)&(nrs<nR)&(ncs>=0i)&(ncs<nC);
  inOut8:   (cfg`sink) & (r=nR-1i) & (nrs=nR) & ((ncs-outOff)>=0i) & ((ncs-outOff)<outLen);
  safeR:      0i|(nR-1i)&nrs;
  safeC:      0i|(nC-1i)&ncs;
  safeOutIdx: 0i|(outLen-1i)&ncs-outOff;
  brSat:  inBrain8 & 2i={[b;r;c] b[r;c]}[br]'[safeR;safeC];
  outSat: inOut8   & 2i=out[safeOutIdx];
  / Zero weights for invalid or saturated targets
  wb: wb * `float$(inBrain8 & not brSat) | (inOut8 & not outSat);
  total:sum wb;
  if[total<=0.0; :acc];

  / Route choice. temp=1 draws exactly as .brain.stepNeuron does (same RNG use)
  chosen: $[0f=cfg`temp; `int$first idesc wb;
            1f=cfg`temp; `int$first where (sums wb)>=total * rand 1.0;
            [p: (wb%max wb) xexp 1%cfg`temp; `int$first where (sums p)>=(sum p) * rand 1.0]];

  nr:nrs chosen;
  nc:ncs chosen;
  inBrain:inBrain8 chosen;
  inOut:inOut8 chosen;

  / Attempt the move
  moved:0b;
  tOrig:0i;
  if[inBrain&(br[nr;nc]<2i);
    tOrig:br[nr;nc];
    br:.[br;(nr;nc);:;1i+br[nr;nc]];
    moved:1b];
  if[(not moved)&inOut&(out[nc-outOff]<2i);
    out:.[out;enlist nc-outOff;:;1i+out[nc-outOff]];
    moved:1b];
  if[not moved; :acc];

  / ENERGY CONSERVATION: decrement source only because move succeeded
  srcOrig:v;
  br:.[br;(r;c);:;0i|v-1i];

  / Target records where its energy came from (brain targets only)
  if[inBrain;
    $[tOrig=0i;
      [inc0:.[inc0;(nr;nc);:;.dirs.opp[chosen]]];   / LOW->MEDIUM: set primary
      [inc1:.[inc1;(nr;nc);:;.dirs.opp[chosen]]]]];  / MEDIUM->HIGH: set secondary

  / Clear source's incoming directions based on new state
  newSrc:br[r;c];
  $[newSrc=0i;
    [inc0:.[inc0;(r;c);:;0i]; inc1:.[inc1;(r;c);:;0i]];  / went to LOW: clear both
    (srcOrig=2i)&(newSrc=1i);
    [inc1:.[inc1;(r;c);:;0i]];                             / HIGH->MEDIUM: clear secondary
    0];

  / Strengthen the route used (and mark as used)
  h:cfg`hebb;
  syns:.[syns;(r;c;d1-1;chosen);:;.syn.MAX_STRENGTH & h + syns[r;c;d1-1;chosen]];
  su:.[su;(r;c;d1-1;chosen);:;1b];
  if[(srcOrig=2i)&(d2>0i);
    syns:.[syns;(r;c;d2-1;chosen);:;.syn.MAX_STRENGTH & h + syns[r;c;d2-1;chosen]];
    su:.[su;(r;c;d2-1;chosen);:;1b]];

  (br;syns;su;inc0;inc1;out)}

/ One propagation step over the neurons active at its start. acc as in step.
propagate:{[cfg;acc]
  idx: where 0i < raze acc 0;
  if[0=count idx; :acc];
  if[`random=cfg`order; idx: neg[count idx]?idx];
  (step[cfg]/)[acc;idx]}

\d .
