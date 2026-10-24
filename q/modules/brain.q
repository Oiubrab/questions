/ Brain module
/ Implements the dual-brain neural simulation core.
/ Data representations:
/   br   : nR x nC integer matrix (trinary states 0/1/2)
/   syns : nR x nC x 8 x 8 integer nested list (4D synapse weights)
/   su   : nR x nC x 8 x 8 boolean nested list (synapse usage this bar)
/   inc0 : nR x nC integer matrix (primary incoming direction per neuron, 1-8, 0=unset)
/   inc1 : nR x nC integer matrix (secondary incoming direction, for HIGH neurons)
/   out  : outLen-element integer vector (outputter accumulator)
/ NOTE: 'rows' and 'cols' are reserved in kdb-x; use nR/nC for row/col count variables

\d .brain

/ Initialize brain state: nR x nC matrix of 0i, plus zeroed inc0/inc1
initBrain:{[nR;nC]
  br: .trinary.initGrid[nR;nC];
  inc0: (nR;nC)#0i;
  inc1: (nR;nC)#0i;
  (br;inc0;inc1)}

/ -------------------------------------------------------------------
/ CORE PROPAGATION STEP
/ Processes each active neuron once, moving energy along weighted
/ random paths through the 4D synapse array.
/ Energy conservation: source is only decremented if target was incremented.
/ -------------------------------------------------------------------

/ Process a single neuron at flat index idx.
/ acc: (br; syns; su; inc0; inc1; out)
/ Returns updated acc.
.brain.stepNeuron:{[nR;nC;inOff;inLen;outOff;outLen;acc;idx]
  br:acc 0; syns:acc 1; su:acc 2; inc0:acc 3; inc1:acc 4; out:acc 5;
  r:`int$idx div nC;
  c:`int$idx mod nC;
  v:br[r;c];
  if[v=0i; :acc];
  d1:inc0[r;c];
  if[d1=0i; :acc];
  d2:inc1[r;c];

  / Get synapse weight vector for primary incoming direction (8 outgoing weights)
  w:`float$syns[r;c;d1-1];
  / HIGH neurons with two incoming directions: average both direction groups
  if[(v=2i)&(d2>0i); w:`float$(syns[r;c;d1-1]+syns[r;c;d2-1])%2];

  / Apply vertical direction bias
  wb: w * .dirs.vBias;

  / Vectorised target validity and saturation check over all 8 directions
  nrs: r+`int$.dirs.dr;
  ncs: c+`int$.dirs.dc;
  inBrain8: (nrs>=0i)&(nrs<`int$nR)&(ncs>=0i)&(ncs<`int$nC);
  inOut8:   (r=`int$nR-1i) & (nrs=`int$nR) & ((ncs-`int$outOff)>=0i) & ((ncs-`int$outOff)<`int$outLen);
  / Clamp indices for safe reads (guarded by inBrain8/inOut8 so clamped values are never used)
  safeR:      0i|(`int$nR-1i)&nrs;
  safeC:      0i|(`int$nC-1i)&ncs;
  safeOutIdx: 0i|(`int$outLen-1i)&ncs-`int$outOff;
  / Saturation flags: 1b where target is already at HIGH (2i)
  brSat:  inBrain8 & 2i={[b;r;c] b[r;c]}[br]'[safeR;safeC];
  outSat: inOut8   & 2i=out[safeOutIdx];
  / Zero weights for invalid or saturated targets
  wb: wb * `float$(inBrain8 & not brSat) | (inOut8 & not outSat);

  total:sum wb;
  if[total<=0.0; :acc];

  / Weighted random direction selection
  r2:total * rand 1.0;
  chosen:`int$first where (sums wb)>=r2;

  / Compute target position
  nr:r+`int$.dirs.dr[chosen];
  nc:c+`int$.dirs.dc[chosen];

  / Attempt the move
  moved:0b;
  tOrig:0i;
  inBrain:(nr>=0i)&(nr<`int$nR)&(nc>=0i)&(nc<`int$nC);
  inOut:  (r=`int$nR-1i)&(nr=`int$nR)&((nc-`int$outOff)>=0i)&((nc-`int$outOff)<`int$outLen);

  if[inBrain&(br[nr;nc]<2i);
    tOrig:br[nr;nc];
    br:.[br;(nr;nc);:;1i+br[nr;nc]];
    moved:1b];
  if[(not moved)&inOut&(out[nc-`int$outOff]<2i);
    out:.[out;enlist nc-`int$outOff;:;1i+out[nc-`int$outOff]];
    moved:1b];

  if[not moved; :acc];

  / ENERGY CONSERVATION: decrement source only because move succeeded
  srcOrig:v;
  br:.[br;(r;c);:;0i|v-1i];

  / Update target's incoming direction (brain targets only, not outputter)
  if[inBrain;
    $[tOrig=0i;
      [inc0:.[inc0;(nr;nc);:;.dirs.opp[chosen]]];  / LOW->MEDIUM: set primary
      [inc1:.[inc1;(nr;nc);:;.dirs.opp[chosen]]]]]; / MEDIUM->HIGH: set secondary

  / Clear source's incoming directions based on new state
  newSrc:br[r;c];
  $[newSrc=0i;
    [inc0:.[inc0;(r;c);:;0i]; inc1:.[inc1;(r;c);:;0i]];  / went to LOW: clear both
    (srcOrig=2i)&(newSrc=1i);
    [inc1:.[inc1;(r;c);:;0i]];                             / HIGH->MEDIUM: clear secondary
    0];

  / Reinforce the synapse (and mark as used)
  rAmt:1000i;
  syns:.[syns;(r;c;d1-1;chosen);:;.syn.MAX_STRENGTH & rAmt + syns[r;c;d1-1;chosen]];
  su:.[su;(r;c;d1-1;chosen);:;1b];
  / HIGH neurons reinforce both incoming direction groups
  if[(srcOrig=2i)&(d2>0i);
    syns:.[syns;(r;c;d2-1;chosen);:;.syn.MAX_STRENGTH & rAmt + syns[r;c;d2-1;chosen]];
    su:.[su;(r;c;d2-1;chosen);:;1b]];

  (br;syns;su;inc0;inc1;out)}

/ Run one full propagation step across all neurons.
/ Processes neurons in row-major order.
/ shape: (nR;nC;inOff;inLen;outOff;outLen)
/ Returns updated (br;syns;su;inc0;inc1;out).
propagate:{[shape;br;syns;su;inc0;inc1;out]
  nR:shape 0; nC:shape 1; inOff:shape 2; inLen:shape 3; outOff:shape 4; outLen:shape 5;
  activeIdx: where 0i < raze br;
  if[0=count activeIdx; :(br;syns;su;inc0;inc1;out)];
  acc0:(br;syns;su;inc0;inc1;out);
  (.brain.stepNeuron[nR;nC;inOff;inLen;outOff;outLen]/)[acc0;activeIdx]}

/ -------------------------------------------------------------------
/ INPUT INJECTION
/ Copy non-LOW inputter values into the brain's top row.
/ Direction is set to 7 (SW = "came from above").
/ -------------------------------------------------------------------
copyInputToTopRow:{[inp;inOff;br;inc0]
  bc: `int$inOff+til count inp;          / brain columns
  active: where (bc>=0i) & inp>0i;       / active input positions
  if[0=count active; :(br;inc0)];
  activeCols: bc[active];
  br[0]:   @[br[0];   activeCols; :; inp[active]];
  inc0[0]: @[inc0[0]; activeCols; :; `int$count[activeCols]#7i];
  (br;inc0)}

/ -------------------------------------------------------------------
/ SIDE I/O: META-BRAIN CROSS-FLOW
/ Apply meta-brain output to right column of primary brain.
/ -------------------------------------------------------------------
applyMetaInput:{[metaInp;metaOff;br;inc0]
  nC: count br 0;
  br_rows: `int$metaOff+til count metaInp;
  active: where (br_rows>=0i) & (br_rows<`int$count br) & metaInp>0i;
  if[0=count active; :(br;inc0)];
  activeRows: br_rows[active];
  c: nC-1i;
  / Update right column: br[activeRows[j]; c] := metaInp[active[j]]
  newBrRows:   {[c;row;v] @[row;c;:;v]}[c]'[br[activeRows];   metaInp[active]];
  newInc0Rows: {[c;row;v] @[row;c;:;v]}[c]'[inc0[activeRows]; `int$count[activeRows]#8i];
  br:   @[br;   activeRows; :; newBrRows];
  inc0: @[inc0; activeRows; :; newInc0Rows];
  (br;inc0)}

/ Throttled version: apply meta input with probability = f(pressure)
/ Valve 1: p<0.3 -> full flow, 0.3-0.6 -> linear taper, >0.6 -> cut off
applyThrottledMetaInput:{[metaInp;metaOff;pressure;br;inc0]
  flowRate:$[pressure<0.3; 1.0; pressure<0.6; 1.0-(pressure-0.3)%0.3; 0.0];
  if[flowRate<=0.0; :(br;inc0)];
  nC: count br 0;
  br_rows: `int$metaOff+til count metaInp;
  active: where (br_rows>=0i) & (br_rows<`int$count br) & metaInp>0i;
  if[0=count active; :(br;inc0)];
  / Probabilistic filter: keep each active position independently with prob = flowRate
  active: active where (count[active]?1.0)<flowRate;
  if[0=count active; :(br;inc0)];
  activeRows: br_rows[active];
  c: nC-1i;
  newBrRows:   {[c;row;v] @[row;c;:;v]}[c]'[br[activeRows];   metaInp[active]];
  newInc0Rows: {[c;row;v] @[row;c;:;v]}[c]'[inc0[activeRows]; `int$count[activeRows]#8i];
  br:   @[br;   activeRows; :; newBrRows];
  inc0: @[inc0; activeRows; :; newInc0Rows];
  (br;inc0)}

/ -------------------------------------------------------------------
/ OVERFLOW CAPTURE
/ Copy values from left column into a vector.
/ -------------------------------------------------------------------
captureOverflow:{[br;overflowOff;overflowLen]
  nR: count br;
  brRows: `int$overflowOff+til overflowLen;
  valid: where (brRows>=0i) & (brRows<`int$nR);
  ov: `int$overflowLen#0i;
  if[0=count valid; :ov];
  ov[valid]: `int$first each br[brRows[valid]];
  ov}

/ -------------------------------------------------------------------
/ PRESSURE CALCULATION
/ pressure = sum(all states) / (nR*nC*2)   range [0.0, 1.0]
/ -------------------------------------------------------------------
pressure:{[br]
  nR:count br; nC:count br 0;
  `float$(sum raze br) % nR*nC*2}

/ Leftward pull valve (valve 2): multiplier increases with pressure
leftwardPull:{[p]
  $[p<0.1; 1.0;
    p<0.3;  1.0+(p-0.1)%0.2;
    2.0]}

/ -------------------------------------------------------------------
/ ENERGY DRAIN
/ Probabilistically reduce neuron states to prevent saturation.
/ drain_probability scales with pressure: 3% at p=0.1, 15% at p>=0.5
/ Vectorised per-row, applied with each over all rows.
/ -------------------------------------------------------------------
drain:{[drainProb;br;inc0;inc1]
  / Inner function: drain one row of the brain (vectorised over columns)
  drainRow:{[prob;brRow;i0Row;i1Row]
    active: where brRow>0i;
    if[0=count active; :(brRow;i0Row;i1Row)];
    / Independent random draw per active neuron
    drained: active where (count[active]?1.0)<prob;
    if[0=count drained; :(brRow;i0Row;i1Row)];
    srcOrig: brRow[drained];
    newVals: 0i|srcOrig-1i;
    brRow:  @[brRow;  drained; :; newVals];
    / Clear incoming directions for state transitions
    wentLow:   drained where 0i=newVals;
    highToMed: drained where (srcOrig=2i) & 1i=newVals;
    i0Row: @[i0Row; wentLow;   :; `int$count[wentLow]#0i];
    i1Row: @[i1Row; wentLow;   :; `int$count[wentLow]#0i];
    i1Row: @[i1Row; highToMed; :; `int$count[highToMed]#0i];
    (brRow;i0Row;i1Row)};
  results: drainRow[drainProb]'[br;inc0;inc1];
  (results[;0]; results[;1]; results[;2])}

\d .
