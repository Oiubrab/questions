/ Energy conservation in propagation — hard invariant (see CLAUDE.md)
/ Inside .brain.propagate, energy only moves: sum(br) + sum(out) is unchanged,
/ given out starts at zero. Input injection, meta cross-flow and drain are
/ deliberately non-conserving and are not tested for conservation here.
/ Run from repo root: q q/tests/test_conservation.q

\l q/load.q
\l q/tests/harness.q

-1 "test_conservation";
system "S 12345";

/ Random brain state that satisfies the bookkeeping invariants:
/ br=0 -> inc0=inc1=0; br>0 -> inc0 in 1..8; br=2 -> inc1 in 0..8
randState:{[nR;nC]
  br: (nR;nC)#(nR*nC)?3i;
  inc0: (nR;nC)#(1i+(nR*nC)?8i) * 0<raze br;
  inc1: (nR;nC)#((nR*nC)?9i) * 2=raze br;
  (br;inc0;inc1)}

/ Run k propagate calls on a random brain; return (energy before/after pairs; final state)
trial:{[nR;nC;inOff;inLen;outOff;outLen;k]
  st: randState[nR;nC];
  syns: .syn.init[nR;nC];
  su: .syn.initSU[nR;nC];
  shape: (nR;nC;inOff;inLen;outOff;outLen);
  acc: (st 0;syns;su;st 1;st 2;`int$outLen#0i);
  pairs: ();
  i: 0;
  while[i<k;
    before: sum raze acc 0;
    acc: .brain.propagate[shape;acc 0;acc 1;acc 2;acc 3;acc 4;`int$outLen#0i];
    after: (sum raze acc 0) + sum acc 5;
    pairs,: enlist (before;after);
    i+: 1];
  (pairs;acc)}

checkShape:{[label;nR;nC;inOff;inLen;outOff;outLen]
  r: trial[nR;nC;inOff;inLen;outOff;outLen;200];
  pairs: r 0; acc: r 1;
  br: raze acc 0; inc0: raze acc 3; inc1: raze acc 4; out: acc 5;
  .t.eq[label," energy conserved over 200 propagate calls";pairs[;1];pairs[;0]];
  .t.ok[label," states stay in 0..2";br within 0 2];
  .t.ok[label," outputter stays in 0..2";out within 0 2];
  .t.ok[label," LOW neurons have no incoming direction";all 0=(inc0,inc1) where (br,br)=0];
  .t.ok[label," active neurons have an incoming direction";all inc0[where br>0] within 1 8];
  .t.ok[label," synapses within [MIN,MAX]";all (raze/[acc 1]) within (.syn.MIN_STRENGTH;.syn.MAX_STRENGTH)]}

checkShape["primary 6x12";6;12;2;8;2;8];
checkShape["meta 3x5";3;5;0;5;0;5];
checkShape["larger 8x16";8;16;4;8;4;8];

/ Drain never adds energy
st: randState[6;12];
dr: .brain.drain[0.5;st 0;st 1;st 2];
.t.ok["drain does not add energy";(sum raze dr 0)<=sum raze st 0];
.t.ok["drain clears directions of neurons it empties";all 0=(raze dr 1) where 0=raze dr 0];

.t.done[];
