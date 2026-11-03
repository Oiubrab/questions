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

/ G.R3: .brain.HEBB sets the unconditional per-use increment exactly
hebbStep:{[h] .brain.HEBB:h; system "S 9"; sy:.syn.init[6;12];
  br:6 12#0i; br[2;5]:1i; i0:6 12#0i; i0[2;5]:2i;
  acc:.brain.propagate[(6;12;2;8;2;8);br;sy;.syn.initSU[6;12];i0;6 12#0i;8#0i];
  d:(raze raze raze acc 1)-raze raze raze sy; .brain.HEBB:1000i; d where d<>0}
.t.eq["HEBB 1000: one used route gains exactly 1000";hebbStep 1000i;enlist 1000i];
.t.eq["HEBB 250: one used route gains exactly 250";hebbStep 250i;enlist 250i];
.t.eq["HEBB restored to the default";.brain.HEBB;1000i];

/ Internal input (W.1b): .brain.injectAdd adds energy, caps at HIGH, never removes, sets the label
ib: (6 12#0i); ib[2;3]: 1i; ii: 6 12#0i;
ia: .brain.injectAdd[ib;ii;0 2 2i;5 3 3i;1i;7i];
.t.eq["injectAdd adds a unit to an empty cell";ia[0;0;5];1i];
.t.eq["injectAdd adds to an occupied cell and caps at HIGH (two adds to a cell holding 1)";ia[0;2;3];2i];
.t.eq["injectAdd adds exactly the units that fit";(sum raze ia 0)-sum raze ib;2i];
.t.eq["injectAdd sets the incoming label";(ia[1;0;5];ia[1;2;3]);7 7i];
.t.ok["injectAdd with no cells is the identity";(ib;ii)~.brain.injectAdd[ib;ii;`int$();`int$();1i;7i]];

/ Meta-brain input: satiety enters on the left when starving, on the right when full (user, 2026-10-29)
.t.eq["satiety cells: starving .. full";.brain.satietyCell each 0 0.49 0.5 1.9 2 4.9 5 9.9 10 40f;0 0 1 1 2 2 3 3 4 4i];
.t.eq["satiety input: starving enters on the left";.brain.satietyInput[0f;5i];1 0 0 0 0i];
.t.eq["satiety input: full enters on the right";.brain.satietyInput[25f;5i];0 0 0 0 1i];
.t.ok["satiety input: always exactly one unit";all 1=sum each .brain.satietyInput[;5i] each 0.1*til 300];

/ Modular brain (.mod): splitting and stacking, and transfers between brains
\l q/modules/brain/modular.q
system "S 11";
mp: .engine.initSystem[10i;12i;8i;8i;2i;2i];
mp[`br]: `int$10 12#120?3; mp[`inc0]: `int$10 12#120?9; mp[`inc1]: `int$10 12#120?9;
ms: (.mod.sub[mp;0;3;2i;8i;2i;8i];.mod.sub[mp;3;3;2i;8i;2i;8i];.mod.sub[mp;6;4;2i;8i;2i;8i]);
st: .mod.stack ms;
.t.ok["split then stack restores the state";all {x~y}'[st`br`syns`inc0`inc1;mp`br`syns`inc0`inc1]];
ms[1;`su]: (1b;0 70i); ms[2;`su]: (1b;enlist 5i);
.t.eq["stack offsets usage by each brain's first row (3 and 6 rows of 12 cells x 64)";(.mod.stack ms)[`su] 1;2304 2374 4613i];
/ transferIn: units placed + units lost = units sent; first unit sets inc0, second inc1
tb: 3 12#0i; tb[0;4]: 1i; tb[0;5]: 2i;
tr: .mod.transferIn[tb;3 12#0i;3 12#0i;2i;0 0 2 1 1 0 0 0i;7i];
.t.eq["transferIn conserves units (placed + lost = sent)";((sum raze tr 0)-sum raze tb)+tr 3;4i];
.t.eq["transferIn: a unit into a MEDIUM cell makes it HIGH and sets inc1";(tr[0;0;4];tr[1;0;4];tr[2;0;4]);2 0 7i];
.t.eq["transferIn: a unit into an empty cell sets inc0";(tr[0;0;6];tr[1;0;6]);1 7i];
.t.eq["transferIn: units into full cells are lost (one at col 4, one at col 5)";tr 3;2i];
/ One modular step conserves energy: sight + touch + central + everything that left the central brain
ce: {sum raze x`br};
sB: ms 0; tB: ms 1; cB: ms 2; sB[`out]: 8#0i; tB[`out]: 8#0i;
before: (ce sB)+(ce tB)+ce cB;
sB: .engine.runCycle sB; tB: .engine.runCycle tB;
r: .mod.transferIn[cB`br;cB`inc0;cB`inc1;2i;sB`out;7i];
lost: r 3; r: .mod.transferIn[r 0;r 1;r 2;2i;tB`out;6i]; lost+: r 3;
cB[`br`inc0`inc1]: 3#r; cB: .engine.runCycle cB;
.t.eq["one modular step conserves energy (lost transfers and outputs counted)";(ce sB)+(ce tB)+(ce cB)+(sum cB`out)+lost;before];
/ .mod.lesion: routes into a stacked cell come only from neighbours in the same brain
ls0: 9 12 8 8#5i;
nz:{sum raze raze raze 0i=x};
.t.eq["lesion inside the touch brain (4:3) cuts all 8 routes in, 8 input labels each";nz .mod.lesion[1b;3;9;12;ls0;"4:3"];64i];
.t.eq["lesion on a brain's top row (3:3) doesn't reach across to the sight brain";nz .mod.lesion[1b;3;9;12;ls0;"3:3"];40i];
.t.ok["lesion on the touch brain's top row leaves sight-brain row 2 untouched";all 5i=raze raze raze (.mod.lesion[1b;3;9;12;ls0;"3:3"]) 2];
.t.eq["the same cell in a single brain has all 8 neighbours";nz .mod.lesion[0b;3;6;12;6 12 8 8#5i;"3:3"];64i];
.t.eq["several cells at once (comma list)";nz .mod.lesion[1b;3;9;12;ls0;"4:3,7:6"];128i];
.t.eq["a lesion zeroes the route out of the neighbour along the move that enters the cell";.mod.lesion[1b;3;9;12;ls0;"4:3"][4;2;;4];8#0i];

.t.done[];
