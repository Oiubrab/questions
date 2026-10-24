/ Substrate lab module (.sub): faithful to .brain.propagate by default, and still
/ energy-conserving under every lab setting (REVIVAL_PLAN.md task S.1)
/ Run from repo root: q q/tests/test_substrate.q

\l q/load.q
\l q/modules/substrate.q
\l q/tests/harness.q

-1 "test_substrate";

/ Random state at ~50% pressure satisfying the direction bookkeeping invariants
randState:{[nR;nC]
  br: (nR;nC)#(nR*nC)?3i;
  inc0: (nR;nC)#(1i+(nR*nC)?8i) * 0<raze br;
  inc1: (nR;nC)#((nR*nC)?9i) * 2=raze br;
  (br;inc0;inc1)}

/ k steps of .brain.propagate vs .sub.propagate from the same state and RNG seed
viaBrain:{[st;syns;k]
  system "S 77";
  acc:(st 0;syns;.syn.initSU[6;12];st 1;st 2;8#0i);
  do[k; acc:.brain.propagate[(6;12;2;8;2;8);acc 0;acc 1;acc 2;acc 3;acc 4;8#0i]];
  acc}
viaSub:{[st;syns;k]
  system "S 77";
  acc:(st 0;syns;.syn.initSU[6;12];st 1;st 2;8#0i);
  do[k; acc[5]:8#0i; acc:.sub.propagate[.sub.defaults;acc]];
  acc}

system "S 5";
st: randState[6;12];
syns: .syn.init[6;12];
.t.ok["defaults reproduce .brain.propagate bit for bit (50 steps)";viaBrain[st;syns;50]~viaSub[st;syns;50]];

/ Conservation under lab settings: sealed + wrapped, every temp and order
cons:{[cfg;k]
  system "S 3";
  st: randState[cfg`nR;cfg`nC];
  acc:(st 0;.syn.init[cfg`nR;cfg`nC];.syn.initSU[cfg`nR;cfg`nC];st 1;st 2;`int$0#0i);
  e0: sum raze acc 0;
  ok: 1b;
  do[k; acc:.sub.propagate[cfg;acc]; ok: ok & e0=sum raze acc 0];
  ok & (all (raze acc 0) within 0 2)}

base: .sub.defaults,`nR`nC`wrap`sink`outLen!(10i;10i;1b;0b;0i);
.t.ok["wrapped + sealed conserves energy, temp 1";cons[base;100]];
.t.ok["wrapped + sealed conserves energy, temp 0.3";cons[base,(enlist `temp)!enlist 0.3;100]];
.t.ok["wrapped + sealed conserves energy, temp 0 (argmax)";cons[base,(enlist `temp)!enlist 0f;100]];
.t.ok["wrapped + sealed conserves energy, random order, flat bias";cons[base,`order`bias!(`random;8#1f);100]];
.t.ok["walls + sealed conserves energy";cons[base,(enlist `wrap)!enlist 0b;100]];

/ Wrapping: energy at the right edge can move east onto the left edge
cfgW: .sub.defaults,`nR`nC`wrap`sink`outLen`bias`hebb!(3i;3i;1b;0b;0i;0 0 0 0 1 0 0 0f;0i);
accW: (3 3#0 0 0 0 0 1 0 0 0i; .syn.init[3;3]; .syn.initSU[3;3]; 3 3#0 0 0 0 0 1 0 0 0i; 3 3#0i; `int$());
accW: .sub.propagate[cfgW;accW];
.t.eq["wrap: E move from right edge lands on left edge";accW 0;3 3#0 0 0 1 0 0 0 0 0i];

/ hebb=0 leaves synapses untouched
s0: .syn.init[10;10];
system "S 3"; st: randState[10;10];
accH: .sub.propagate[base,(enlist `hebb)!enlist 0i;(st 0;s0;.syn.initSU[10;10];st 1;st 2;`int$())];
.t.ok["hebb 0 leaves synapses unchanged";s0~accH 1];

.t.done[];
