/ Synapse update vectorization must preserve nested outputs and RNG consumption.
/ Run from repo root: q q/tests/test_synapses.q

\l q/modules/brain/trinary.q
\l q/modules/brain/directions.q
\l q/modules/brain/synapses.q
\l q/tests/harness.q

-1 "test_synapses";
syns: 2#enlist 3#enlist 8 8#100i;
used: 2#enlist 3#enlist 8 8#1b;
unused: 2#enlist 3#enlist 8 8#0b;
oldReinforce:{[weights;usage;numSteps]
  mult:`float$1.2%0.95 xexp numSteps;
  {[mult;x;y] .syn.reinforceCell[mult]'[x;y]}[mult]'[weights;usage]};
oldPunish:{[weights;usage;numSteps]
  mult:`float$0.8*0.95 xexp numSteps;
  {[mult;x;y] .syn.punishCell[mult]'[x;y]}[mult]'[weights;usage]};
oldDecay:{[weights] {.syn.decayCell each x} each weights};

system "S 314";
expected:oldReinforce[syns;used;12i];
expectedNext:rand 1.0;
system "S 314";
got:.syn.applyReinforce[syns;used;12i];
gotNext:rand 1.0;
.t.ok["reinforce values and shape match nested reference";expected~got];
.t.eq["reinforce preserves RNG stream";gotNext;expectedNext];

system "S 2718";
expected:oldPunish[syns;used;12i];
expectedNext:rand 1.0;
system "S 2718";
got:.syn.applyPunish[syns;used;12i];
gotNext:rand 1.0;
.t.ok["punish values and shape match nested reference";expected~got];
.t.eq["punish preserves RNG stream";gotNext;expectedNext];

system "S 1618";
expected:oldDecay[syns];
expectedNext:rand 1.0;
system "S 1618";
got:.syn.applyDecay[syns];
gotNext:rand 1.0;
.t.ok["decay values and shape match nested reference";expected~got];
.t.eq["decay preserves RNG stream";gotNext;expectedNext];
.t.ok["unused synapses stay unchanged";.syn.applyReinforce[syns;unused;12i]~syns];

/ Catch-rate plan: applyDecay is applyDecayBy with the default constants; --decay-mult D gives lo = 1-D*(1-0.98),
/ span = D*0.015, which scales the mean loss (1.25% per period) by D; the floor holds
system "S 77";
Wd: (2;2;8;8)#`int$100000+900000*256?1f;
system "S 5"; d1: .syn.applyDecay[Wd];
system "S 5"; d1b: .syn.applyDecayBy[Wd;.syn.DECAY_LO;.syn.DECAY_SPAN];
.t.ok["applyDecay = applyDecayBy with DECAY_LO, DECAY_SPAN";d1~d1b];
loss:{[W;W2] avg 1-(`float$raze raze raze W2)%`float$raze raze raze W};
system "S 5"; dh: .syn.applyDecayBy[Wd;1f-0.5*1f-.syn.DECAY_LO;0.5*.syn.DECAY_SPAN];
.t.near["default mean loss is 1.25% per period";loss[Wd;d1];0.0125;0.0005];
.t.near["decay-mult 0.5 halves the mean loss";loss[Wd;dh];0.00625;0.0003];
oldFloor: .syn.MIN_STRENGTH; .syn.MIN_STRENGTH: 100i;
Wl: (2;2;8;8)#100i;
.t.ok["decay never takes a weight below a raised floor";all 100=raze raze raze .syn.applyDecayBy[Wl;.syn.DECAY_LO;.syn.DECAY_SPAN]];
.syn.MIN_STRENGTH: oldFloor;

/ G.R5 two-timescale weights
system "S 9";
W: (2;2;8;8)#`int$25+10000*256?1f;              / a 2x2 brain, weights 25..10025
Mw: log `float$raze raze raze W;
.t.ok["relaxDecay towards M = ln W leaves W (almost) unchanged";all 1>=abs (raze raze raze .syn.relaxDecay[W;Mw;80f])-raze raze raze W];
M0: (count Mw)#log 25f;
Wr: .syn.relaxDecay[W;M0;80f];
gap: (`float$raze raze raze W)-25f;
ratio: avg (((`float$raze raze raze Wr)-25f)%gap) where gap>1000;
.t.near["relaxDecay towards the floor keeps ~98.75% of the gap at tauF 80 (today's mean decay)";ratio;0.9875;0.002];
Wf: .syn.relaxDecay[W;M0;20f];
ratioF: avg (((`float$raze raze raze Wf)-25f)%gap) where gap>1000;
.t.near["tauF 20 relaxes 4x faster (keeps ~95%)";ratioF;0.95;0.004];
.t.ok["relaxDecay keeps weights within [floor, cap]";all (raze raze raze Wr) within (.syn.MIN_STRENGTH;.syn.MAX_STRENGTH)];
.t.eq["relaxDecay keeps the shape";count each (Wr;first Wr);2 2];
Mc: .syn.consolidate[W;M0;50f];
.t.near["consolidate moves M 1/tauS of the way to ln W";avg (Mc-M0)%(log `float$raze raze raze W)-M0;0.02;1e-9];
.t.near["consolidate: ln W is the fixed point";max abs .syn.consolidate[W;Mw;50f]-Mw;0f;1e-12];

.t.done[];
