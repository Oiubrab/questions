/ Synapse update vectorization must preserve nested outputs and RNG consumption.
/ Run from repo root: q q/tests/test_synapses.q

\l q/modules/trinary.q
\l q/modules/directions.q
\l q/modules/synapses.q
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

.t.done[];
