/ Phase R (researchNotes/CONSTANT_SCALING.md): constants derived from the brain's configuration.
/ The geometry must match .theory's exact chain; the rules must give today's tested constants on today's brain.
/ Run from repo root: q q/tests/test_scaling.q

\l q/tests/harness.q
\l q/load.q
\l q/modules/analysis/theory.q

-1 "test_scaling";
R: .scale.REF
F3: .scale.exitCdf[R;.dirs.vBias;60]
/ the same quantity from .theory's directed-state chain (sight then central; one step saved by the same-step hand-off)
flat:{[nR] nR#enlist 12#enlist 8 8#1f}
QR: .theory.chainMod[flat 3;flat 3;.dirs.vBias]
x: avg {[Q;s] @[(count Q)#0f;(8*2+s)+6;:;1f]}[QR 0] each til 8
e: (); do[61; e,:sum x mmu QR 1; x: x mmu QR 0]
.t.near["exit share by step matches the .theory chain (modular, central rows 3)";F3;60#1_sums e;1e-12];
Fs: .scale.exitCdf[@[R;`modular`centRows;:;(0b;6)];.dirs.vBias;30]
QR2: .theory.chainG[flat 6;.dirs.vBias]
x: avg {[Q;s] @[(count Q)#0f;(8*2+s)+6;:;1f]}[QR2 0] each til 8
e: (); do[30; e,:sum x mmu QR2 1; x: x mmu QR2 0]
.t.near["exit share by step matches the .theory chain (single 6x12 brain)";Fs;sums e;1e-12];
.t.near["today's brain passes 0.599 of an untrained input per 12-step bar";F3 11;0.5993827;1e-6];

Fref: F3 11
st:{[c] .scale.steps[.scale.exitCdf[@[R;`centRows;:;c];.dirs.vBias;400];Fref]}
.t.eq["steps per bar: today's brain keeps 12";st 3;12i];
.t.eq["steps per bar for central rows 2, 4, 6, 9 (CONSTANT_SCALING §2)";st each 2 4 6 9;10 15 21 29i];
.t.eq["steps per bar for the single 6x12 brain";.scale.steps[.scale.exitCdf[@[R;`modular`centRows;:;(0b;6)];.dirs.vBias;400];Fref];15i];

P: .scale.punishStep[12;0.25]
.t.near["one punish call at 12 steps, reward scale 0.25: 0.25 x ln(1/(0.8 x 0.95^12))";P;0.2096658;1e-6];
C: .scale.commonMode[0.25;20;1;1.1;20;0.1265;1.5]
.t.near["common mode: 0.25 x (20 ln 1.1 + 20 x 0.1265 ln 1.5)";C;0.7330076;1e-6];
.t.eq["punish calls for today's brain: 4";.scale.punishCalls[P;C;1.1];4i];
.t.eq["punish calls at central rows 6 (S 21) and 9 (S 29): 3 and 2";{.scale.punishCalls[.scale.punishStep[x;0.25];C;1.1]} each 21 29;3 2i];
.t.eq["the reward scale cancels: same calls at reward scale 1";.scale.punishCalls[.scale.punishStep[12;1f];.scale.commonMode[1f;20;1;1.1;20;0.1265;1.5];1.1];4i];
.t.eq["at least one call even with no common mode";.scale.punishCalls[P;0f;1.1];1i];
.t.eq["hebb for today's brain: 252";.scale.hebb[100;4*P;1.5];252i];
.t.ok["hebb meets the re-learning limit with the margin";(log 1+252%100)>=1.5*4*P];
.t.eq["hebb at central rows 6 and 9: 332 and 261";(.scale.hebb[100;3*.scale.punishStep[21;0.25];1.5];.scale.hebb[100;2*.scale.punishStep[29;0.25];1.5]);332 261i];
.t.done[];
