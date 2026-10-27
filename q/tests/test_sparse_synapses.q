/ Sparse usage prototype checks before integrating with simulation state.
/ Run from repo root: q q/tests/test_sparse_synapses.q

\l q/modules/trinary.q
\l q/modules/directions.q
\l q/modules/synapses.q
\l q/modules/brain.q
\l q/modules/brain_engine.q
\l q/tests/harness.q

-1 "test_sparse_synapses";
usage:.syn.emptySparseUsage[];
.t.ok["tagged empty usage is sparse";.syn.isSparseUsage usage];
.t.eq["empty sparse indices";.syn.usageIndices usage;`int$()];
usage:.syn.addSparseUsage[usage;17i];
usage:.syn.addSparseUsage[usage;63i];
usage:.syn.addSparseUsage[usage;17i];
.t.eq["sparse usage deduplicates indices";.syn.usageIndices usage;17 63i];
.t.eq["flat synapse index formula";.syn.indexOf[2i;3i;4i;5i;12i];1765i];

syns: 1#enlist 1#enlist 8 8#100i;
used:.syn.sparseUsage[17 17 63i];
before:raze raze raze syns;
after:.syn.applySparseScale[syns;used;1.5];
flatAfter:raze raze raze after;
.t.eq["sparse strategic scale updates only used indices";flatAfter[17 63];150 150i];
otherIdx:(til count flatAfter) except 17 63;
.t.ok["sparse strategic scale leaves other weights alone";all flatAfter[otherIdx]=100i];
reinforced:raze raze raze .syn.applySparseReinforce[syns;used;12i];
.t.ok["sparse reinforcement preserves unselected weights";all reinforced[otherIdx]=100i];
punished:raze raze raze .syn.applySparsePunish[1#enlist 1#enlist 8 8#25i;used;12i];
.t.eq["sparse punishment floors used weights";punished[17 63];25 25i];

lookbackUsages:(.syn.sparseUsage[17 30 63i];.syn.sparseUsage[17 18 63i];.syn.sparseUsage[18 63i]);
sequential:syns;
sequential:.syn.applySparseScale[sequential;lookbackUsages[0];1.21];
sequential:.syn.applySparseScale[sequential;lookbackUsages[1];1.21];
sequential:.syn.applySparseScale[sequential;lookbackUsages[2];1.21];
counted:.syn.applySparseLookbackScale[syns;lookbackUsages;1.21];
seqFlat:raze raze raze sequential;
countFlat:raze raze raze counted;
.t.eq["counted lookback matches sequential route occurrence set";`int$where seqFlat<>100i;`int$17 18 30 63];
.t.ok["counted lookback rounding stays within 1 weight unit";all 1>=abs seqFlat-countFlat];
.t.eq["counted lookback respects max strength";max raze raze raze .syn.applySparseLookbackScale[2#enlist 3#enlist 8 8#1999999i;lookbackUsages;1.21];2000000i];

system "S 9265";
sequential:syns;
do[3; sequential:.syn.applySparseReinforce[sequential;used;12i]];
expectedNext:rand 1.0;
system "S 9265";
batched:.syn.applySparseReinforceN[syns;used;12i;3i];
gotNext:rand 1.0;
seqFlat:raze raze raze sequential;
batchedFlat:raze raze raze batched;
.t.ok["batched reinforcement uses the same random stream";gotNext=expectedNext];
.t.ok["batched reinforcement rounding remains bounded";all 3>=abs seqFlat-batchedFlat];

system "S 31415";
sequential:syns;
do[3; sequential:.syn.applySparsePunish[sequential;used;12i]];
expectedNext:rand 1.0;
system "S 31415";
batched:.syn.applySparsePunishN[syns;used;12i;3i];
gotNext:rand 1.0;
seqFlat:raze raze raze sequential;
batchedFlat:raze raze raze batched;
.t.ok["batched punishment uses the same random stream";gotNext=expectedNext];
.t.ok["batched punishment rounding remains bounded";all 3>=abs seqFlat-batchedFlat];

denseFlat:(2*3*64)#0b;
denseFlat:@[denseFlat;17 63 150i;:;3#1b];
denseSlot:2 3 8 8#denseFlat;
denseHist:3#enlist denseSlot;
sparseHist:.syn.denseHistToSparse denseHist;
.t.eq["dense history converts to tagged index sets";.syn.usageIndices sparseHist[0];17 63 150i];
.t.ok["sparse history converts back to identical dense masks";.syn.sparseHistToDense[sparseHist;2i;3i]~denseHist];
.t.ok["sparse history initializes tagged empty slots";.syn.isSparseUsage first .syn.initSparseHist[4i]];

sys:.engine.initSystem[6i;12i;8i;8i;2i;2i];
br:sys`br; br[2;5]:1i;
inc0:sys`inc0; inc0[2;5]:7i;
shape:(6i;12i;2i;8i;2i;8i);
system "S 271828";
dense:.brain.propagate[shape;br;sys`syns;sys`su;inc0;sys`inc1;8#0i];
system "S 271828";
sparse:.brain.propagate[shape;br;sys`syns;.syn.emptySparseUsage[];inc0;sys`inc1;8#0i];
.t.ok["sparse propagation preserves brain state";(dense 0)~(sparse 0)];
.t.ok["sparse propagation preserves synapse weights";(dense 1)~(sparse 1)];
.t.ok["sparse propagation preserves incoming labels";(dense 3;dense 4)~(sparse 3;sparse 4)];
.t.ok["sparse propagation preserves output";(dense 5)~(sparse 5)];
.t.ok["sparse route set matches dense usage mask";all (.syn.usageIndices[sparse 2])=(`int$where raze raze raze (dense 2))];

/ G.3(b) log-scaled rewards: lam=1 is the original update; lam scales each synapse's log-change;
/ the RNG stream is consumed exactly as by the unscaled update
system "S 21"; base:.syn.init[6;12]; u:.syn.sparseUsage 3 50 777 4000i;
fl:{raze raze raze x};
system "S 31"; a:.syn.applySparseReinforceN[base;u;12i;4i]; ra:rand 1.0;
system "S 31"; b:.syn.applySparseReinforceNL[base;u;12i;4i;1f]; rb:rand 1.0;
.t.ok["reinforce lam=1 identical to unscaled";a~b];
system "S 31"; c:.syn.applySparseReinforceNL[base;u;12i;4i;0.25]; rc:rand 1.0;
.t.ok["reinforce lam=0.25 uses the same RNG draws";(ra=rb)&ra=rc];
lo:log `float$(fl base) 3 50 777 4000; la:(log `float$(fl a) 3 50 777 4000)-lo; lc:(log `float$(fl c) 3 50 777 4000)-lo;
.t.near["reinforce lam=0.25 gives a quarter of the log-change";lc;0.25*la;0.05];
system "S 31"; d:.syn.applySparsePunishN[a;u;12i;2i];
system "S 31"; e:.syn.applySparsePunishNL[a;u;12i;2i;0.5];
lp:(log `float$(fl d) 3 50 777 4000)-log `float$(fl a) 3 50 777 4000;
le:(log `float$(fl e) 3 50 777 4000)-log `float$(fl a) 3 50 777 4000;
.t.near["punish lam=0.5 gives half the log-change";le;0.5*lp;0.05];
.t.ok["unused synapses untouched by scaled reinforce";(fl[base] til 3)~fl[c] til 3];
.t.eq["scaleMult lam=1 returns m";.syn.scaleMult[1.5;1f];1.5];
.t.near["scaleMult lam=0.5 is the square root";.syn.scaleMult[1.5;0.5];sqrt 1.5;1e-12];

/ G.R2 policy-gradient update: shape, zero row-sum, finite-difference gradient, untouched rows
system "S 41"; pgBase:.syn.init[6;12];
fl:{raze raze raze x};
uPG:.syn.sparseUsage ((8*100)+3),(8*250)+0 5;   / parenthesised: q is right-to-left     / state 100 took route 3; state 250 took 0 and 5
b:.dirs.vBias; eta:0.2; dl:0.5;
pgNew:.syn.applySparsePG[pgBase;uPG;b;eta;dl];
th0:log `float$fl pgBase; th1:log `float$fl pgNew;
r100:(8*100)+til 8; r250:(8*250)+til 8;
pRow:{[b;th;r] e:(exp th r)*b; e%sum e};
expected100: eta*dl*((til 8)=3) - pRow[b;th0;r100];
/ weights are ints, so log values carry rounding error up to ~0.5/W (W ~ 70-1000 here)
.t.near["PG chosen +(1-p), siblings -p (state 100, within int rounding)";th1 r100;(th0 r100)+expected100;0.008];
.t.near["PG row log-change sums to ~0 (state 100)";sum (th1-th0) r100;0f;0.02];
n250:`float$(til 8) in 0 5;
.t.near["PG two used routes: n - 2p (state 250)";(th1-th0) r250;eta*dl*n250-2*pRow[b;th0;r250];0.008];
/ finite difference: d ln p(3) / d theta_o equals 1[o=3] - p_o
fd:{[b;th;r;o] eps:1e-6; th2:@[th;r o;+;eps]; ((log pRow[b;th2;r] 3)-log pRow[b;th;r] 3)%eps}[b;th0;r100] each til 8;
.t.near["PG gradient matches finite difference of ln p";fd;((til 8)=3)-pRow[b;th0;r100];1e-4];
untouched:not (til count fl pgBase) in r100,r250;
.t.ok["PG leaves unused states untouched";(fl[pgBase] where untouched)~fl[pgNew] where untouched];
.t.ok["PG with delta=0 is a no-op";pgBase~.syn.applySparsePG[pgBase;uPG;b;eta;0f]];
system "S 51"; ra:rand 1.0; system "S 51"; .syn.applySparsePG[pgBase;uPG;b;eta;dl]; rb:rand 1.0;
.t.ok["PG uses no RNG";ra=rb];

.t.done[];
