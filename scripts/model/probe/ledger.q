\l q/load.q
\c 60 240
S: first .z.x
ids:1+til 16
one:{[id]
  p: get hsym `$S,"/pr",(string id),"/probe.bin";
  L:p 0; L1:p 1; U:p 2; C:p 3; N:p 4; B:p 5;
  q: C%N;     / mean move cosine over a route's uses
  cls: ?[q>0.7;`good;?[q>=0f;`neutral;`bad]];
  / phase B = after bar 1000
  LB: L-L1; 
  uses: sum U;
  keep: where N>=20;     / routes used enough to classify
  / NB primary rows only (uses from stacked state)
  t: ([] id:id; route:keep; cls:cls keep; N:N keep; dpos:L[`dpos] keep; dneg:L[`dneg] keep; strat:L[`strat] keep; catch:L[`catch] keep; decay:L[`decay] keep; hebb:L[`hebb] keep; w:`float$(p 7) keep;
    dposB:LB[`dpos] keep; dnegB:LB[`dneg] keep; stratB:LB[`strat] keep; catchB:LB[`catch] keep; decayB:LB[`decay] keep; hebbB:LB[`hebb] keep);
  t}
T: raze one each ids;
-1 "routes used >=20 times, classified by mean move cosine (good>0.7, neutral 0-0.7, bad<0). Log-weight per use, whole run:";
show select routes:count i, usesMean:avg N, wGeoMean:exp avg log w, atCap:avg w>=2000000, dpos:(sum dpos)%sum N, dneg:(sum dneg)%sum N, strat:(sum strat)%sum N, catch:(sum catch)%sum N, hebb:(sum hebb)%sum N, decay:(sum decay)%sum N by cls from T;
-1 "after bar 1000:";
show select dposB:(sum dposB)%sum N, dnegB:(sum dnegB)%sum N, stratB:(sum stratB)%sum N, catchB:(sum catchB)%sum N, hebbB:(sum hebbB)%sum N, decayB:(sum decayB)%sum N by cls from T;
-1 "";
b: raze {[id] p: get hsym `$S,"/pr",(string id),"/probe.bin"; b:p 5; flip `id`bar`thresh`scope`mag`rate`rein`pun`stratN`sat`slice`cos`nuse!(count[b]#id;b[;0];b[;1];b[;2];b[;3];b[;4];b[;5];b[;6];b[;7];b[;8];b[;9];b[;10];b[;11])} each ids;
b: update phase:?[bar<=1000;`p0_1k;?[bar<=4000;`p1_4k;`p4_10k]] from b;
-1 "per-bar controls:";
show select thresh:avg thresh, th60:avg thresh<=0.6001, th70:avg thresh within 0.69 0.71, scope:avg scope, s20:avg scope=20, s40:avg scope=40, s80:avg scope=80, mag:avg mag, m2:avg mag=2, stratOn:avg stratN>0, rein:avg rein, pun:avg pun, satCell:avg sat, nuse:avg nuse by phase from b;
-1 "per-seed scope and magnitude, after bar 4000 (with mean strategic gain per use = 0.0238*scope*mag):";
show select thresh:avg thresh, scope:avg scope, mag:avg mag, stratOn:avg stratN>0, align:avg cos, nuse:avg nuse by id from b where bar>4000;
