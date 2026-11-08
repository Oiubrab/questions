/ Scratch: per-slice policy classes (from weights) and execution loss (from per-bar logs), last 2000 bars.
\l q/load.q
\l q/modules/analysis/theory.q
\l q/modules/world/relearn.q
\c 60 250
arm: first .z.x
ids: 1+til 16
perm: .relearn.ANGLE_ORDER
slotAngleCos:{[s;a] cos ((a*45f)-(s*45f)+22.5)*.vision.PI%180f}   / slice s centre vs slot a angle
A:{[w] .theory.policyG[.theory.chainMod[3#w;6_w;.dirs.vBias];24]}
rowf:{[id]
  w:(get hsym `$arm,"/s",(string id),"/weights_seed",(string id),".bin") 0;
  P:A w; P:P%sum each P;
  top: {first idesc x} each P; share: max each P;
  / offset of top slot from slice in slots, circular, where 0 or +1 are the two correct edges
  off: {[s;a] ((a-s)+4) mod 8}'[til 8;top]-4;
  l:("IIIIIIIIIIFIFI";enlist ",") 0: `$":",arm,"/s",(string id),"/simulation_log.csv";
  l:select from l where bar>8000;
  ([] id:id; slice:til 8; off:off; share:share; pred:(sum each P*.theory.cosM[;perm]);
     meas:{[l;s] exec avg alignment from l where visionSlice=s}[l] each til 8)}
t: raze rowf each ids;
-1 "--- top-slot offset from slice (0,+1 correct edges) over 16 seeds x 8 slices, last-state policy";
show `off xasc select n:count i, pred:avg pred, meas:avg meas, share:avg share by off from t;
-1 "--- by slice";
show select correct:avg off in 0 1, pred:avg pred, meas:avg meas by slice from t;
-1 "--- by seed";
show select correct:sum off in 0 1, pred:avg pred, meas:avg meas, loss:(avg pred)-avg meas by id from t;
-1 "overall pred ",(string exec avg pred from t)," meas ",string exec avg meas from t;
-1 "--- pooled: predicted vs measured given a slice's top slot is correct";
show select n:count i, pred:avg pred, meas:avg meas by correct:off in 0 1 from t;
/ per-bar execution facts
ex:{[id]
  l:("IIIIIIIIIIFIFI";enlist ",") 0: `$":",arm,"/s",(string id),"/simulation_log.csv";
  l:select from l where bar>8000;
  select id:first id_, bars:count i, moved:avg moveDist>0, dist2:avg moveDist=2, oe1:avg outEnergy=1, oe2p:avg outEnergy>1, alOne:avg alignment where outEnergy=1, alMany:avg alignment where outEnergy>1 from update id_:id from l}
e: raze ex each ids;
-1 "--- execution by seed (last 2000 bars)"; show e;
-1 "means"; show avg each flip delete id from e;
