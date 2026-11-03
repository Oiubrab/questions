/ Memory decoding (OBSTACLES_PLAN.md I.2): can the brain's state tell where the mouse was last seen?
/ Usage (repo root): q q/programs/analysis/decode.q results/ARM < /dev/null
/ ARM holds s<id>/ runs made with --log-state (state_log.bin) in a world where the mouse hides.
/ Features: each cell's energy and incoming label at the end of the bar. Label: the vision slice in
/ which the mouse was last seen. A nearest-centroid classifier is trained on the odd seeds and tested
/ on the even seeds, separately for each band of bars since the mouse was last seen (0 = visible this
/ bar, the positive control). Chance is 1/8; the shuffled control trains on randomly permuted labels.
/ nonEmpty: share of those bars whose brain still holds any energy at the end of the bar.

\c 40 200
system "S 7"
\l q/modules/analysis/arms.q
a: .z.x
if[0=count a; -2 "usage: q q/programs/analysis/decode.q results/ARM"; exit 1]
d: .arm.strip first a
subs: .arm.runs[d;"state_log.bin"]
if[0=count subs; -2 "no state_log.bin under ",d," (run with --log-state)"; exit 1]

/ One seed's samples: (features; label; band) for bars with a last-seen slice
BANDS: (0 0;1 2;3 5;6 10;11 20;21 1000)
load1:{[d;s] L: get hsym `$d,"/",s,"/state_log.bin";
  k: where L[`lastSeen]>=0;
  f: `float$(L[`br] k),'(L[`inc0] k);
  b: {[B;x] first where x within/: B}[BANDS] each L[`sinceSeen] k;
  (f; L[`lastSeen] k; b; "I"$1_s)}
S: load1[d] each subs
odd: S where 1=(S@\:3) mod 2; even: S where 0=(S@\:3) mod 2

/ Nearest centroid: centroid of each label's training features; predict the closest
fit:{[F;y] (asc distinct y)!{[F;y;c] avg F where y=c}[F;y] each asc distinct y}
pred:{[C;F] (key C) {[V;x] first iasc {sum x*x} each V -\: x}[value C] each F}
acc:{[tr;te;bd;shuf]
  Ft: raze tr@\:0; yt: raze tr@\:1; bt: raze tr@\:2;
  Fe: raze te@\:0; ye: raze te@\:1; be: raze te@\:2;
  i: where bt=bd; j: where be=bd;
  if[(0=count i)|0=count j; :0n];
  y: yt i; if[shuf; y: (neg count y)?y];
  avg ye[j]=pred[fit[Ft i;y];Fe j]}

/ Share of bars in the band whose brain holds any energy at the end of the bar (an empty brain can't remember)
live:{[bd] F: raze S@\:0; b: raze S@\:2; nb: count F 0; avg 0<sum each (nb div 2)#/:F where b=bd}
r: {[bd] `band`sinceSeen`testBars`nonEmpty`accuracy`shuffled!(bd; `$"-" sv string BANDS bd; sum (raze even@\:2)=bd; live bd; acc[odd;even;bd;0b]; acc[odd;even;bd;1b])} each til count BANDS
-1 "Decoding the last-seen slice from the brain state (chance 0.125); arm ",d;
show r;
