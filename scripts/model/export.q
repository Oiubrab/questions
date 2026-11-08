/ Export the stacked primary weights (sight, touch, central; 9x12x8x8) of every run of one or more arms as flat text,
/ one file per run: OUT/<arm>_<id>.txt. The model (model.py load()) reads them.
/ Usage (repo root): q scripts/model/export.q OUT ARM [ARM ...] < /dev/null      e.g. OUT=results/model/w ARM=o_open_brain_20261103
\l q/modules/analysis/arms.q
out: first .z.x
f:{[a;s] id: 1_s; w:(get hsym `$"results/",a,"/",s,"/weights_seed",id,".bin") 0;
  (hsym `$out,"/",a,"_",id,".txt") 0: string raze raze raze w;}
{[a] f[a] each .arm.runs["results/",a;"summary.csv"]} each 1_.z.x;
exit 0
