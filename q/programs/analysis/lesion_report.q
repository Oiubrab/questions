/ Lesion map report (OBSTACLES_PLAN.md I.4), written by scripts/probes/lesion_map.sh.
/ Usage (repo root): q q/programs/analysis/lesion_report.q results/lesion_ARM < /dev/null
/ For each lesioned cell: the change in mean alignment and in catches per 1,000 bars against the same
/ brain run intact (paired by seed), averaged over seeds, laid out as stacked rows x 12 columns.

\c 60 250
\l q/modules/analysis/arms.q
d: .arm.strip first .z.x
SUM: `seed`bars`catches`meanAlignment!"IIIF"
rd:{[f] h: `$"," vs first read0 f; first ((" "^SUM h);enlist ",") 0: f}
rows: raze {[d;s] cs: string key hsym `$d,"/",s;
  {[d;s;c] r: rd hsym `$d,"/",s,"/",c,"/summary.csv"; r,`seedDir`cell!(`$s;`$c)}[d;s] each cs where {[d;s;c] `summary.csv in key hsym `$d,"/",s,"/",c}[d;s] each cs}[d] each {x where x like "s*"} string key hsym `$d
t: ([] seedDir:rows@\:`seedDir; cell:rows@\:`cell; catch1k:{1000f*x[`catches]%x`bars} each rows; align:rows@\:`meanAlignment)
base: select seedDir, c0:catch1k, a0:align from t where cell=`intact
L: select dCatch:avg catch1k-c0, dAlign:avg align-a0, seeds:count i by cell from (t where not t[`cell]=`intact) lj `seedDir xkey base
L: update r:"I"$first each "_" vs' string cell, c:"I"$last each "_" vs' string cell from 0!L
-1 "Intact (frozen): mean catches per 1,000 bars ",(string avg base`c0),", alignment ",string avg base`a0;
-1 "Change in alignment when one cell is lesioned (rows = stacked brain rows, cols 0-11):";
show exec (`$string c)!0.01*floor 100*dAlign by r from `r`c xasc L;
-1 "Change in catches per 1,000 bars:";
show exec (`$string c)!floor dCatch by r from `r`c xasc L;
-1 "Most damaging cells:";
show 8 sublist `dAlign xasc select r, c, dAlign, dCatch, seeds from L;
