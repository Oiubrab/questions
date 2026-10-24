/ Summarise a substrate-lab sweep: average each configuration over its seeds
/ Usage (repo root): q q/programs/lab_report.q results/lab/<sweep> < /dev/null

if[0=count .z.x; -2 "usage: q q/programs/lab_report.q results/lab/<sweep>"; exit 1]
d: {$["/"=last x; -1_x; x]} first .z.x
system "c 60 250"

/ Column types must match summary.csv as written by substrate_lab.q
TYPES: "IIBSFIBSIFIFFFFFFFFFIIJF"
/ Run dirs are those containing summary.csv (names may contain dots, e.g. t0.3)
subs: string key hsym `$d
subs: subs where {[d;s] `summary.csv in key hsym `$d,"/",s}[d] each subs
t: raze {[d;s] (TYPES;enlist ",") 0: hsym `$d,"/",s,"/summary.csv"}[d] each subs
/ rows/cols are q built-ins and misbehave as column names in queries: rename by position
t: (`nRows`nCols,2_cols t) xcol t

-1 "runs: ",string count t;
-1 "";
-1 "Metric guide (means over the second half of each run, then over seeds):";
-1 "  activity  fraction of cells changing per step     saturated/empty  fraction of cells at 2 / at 0";
-1 "  persist10 corr(state now, 10 steps ago): ~1 frozen, ~0 random churn, between = lasting structure";
-1 "  sharp     mean probability of the most likely route    turnover  fraction of strongest routes that changed per sample";
-1 "  spreadR/C coefficient of variation of energy across rows/columns (banding or piling)";
-1 "";
r: select n:count i, energy:avg energy, activity:avg activity, saturated:avg saturated, empty:avg empty,
          spreadR:avg rowSpread, spreadC:avg colSpread, persist10:avg persist10, sharp:avg sharp,
          turnover:avg turnover, lost:avg e0-eEnd, secs:avg secs
   by nRows, nCols, wrap, bias, temp, hebb, sink, order from t;
show `persist10 xdesc r;
