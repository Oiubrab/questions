/ Experiment arms on disk (.arm): an arm is a results directory holding one s<id>/ run per brain id.
/ Shared by the analysis programs in q/programs/analysis/. Load with \l q/modules/analysis/arms.q

\d .arm

/ Drop a trailing slash from a path given on the command line
strip:{$["/"=last x; -1_x; x]}

/ The run directories (s<id> names) of arm d that contain file f, e.g. runs["results/ARM";"summary.csv"]
runs:{[d;f]
  d: strip d;
  subs: {x where x like "s*"} string key hsym `$d;
  subs where {[d;f;s] (`$f) in key hsym `$d,"/",s}[d;f] each subs}

\d .
