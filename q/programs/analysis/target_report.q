/ North-star target report (REVIVAL_PLAN.md §0, CATCH_RATE_PLAN.md §0): per arm, alignment and catches in the LAST
/ epoch of each run (bars 8,001-10,000 for a 10k-bar run), how many seeds reach the target, and paired differences
/ against the first arm (matched by brain id) with bootstrap 95% CIs.
/ Usage (repo root): q q/programs/analysis/target_report.q results/REF results/ARM ... [--target 0.7] < /dev/null

\c 60 250
system "S 42"     / fixed seed: reproducible bootstrap CIs
\l q/modules/analysis/arms.q
a: .z.x
ti: first where a~\:"--target"
TARGET: $[null ti; 0.7; "F"$a ti+1]
dirs: .arm.strip each a where not (til count a) in $[null ti; `long$(); ti+0 1]
if[0=count dirs; -2 "usage: q q/programs/analysis/target_report.q results/REF [results/ARM ...] [--target 0.7]"; exit 1]

ET: `epoch`bars`catches`towards`away`perp`meanAlignment!"IIIIIIF"
lastEpoch:{[d;s] f: hsym `$d,"/",s,"/epoch_summary.csv"; r: last (value ET;enlist ",") 0: f;
  `arm`id`epoch`align`c1k!(`$last "/" vs d;"I"$1_s;r`epoch;r`meanAlignment;1000f*r[`catches]%r`bars)}
T: raze {[d] lastEpoch[d] each .arm.runs[d;"epoch_summary.csv"]} each dirs
arms: distinct T`arm

-1 "=== LAST EPOCH PER ARM (target: alignment >= ",(string TARGET),") ===";
show select n:count i, epoch:max epoch, meanAlign:avg align, medAlign:med align, minAlign:min align, atTarget:sum align>=TARGET,
  catches1k:avg c1k by arm from T;

boot:{[d] b: asc {[d;x] avg d (count d)?count d}[d] each til 10000; (b 250;b 9749)}
pair:{[T;ref;l]
  j: (select id, rA:align, rC:c1k from T where arm=ref) ij `id xkey select id, A:align, C:c1k from T where arm=l;
  dA: j[`A]-j`rA; dC: j[`C]-j`rC; ca: boot dA; cc: boot dC;
  `arm`n`dAlign`alignLo`alignHi`dCatches1k`catchLo`catchHi`armBetter!(l;count dA;avg dA;ca 0;ca 1;avg dC;cc 0;cc 1;sum dA>0)}
if[1<count arms;
  -1 "";
  -1 "=== PAIRED vs ",(string first arms)," (arm minus reference, last epoch, 95% bootstrap CI; armBetter = seeds with higher alignment) ===";
  show pair[T;first arms] each 1_arms];

-1 "";
-1 "=== PER SEED: last-epoch alignment ===";
show exec arm!align by id from T;
exit 0
