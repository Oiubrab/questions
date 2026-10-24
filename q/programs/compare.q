/ Compare experiment arms produced by scripts/run_trials.sh
/ Usage (repo root): q q/programs/compare.q results/<ref> results/<other> ... < /dev/null
/ The first directory is the reference arm. Other arms are paired with it by brain
/ id (the s<id> directory name), so every comparison is within-brain.

dirs: {$["/"=last x; -1_x; x]} each .z.x
if[0=count dirs; -2 "usage: q q/programs/compare.q results/<ref> results/<other> ..."; exit 1]
system "S 42"    / fixed seed so bootstrap CIs are reproducible
system "c 50 250" / wide console so epoch tables aren't truncated

/ Read one CSV from every s<id>/ subdirectory of an arm, tagging rows with label and id
loadArm:{[types;file;d]
  subs: {x where x like "s*"} string key hsym `$d;
  subs: subs where {[file;d;s] (`$file) in key hsym `$d,"/",s}[file;d] each subs;
  t: raze {[types;file;d;s] update id:"I"$1_s from (types;enlist ",") 0: hsym `$d,"/",s,"/",file}[types;file;d] each subs;
  update label:`$last "/" vs d from t}

runs: raze loadArm["IIBBBSIIIIIF";"summary.csv"] each dirs
eps:  raze loadArm["IIIIII";"epoch_summary.csv"] each dirs
labels: distinct runs`label

-1 "=== PER-ARM SUMMARY (catches per run) ===";
show select n:count i, mean:avg catches, sd:sdev catches, minimum:min catches, maximum:max catches,
            per1kBars:avg 1000*catches%bars, towardsPct:avg 100*towards%totalMoves
     by label from runs;

/ Paired comparison of arm l against the reference arm, matched by brain id.
/ t is the paired t statistic; ci95 is a bootstrap CI of the mean paired difference.
pair:{[runs;refL;l]
  j: (select id, refC:catches from runs where label=refL) ij `id xkey select id, otherC:catches from runs where label=l;
  d: `float$j[`otherC]-j`refC;
  n: count d; m: avg d;
  boot: asc {[d;x] avg d (count d)?count d}[d] each til 10000;
  `arm`n`meanRef`meanArm`meanDiff`pctChange`t`ci95lo`ci95hi!
    (l; n; avg j`refC; avg j`otherC; m; 100*m%avg j`refC; m%(sdev d)%sqrt n; boot 250; boot 9749)}

if[1<count labels;
  -1 "";
  -1 "=== PAIRED DIFFERENCES vs ",(string first labels)," (arm minus reference, matched by brain id) ===";
  show pair[runs;first labels] each 1_labels];

-1 "";
-1 "=== CATCHES PER 1000 BARS BY EPOCH (mean over brains) ===";
/ Pivot to one column per epoch; arms may have different epoch counts (missing -> null)
er: select rate:avg 1000*catches%bars by label, epoch from eps;
ec: asc distinct exec epoch from er;
d: exec epoch!rate by label from er;
show 1!([] label:key d) ,' flip (`$"e",/:string ec)!flip (value d)@\:ec;
