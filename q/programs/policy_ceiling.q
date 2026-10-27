/ Where is the alignment ceiling: in the weights, or in execution? (REVIVAL_PLAN.md G.R0)
/ For each seed of each arm, compute the policy from the final saved weights alone (linear chain:
/ the vision unit only, a 12-step bar, T=1, vBias, saturation ignored) and compare its expected
/ alignment with the alignment the run actually achieved in its last epoch.
/ Usage (repo root): q q/programs/policy_ceiling.q results/<arm> [results/<arm2> ...] < /dev/null
/ Optional: -seeds to print the per-seed table. Best possible E[cos] with 8 slices x 8 moves ~ 0.90;
/ predicted E[cos] uses slice-centre angles, so it reads ~2-3% optimistic vs true-angle alignment.

\l q/load.q
\l q/modules/theory.q
\c 50 220

a: .z.x
showSeeds: any a like "-seeds"
dirs: {$["/"=last x; -1_x; x]} each a where not a like "-*"
if[0=count dirs; -2 "usage: q q/programs/policy_ceiling.q results/<arm> [...] [-seeds]"; exit 1]

/ One seed: weights-only policy statistics plus measured last-epoch and whole-run alignment
seedRow:{[d;s]
  id: "I"$1_s;
  w: get hsym `$d,"/",s,"/weights_seed",(string id),".bin";
  pr: .theory.stats .theory.policy .theory.chain[w 0;.dirs.vBias];
  f: hsym `$d,"/",s,"/epoch_summary.csv";
  e: ((`epoch`bars`catches`towards`away`perp`meanAlignment!"IIIIIIF") `$"," vs first read0 f;enlist ",") 0: f;
  (`id`predExpCos`predPMove`predMI!(id; pr`expCos; pr`pMove; pr`MIbits)),
    `measLastEpoch`measRun!(last e`meanAlignment; (sum e[`bars]*e`meanAlignment)%sum e`bars)}

arm:{[d]
  subs: {x where x like "s*"} string key hsym `$d;
  subs: subs where {[d;s] (`$"epoch_summary.csv") in key hsym `$d,"/",s}[d] each subs;
  t: `id xasc seedRow[d] each subs;
  if[showSeeds; -1 "--- ",d; show t];
  `arm`n`predExpCos`measLastEpoch`gapInExecution`predPMove`predMI`corrPredMeas!
    (`$last "/" vs d; count t; avg t`predExpCos; avg t`measLastEpoch;
     (avg t`predExpCos)-avg t`measLastEpoch; avg t`predPMove; avg t`predMI;
     t[`predExpCos] cor t`measLastEpoch)}

-1 "Weights-only predicted alignment vs measured (best possible ~0.90):";
show arm each dirs;
