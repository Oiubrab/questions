/ Phase W report (OBSTACLES_PLAN.md W.0-W.3): how each arm hunts when the mouse can be hidden.
/ Usage (repo root): q q/programs/analysis/world_report.q results/ARM [results/ARM2 ...] < /dev/null
/ Each ARM holds s<id>/ runs with summary.csv and world_log.csv (written when a W flag is on; without
/ one, simulation_log.csv is read and the mouse counts as always visible).
/ Arms are compared as given; put the scripted baselines (random, reactive, persist, greedy) beside
/ the brain arms on the same world, so "better than memoryless" can be read off directly.

\c 60 250
\l q/modules/analysis/arms.q
a: .z.x
if[0=count a; -2 "usage: q q/programs/analysis/world_report.q results/ARM [...]"; exit 1]

/ Typed read of the summary columns this report uses (others are skipped: type " ")
COLS: `seed`bars`catches`meanAlignment`visibleFrac`alignVisible`alignHidden`blockedFrac`catchTimeMed`world`blink`policy`hiddenReward!"IIIFFFFFFSSSS"
readSum:{[f] h: `$"," vs first read0 f; t: ((" "^COLS h);enlist ",") 0: f; first t}
WL_TYPES: "IBIBBFB"

/ One seed: summary row plus hidden-bar behaviour from world_log.csv
seedRow:{[d;s]
  r: readSum hsym `$d,"/",s,"/summary.csv";
  / A run without W flags writes no world_log.csv: the mouse was always visible
  w: $[`world_log.csv in key hsym `$d,"/",s; (WL_TYPES;enlist ",") 0: hsym `$d,"/",s,"/world_log.csv";
    [l: ("I     I  IF";enlist ",") 0: hsym `$d,"/",s,"/simulation_log.csv";
     ([] bar:l`bar; visible:(count l)#1b; sinceSeen:(count l)#0i; moved:0i<l`moveDist; blocked:(count l)#0b; alignment:l`alignment)]];
  hid: w where not w`visible;
  b: {[hid;lo;hi] k: hid where hid[`sinceSeen] within (lo;hi); avg k[`alignment] where not null k`alignment}[hid];
  r,`catchPer1k`moveRateVisible`moveRateHidden`alignHid1to5`alignHid6to20`alignHid21up!
    (1000f*r[`catches]%r`bars; avg (w`moved) where w`visible; avg hid`moved; b[1;5]; b[6;20]; b[21;0W])}

arm:{[d] d: .arm.strip d;
  subs: .arm.runs[d;"summary.csv"];
  if[0=count subs; -2 "no runs under ",d; exit 1];
  update arm:`$last "/" vs d from seedRow[d] each subs}

t: raze arm each a
-1 "catchPer1k: catches per 1,000 bars; catchTimeMed: median bars between catches (per seed, then median)";
-1 "moveRate*: fraction of bars the cat moves; alignHidN: mean alignment of moves N bars after losing sight";
-1 "alignment is the straight-line cosine to the mouse (through walls), so with walls it understates good detours";
show select seeds:count i, world:first world, blink:first blink, policy:first policy,
  catchPer1k:avg catchPer1k, catchTimeMed:med catchTimeMed, visibleFrac:avg visibleFrac,
  alignVisible:avg alignVisible, alignHidden:avg alignHidden, blockedFrac:avg blockedFrac
  by arm from t;
-1 "";
-1 "=== While the mouse is hidden ===";
show select moveRateVisible:avg moveRateVisible, moveRateHidden:avg moveRateHidden,
  alignHid1to5:avg alignHid1to5, alignHid6to20:avg alignHid6to20, alignHid21up:avg alignHid21up
  by arm from t;
