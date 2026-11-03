/ Viz server: receives frames from the sim (q/modules/viz.q) and serves them to KX Dashboards.
/ Usage: q q/programs/viz_server.q -port 6814 < /dev/null &    then run the sim with --viz-port 6814
/ Replay a finished run (field and curves only; per-step energy is not in simulation_log.csv):
/   q q/programs/viz_server.q -port 6814 -replay results/seed42 -rate 50 < /dev/null &
/ Compare two finished runs side by side (dashboard "Cat Brain Compare"; dashboards plan V3.1/V3.2):
/   q q/programs/viz_server.q -port 6814 -replay results/A/s1 -replay2 results/B/s1 -rate 50 < /dev/null &
/ Each replayed directory's weights_seed*.bin, if present, gives that run's route map.
/ Listens on localhost only: the port accepts arbitrary q, so never expose it.

\l q/modules/directions.q

\d .viz

/ ROWS: rows shown for the primary side. The default modular brain is 9 rows (sight 0-2, touch 3-5,
/ central 6-8); a single-brain run (6 rows) fills the top 6. Frames of any row count are accepted.
ROWS: 9; COLS: 12; META_ROWS: 3; META_COLS: 5
HIST_FRAMES: 240      / energy frames kept for scrubbing (20 bars of 12 steps)
HIST_BARS: 5000

br: (ROWS;COLS)#0i
mbr: (META_ROWS;META_COLS)#0i
stepNo: 0; barNo: 0
frames: ()

bars: ([] bar:`int$(); mx:`float$(); my:`float$(); cx:`float$(); cy:`float$();
  slice:`int$(); energy:`int$(); action:`int$(); catches:`int$(); align:`float$(); pressure:`float$())

onStep:{[bar;step;b;m]
  br:: b; mbr:: m; barNo:: bar; stepNo:: step;
  frames:: (neg HIST_FRAMES) sublist frames, enlist (bar;step;b)}

barRow:{[bar;mouse;cat;rest] `bar`mx`my`cx`cy`slice`energy`action`catches`align`pressure!(bar;mouse 0;mouse 1;cat 0;cat 1),rest}
onBar:{[bar;mouse;cat;rest]
  / A lower bar number means a new run: start from a clean slate
  if[(0<count bars)&(bar<last bars`bar); bars:: 0#bars; frames:: ()];
  bars:: (neg HIST_BARS) sublist bars, enlist barRow[bar;mouse;cat;rest]}
/ The second run of a side-by-side replay
bars2: 0#bars
onBar2:{[bar;mouse;cat;rest] bars2:: (neg HIST_BARS) sublist bars2, enlist barRow[bar;mouse;cat;rest]}

/ ---- Route maps (V3.1): how sharply each cell's routes have formed, from a run's weights ----
/ For cell (r,c), route o's share is sum over incoming directions of W[r;c;d;o], times the downward
/ bias, normalised; sharp = the largest share (1/8 = no preference, 1 = one route), dom = its route.
routes: 2#enlist ([] row:`int$(); col:`int$(); sharp:`float$(); dom:`int$())
routesFrom:{[syns]
  t: raze {[syns;r] {[syns;r;c] s: .dirs.vBias*`float$sum syns[r;c]; p: s%sum s;
      `row`col`sharp`dom!(`int$r;`int$c;max p;`int$first idesc p)}[syns;r] each til count syns 0}[syns] each til count syns;
  t}
loadRoutes:{[k;dir]
  f: {x where x like "weights_seed*.bin"} string key hsym `$dir;
  if[0=count f; :()];
  routes[k]:: routesFrom first get hsym `$dir,"/",first f}
/ Bubble data: one bubble per cell coloured by sharpness band (r0 < 0.4 <= r1 < 0.7 <= r2), and a small
/ dot offset from the cell centre towards its dominant route (screen y points up, brain rows down)
routesNow:{[k]
  t: routes k;
  if[0=count t; :([] x:`float$(); y:`float$(); lvl:`symbol$())];
  cx: t[`col]+0.5; cy: (ROWS-0.5)-t`row;
  ([] x:cx; y:cy; lvl:`r0`r1`r2 (t[`sharp]>=0.4)+t[`sharp]>=0.7),
   ([] x:cx+0.32*.dirs.dc t`dom; y:cy-0.32*.dirs.dr t`dom; lvl:(count t)#`dir)}

/ ---- Dashboards queries ----

/ 72 rows: row 0 is the top (input) row
energyNow:{n: count br; ([] row:`int$raze COLS#'til n; col:`int$(n*COLS)#til COLS; energy:`int$raze br)}
metaNow:{([] row:`int$raze META_COLS#'til META_ROWS; col:`int$(META_ROWS*META_COLS)#til META_COLS; energy:`int$raze mbr)}

fieldT:{[bars]
  if[0=count bars; :([] who:`symbol$(); x:`float$(); y:`float$())];
  r:last bars;
  ([] who:`mouse`cat; x:r`mx`cx; y:r`my`cy), gazeNow[r]}
fieldNow:{fieldT bars}
fieldNow2:{fieldT bars2}

/ Dots fanned across the cat's active vision slice (slice k spans angle k*45deg..(k+1)*45deg from +x); none if no slice is active
gazeNow:{[r]
  if[r[`slice]<0; :([] who:`symbol$(); x:`float$(); y:`float$())];
  ang:raze 2#enlist ((acos -1)%4f)*r[`slice]+(1+til 4)%5f;
  rad:raze (4#8f;4#12f);
  ([] who:(count ang)#`gaze; x:r[`cx]+rad*cos ang; y:r[`cy]+rad*sin ang)}

/ Last n bars of positions; the field is toroidal for the mouse, so the trail is drawn as points, not a line
trailNow:{[n] trailT[bars;n]}
trailNow2:{[n] trailT[bars2;n]}
trailT:{[bars;n]
  t:(neg n) sublist bars;
  ([] who:(count t)#`mouseTrail; x:t`mx; y:t`my), ([] who:(count t)#`catTrail; x:t`cx; y:t`cy)}

/ Rolling catch rate over 200 bars and mean move alignment over 100 bars (nulls ignored)
statsNow:{[n] statsT[bars;n]}
statsNow2:{[n] statsT[bars2;n]}
statsT:{[bars;n]
  t:select bar, catches, align from bars;
  (neg n) sublist update rate:catches-0^200 xprev catches, alignAvg:mavg[100;align] from t}

status:{`bar`step`bars`frames!(barNo;stepNo;count bars;count frames)}

/ Headline numbers as (stat;value) strings for a grid; the 200-bar catch rate matches rateCounter in the sim
kpiNow:{
  if[0=count bars; :([] stat:`symbol$(); val:())];
  r:last bars; s:last statsNow[1];
  ([] stat:(`Bar`Step`Catches),`$("Catches (last 200 bars)";"Move alignment (last 100)";"Brain energy";"Output action";"Vision slice");
      val:string each (r`bar;stepNo;r`catches;s`rate;0.01*floor 100*0f^s`alignAvg;r`energy;r`action;r`slice))}

/ ---- replay of a finished run ----
replayRows: ()
replayRows2: ()
replayPos: 0
readLog:{[dir] ("IIIIIIIIIIFIFI"; enlist ",") 0: hsym `$dir,"/simulation_log.csv"}
feed:{[f;r] f[r`bar; `float$(r`mouseX;r`mouseY); `float$(r`catX;r`catY);
          (r`visionSlice; r`brainEnergy; r`outAction; r`catches; r`alignment; r`pressure)]}
/ dir2 empty: replay one run. Otherwise both runs advance one bar per tick, side by side.
replay:{[dir;dir2;perSec]
  replayRows:: readLog dir; replayPos:: 0; bars:: 0#bars; bars2:: 0#bars; loadRoutes[0;dir];
  if[count dir2; replayRows2:: readLog dir2; loadRoutes[1;dir2]];
  `.z.ts set {
    if[replayPos<count replayRows; feed[onBar;replayRows replayPos]];
    if[replayPos<count replayRows2; feed[onBar2;replayRows2 replayPos]];
    replayPos+:1};
  system "t ",string `int$1000%perSec}

\d .

args: .Q.opt .z.x
port: first "J"$args`port
system "p 127.0.0.1:",string port
if[`replay in key args; .viz.replay[first args`replay; $[`replay2 in key args; first args`replay2; ""]; $[`rate in key args; "F"$first args`rate; 20f]]]
