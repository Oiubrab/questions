/ Viz server: receives frames from the sim (q/modules/viz.q) and serves them to KX Dashboards.
/ Usage: q q/programs/viz_server.q -port 6814 < /dev/null &    then run the sim with --viz-port 6814
/ Replay a finished run (field and curves only; per-step energy is not in simulation_log.csv):
/   q q/programs/viz_server.q -port 6814 -replay results/seed42 -rate 50 < /dev/null &
/ Listens on localhost only: the port accepts arbitrary q, so never expose it.

\d .viz

ROWS: 6; COLS: 12; META_ROWS: 3; META_COLS: 5
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

onBar:{[bar;mouse;cat;rest]
  / A lower bar number means a new run: start from a clean slate
  if[(0<count bars)&(bar<last bars`bar); bars:: 0#bars; frames:: ()];
  bars:: (neg HIST_BARS) sublist bars, enlist `bar`mx`my`cx`cy`slice`energy`action`catches`align`pressure!
    (bar;mouse 0;mouse 1;cat 0;cat 1),rest}

/ ---- Dashboards queries ----

/ 72 rows: row 0 is the top (input) row
energyNow:{([] row:`int$raze COLS#'til ROWS; col:`int$(ROWS*COLS)#til COLS; energy:`int$raze br)}
metaNow:{([] row:`int$raze META_COLS#'til META_ROWS; col:`int$(META_ROWS*META_COLS)#til META_COLS; energy:`int$raze mbr)}

fieldNow:{
  if[0=count bars; :([] who:`symbol$(); x:`float$(); y:`float$())];
  r:last bars;
  ([] who:`mouse`cat; x:r`mx`cx; y:r`my`cy), gazeNow[r]}

/ Dots fanned across the cat's active vision slice (slice k spans angle k*45deg..(k+1)*45deg from +x); none if no slice is active
gazeNow:{[r]
  if[r[`slice]<0; :([] who:`symbol$(); x:`float$(); y:`float$())];
  ang:raze 2#enlist ((acos -1)%4f)*r[`slice]+(1+til 4)%5f;
  rad:raze (4#8f;4#12f);
  ([] who:(count ang)#`gaze; x:r[`cx]+rad*cos ang; y:r[`cy]+rad*sin ang)}

/ Last n bars of positions; the field is toroidal for the mouse, so the trail is drawn as points, not a line
trailNow:{[n]
  t:(neg n) sublist bars;
  ([] who:(count t)#`mouseTrail; x:t`mx; y:t`my), ([] who:(count t)#`catTrail; x:t`cx; y:t`cy)}

/ Rolling catch rate over 200 bars and mean move alignment over 100 bars (nulls ignored)
statsNow:{[n]
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
replayPos: 0
replay:{[dir;perSec]
  t:("IIIIIIIIIIFIFI"; enlist ",") 0: hsym `$dir,"/simulation_log.csv";
  replayRows:: t; replayPos:: 0; bars:: 0#bars;
  `.z.ts set {
    n:count replayRows; if[replayPos>=n; :()];
    r:replayRows replayPos;
    onBar[r`bar; `float$(r`mouseX;r`mouseY); `float$(r`catX;r`catY);
          (r`visionSlice; r`brainEnergy; r`outAction; r`catches; r`alignment; r`pressure)];
    replayPos+:1};
  system "t ",string `int$1000%perSec}

\d .

args: .Q.opt .z.x
port: first "J"$args`port
system "p 127.0.0.1:",string port
if[`replay in key args; .viz.replay[first args`replay; $[`rate in key args; "F"$first args`rate; 20f]]]
