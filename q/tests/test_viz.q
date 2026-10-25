/ Viz publisher and server: sim output identical with viz off/on, dead viewer tolerated, server state and queries.
/ Run from repo root: q q/tests/test_viz.q

\l q/tests/harness.q

-1 "test_viz";
dir: "results/_test_viz/";
PORT: 6871
run:{[a] system "env q q/programs/cat_mouse.q ",a," < /dev/null > /dev/null 2>&1"}
logOf:{[d] read0 hsym `$d,"simulation_log.csv"}

/ Server in the background; sim publishes to it
system "env q q/programs/viz_server.q -port ",(string PORT)," < /dev/null > /dev/null 2>&1 &";
system "sleep 1";
h: hopen `$"::",string PORT;

run "--seed 5 --bars 60 --output-dir ",dir,"off";
run "--seed 5 --bars 60 --viz-port ",(string PORT)," --output-dir ",dir,"on";
run "--seed 5 --bars 60 --viz-port 6899 --output-dir ",dir,"dead";   / nothing listens there

.t.ok["viz on leaves simulation_log.csv identical";(logOf dir,"off/")~logOf dir,"on/"];
.t.ok["dead viz port does not change the run";(logOf dir,"off/")~logOf dir,"dead/"];
.t.ok["weights identical with viz on";(get hsym `$dir,"off/weights_seed5.bin")~get hsym `$dir,"on/weights_seed5.bin"];

st: h"`.viz.status[]";
.t.eq["server saw the last bar";st`bar;60i];
.t.eq["server kept every bar";st`bars;60];
.t.eq["server saw the last step of the bar";st`step;11i];
.t.eq["energy matrix has 72 cells";count h"`.viz.energyNow[]";72];
.t.eq["meta matrix has 15 cells";count h"`.viz.metaNow[]";15];
.t.eq["field has mouse, cat and gaze dots";(h"`.viz.fieldNow[]")`who;`mouse`cat,8#`gaze];
.t.ok["positions stay inside the field";all (h"`.viz.fieldNow[]")[`x] within 0 100f];
.t.eq["trail has both animals";count h"`.viz.trailNow[10]";20];
.t.eq["stats covers the requested bars";count h"`.viz.statsNow[30]";30];
kp: h".viz.kpiNow[]";
.t.eq["kpi grid has 8 headline rows";count kp;8];
.t.eq["kpi shows total catches as a string";first exec val from kp where stat=`Catches;string last (h".viz.bars")`catches];

/ A new run restarts the bar counter: the server must reset, not append
run "--seed 6 --bars 20 --viz-port ",(string PORT)," --output-dir ",dir,"second";
.t.eq["new run resets server history";(h"`.viz.status[]")`bars;20];

/ Gaze dots fan across the active vision slice around the cat (slice 0 = angles 0..45deg from +x)
h".viz.onBar[1i;50 50f;60 50f;(0i;0i;0i;0i;0n;0f)]";
fz: h".viz.fieldNow[]";
gz: select from fz where who=`gaze;
ang: atan (gz[`y]-50f)%gz[`x]-60f;
.t.eq["active slice adds 8 gaze dots";count gz;8];
.t.ok["gaze dots lie inside the active slice";all ang within 0 0.7853982];
h".viz.onBar[2i;50 50f;60 50f;(-1i;0i;0i;0i;0n;0f)]";
.t.eq["no active slice means no gaze dots";count h".viz.fieldNow[]";2];

/ Ring buffer caps the number of kept frames
h"`.viz.HIST_FRAMES set 5; .viz.onStep[1;0;.viz.br;.viz.mbr]";
h"do[10; .viz.onStep[1;0;.viz.br;.viz.mbr]]";
.t.eq["frame ring buffer is capped";(h"`.viz.status[]")`frames;5];

/ Replay of a finished run drives the same tables
h"`.viz.bars set 0#.viz.bars; .viz.replay[\"",dir,"off\";1000f]";
system "sleep 0.5";
.t.eq["replay fills bars";(h"`.viz.status[]")`bars;60];

hclose h;
@[system; "pkill -f 'viz_server.q -port ",(string PORT),"'"; ::];
.t.done[]
