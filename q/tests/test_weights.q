/ Weight persistence and CLI behaviour (runs the simulation as a subprocess)
/ save -> --load-weights -> save (0 bars) must reproduce the saved state exactly.
/ Run from repo root: q q/tests/test_weights.q

\l q/tests/harness.q
\l q/load.q
\l q/modules/world/world.q

-1 "test_weights";
dir: "results/_test_weights/";
/ "env q", not "q": system "q ..." can be taken as the q command \q and silently do nothing
run:.t.run

run "--seed 5 --bars 200 --output-dir ",dir,"a";
run "--seed 5 --bars 0 --load-weights ",dir,"a/weights_seed5.bin --output-dir ",dir,"b";
run "--seed 5 --bars 0 --output-dir ",dir,"init";
run "--seed 5 --bars 200 --no-learning --output-dir ",dir,"frozen";
wa: get hsym `$dir,"a/weights_seed5.bin";
wb: get hsym `$dir,"b/weights_seed5.bin";
wi: get hsym `$dir,"init/weights_seed5.bin";
wf: get hsym `$dir,"frozen/weights_seed5.bin";
.t.eq["weights file has 10 components";count wa;10];
.t.ok["round trip reproduces saved state";wa~wb];
.t.ok["no-learning preserves primary synapses";(wi 0)~(wf 0)];
.t.ok["no-learning preserves meta synapses";(wi 1)~(wf 1)];
.t.ok["normal run changes primary synapses";not (wi 0)~(wa 0)];
.t.ok["summary.csv written";`summary.csv in key hsym `$dir,"a"];
.t.ok["epoch_summary.csv written";`epoch_summary.csv in key hsym `$dir,"a"];

/ Defaults since 2026-10-29: cosine reward, reward scale 0.25, hebb 250, wrapped world, angle wiring.
/ Each restore flag is recorded and changes learning (same seed, same bars).
run "--seed 5 --bars 200 --raw-dot-reward --output-dir ",dir,"cos";
SUM_TYPES:`seed`bars`directRewards`metaRewards`ablateMeta`noLearning`weightFile`catches`totalMoves`towards`away`perp`meanPressure`meanAlignment`acquisitionTime`cosReward`rewardScale`pgUpdate`pgEta`rpe`rpeRate`hebb`relearnSchedule`wrapWorld`weightRatio`wiring`wiringPerm`world`blink`policy`hiddenReward`seeThrough`visibleFrac`alignVisible`alignHidden`blockedFrac`catchTimeMed`efference`spontaneous`meanSatietyCell`randomTie`mouseStep`fieldSize`twoTimescale`tauFast`tauSlow`architecture`centralRows`touch`metaEntry`mice`evasive`distractor`dogContacts`layoutEvery`satEdges`punishCalls`strategicGate`floor`decayMult`stepsPerBar!"IIBBBBSIIIIIFFIBFBFBFISBFSSSSSSBFFFFFBFFBFFBFFSIBSIBBIISIIIFI"
logOf:{[d] read0 hsym `$dir,d,"/simulation_log.csv"}
sumOf:{[d;c] f:hsym `$dir,d,"/summary.csv"; first ((SUM_TYPES `$"," vs first read0 f;enlist ",") 0: f) c}
.t.eq["summary records cosReward=1 by default";sumOf["a";`cosReward];1b];
.t.eq["summary records cosReward=0 with --raw-dot-reward";sumOf["cos";`cosReward];0b];
.t.ok["--raw-dot-reward changes primary synapses vs default";not (wa 0)~(get hsym `$dir,"cos/weights_seed5.bin") 0];
.t.ok["--cos-reward with --raw-dot-reward exits non-zero";`failed~@[run;"--seed 5 --bars 1 --cos-reward --raw-dot-reward --output-dir ",dir,"cc";{`failed}]];

/ --reward-scale is recorded; 1 is the default; a non-positive value aborts
.t.eq["summary records rewardScale=0.25 by default";sumOf["a";`rewardScale];0.25];
run "--seed 5 --bars 200 --reward-scale 1 --output-dir ",dir,"rs";
.t.eq["summary records --reward-scale 1";sumOf["rs";`rewardScale];1f];
.t.ok["--reward-scale 1 changes primary synapses vs default";not (wa 0)~(get hsym `$dir,"rs/weights_seed5.bin") 0];
.t.ok["--reward-scale 0 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --reward-scale 0 --output-dir ",dir,"rs0";{`failed}]];

/ --pg-update / --rpe (G.R1/G.R2) are recorded, change learning, and validate their arguments
run "--seed 5 --bars 200 --pg-update --pg-eta 0.2 --rpe --rpe-rate 0.05 --output-dir ",dir,"pg";
.t.eq["summary records pgUpdate=0 by default";sumOf["a";`pgUpdate];0b];
.t.eq["summary records --pg-update";sumOf["pg";`pgUpdate];1b];
.t.eq["summary records --pg-eta";sumOf["pg";`pgEta];0.2];
.t.eq["summary records --rpe and its rate";(sumOf["pg";`rpe];sumOf["pg";`rpeRate]);(1b;0.05)];
.t.ok["--pg-update changes primary synapses vs default";not (wa 0)~(get hsym `$dir,"pg/weights_seed5.bin") 0];
.t.ok["--rpe without --pg-update exits non-zero";`failed~@[run;"--seed 5 --bars 1 --rpe --output-dir ",dir,"rpe0";{`failed}]];

/ --hebb (G.R3) is recorded and changes learning; it can't be zero (unconditional reinforcement stays)
run "--seed 5 --bars 200 --hebb 1000 --output-dir ",dir,"hebb";
.t.eq["summary records the derived hebb 252 by default (Phase R)";sumOf["a";`hebb];252i];
.t.eq["summary records --hebb 1000";sumOf["hebb";`hebb];1000i];
.t.ok["--hebb 1000 changes primary synapses vs default";not (wa 0)~(get hsym `$dir,"hebb/weights_seed5.bin") 0];
.t.ok["--hebb 0 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --hebb 0 --output-dir ",dir,"h0";{`failed}]];

/ --relearn-schedule (G.4) sets the run length, writes its outputs, and validates the schedule
run "--seed 5 --relearn-schedule A:150,L180:100 --output-dir ",dir,"rl";
.t.eq["schedule sets the run length";sumOf["rl";`bars];250i];
.t.ok["switches.csv and relearn_log.csv written";all `switches.csv`relearn_log.csv in key hsym `$dir,"rl"];
.t.eq["switches.csv has one row per block";count read0 hsym `$dir,"rl/switches.csv";3];
.t.eq["relearn_log.csv has one row per bar";count read0 hsym `$dir,"rl/relearn_log.csv";251];
.t.ok["unknown world exits non-zero";`failed~@[run;"--seed 5 --relearn-schedule A:10,XYZ:10 --output-dir ",dir,"rlb";{`failed}]];
.t.ok["--bars disagreeing with the schedule exits non-zero";`failed~@[run;"--seed 5 --bars 99 --relearn-schedule A:10 --output-dir ",dir,"rlc";{`failed}]];

/ --wrap-world: recorded, cat stays inside the field and actually crosses an edge
run "--seed 5 --bars 2000 --wrap-world --output-dir ",dir,"wrap";
.t.eq["summary records wrapWorld=1 by default";sumOf["a";`wrapWorld];1b];
run "--seed 5 --bars 300 --walled-world --output-dir ",dir,"walled";
.t.eq["summary records --walled-world";sumOf["walled";`wrapWorld];0b];
.t.ok["--walled-world keeps the cat in the box";all (raze (("IIIII";enlist ",") 0: hsym `$dir,"walled/simulation_log.csv")`catX`catY) within 0 100];
.t.eq["summary records --wrap-world";sumOf["wrap";`wrapWorld];1b];
wl: ("IIIII";enlist ",") 0: hsym `$dir,"wrap/simulation_log.csv";
.t.ok["wrapped cat stays within the field";all (raze wl`catX`catY) within 0 100];
.t.ok["wrapped cat crosses an edge at least once";any 50<abs 1_deltas wl`catX];

/ --weight-ratio (G.R4): recorded, caps every weight at ratio x floor (floor 100 by default since 2026-11-08), rejects ratio x floor below 1000
run "--seed 5 --bars 300 --weight-ratio 100 --output-dir ",dir,"wr";
.t.eq["summary records weightRatio 20000 by default (cap 2,000,000 / floor 100)";sumOf["a";`weightRatio];20000f];
.t.eq["summary records --weight-ratio 100";sumOf["wr";`weightRatio];100f];
wwr: get hsym `$dir,"wr/weights_seed5.bin";
.t.ok["--weight-ratio 100 keeps primary weights within [100, 10000] (meta: [25 floor at init, 10000])";(all (raze raze/[wwr 0]) within 100 10000)&all (raze raze/[wwr 1]) within 25 10000];
.t.ok["--weight-ratio 9 (cap 900 at floor 100) exits non-zero";`failed~@[run;"--seed 5 --bars 1 --weight-ratio 9 --output-dir ",dir,"wr0";{`failed}]];
.t.ok["--floor 25 --weight-ratio 39 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --floor 25 --weight-ratio 39 --output-dir ",dir,"wr0";{`failed}]];

/ --wiring (§7 decision 6): recorded; random is a per-seed permutation that leaves the initial weights
/ paired with the grid run; angle changes behaviour; an unknown wiring aborts
run "--seed 5 --bars 0 --wiring random --output-dir ",dir,"wrnd";
run "--seed 5 --bars 200 --wiring grid --output-dir ",dir,"wang";
.t.eq["summary records angle wiring (in angle order) by default";(sumOf["a";`wiring];sumOf["a";`wiringPerm]);(`angle;`47653012)];
.t.eq["summary records grid wiring";(sumOf["wang";`wiring];sumOf["wang";`wiringPerm]);(`grid;`01234567)];
.t.ok["random wiring is a permutation of the 8 slots";(til 8)~asc -48+"i"$string sumOf["wrnd";`wiringPerm]];
.t.ok["random wiring leaves the initial weights paired with grid";wi~get hsym `$dir,"wrnd/weights_seed5.bin"];
.t.ok["grid wiring changes learning vs angle";not (wa 0)~(get hsym `$dir,"wang/weights_seed5.bin") 0];
.t.ok["--wiring xyz exits non-zero";`failed~@[run;"--seed 5 --bars 1 --wiring xyz --output-dir ",dir,"wx";{`failed}]];

/ Phase W: worlds, blinking vision and scripted baselines
WL:{[d] ("IBIBBFB";enlist ",") 0: hsym `$dir,d,"/world_log.csv"}
run "--seed 5 --bars 400 --wrap-world --blink 20:30 --output-dir ",dir,"blink";
bl: WL "blink";
.t.eq["blink: visible exactly 20 of every 50 bars";sumOf["blink";`visibleFrac];0.4];
.t.ok["blink: hidden bars give the brain no vision input";all -1i=((("IIIIII";enlist ",") 0: hsym `$dir,"blink/simulation_log.csv")`visionSlice) where not bl`visible];
.t.eq["blink: sinceSeen counts the hidden bars";bl[`sinceSeen] 20+til 31;`int$(1+til 30),0];
.t.eq["no W flag: no world_log.csv";`world_log.csv in key hsym `$dir,"a";0b];
.t.eq["summary records the default world, policy and hidden reward";(sumOf["a";`world];sumOf["a";`policy];sumOf["a";`hiddenReward]);`none`brain`none];
run "--seed 5 --bars 400 --wrap-world --policy greedy --output-dir ",dir,"greedy";
run "--seed 5 --bars 400 --wrap-world --policy random --output-dir ",dir,"rand";
.t.ok["greedy in the open world heads at the mouse (alignment > 0.9)";0.9<sumOf["greedy";`meanAlignment]];
.t.ok["random policy is near zero alignment";0.15>abs sumOf["rand";`meanAlignment]];
.t.ok["scripted policies don't learn";(wi 0)~(get hsym `$dir,"greedy/weights_seed5.bin") 0];
.t.eq["summary records noLearning for a scripted policy";sumOf["greedy";`noLearning];1b];
run "--seed 5 --bars 400 --wrap-world --world rooms --output-dir ",dir,"rooms";
rl: ("IIIII";enlist ",") 0: hsym `$dir,"rooms/simulation_log.csv";
R: .world.tile[.world.LAYOUTS`rooms;100f];
.t.ok["rooms: the cat is never inside a wall (1-unit rounding allowance)";not any {.world.inWall[R;x;-1f]} each flip `float$rl 3 4];
.t.ok["rooms: the mouse is never inside a wall";not any {.world.inWall[R;x;-1f]} each flip `float$rl 1 2];
.t.ok["rooms: world_log.csv written";`world_log.csv in key hsym `$dir,"rooms"];
.t.ok["--world with --walled-world exits non-zero";`failed~@[run;"--seed 5 --bars 1 --world pillars --walled-world --output-dir ",dir,"w0";{`failed}]];
.t.ok["--blink 20 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --blink 20 --output-dir ",dir,"w1";{`failed}]];
.t.ok["--policy xyz exits non-zero";`failed~@[run;"--seed 5 --bars 1 --policy xyz --output-dir ",dir,"w2";{`failed}]];
.t.ok["--policy with --relearn-schedule exits non-zero";`failed~@[run;"--seed 5 --policy greedy --relearn-schedule A:10 --output-dir ",dir,"w3";{`failed}]];

/ W.1b internal input: recorded, changes the run, adds energy; bad values abort
run "--seed 5 --bars 300 --efference --output-dir ",dir,"intl";
.t.eq["summary records the efference default";sumOf["a";`efference];0b];
.t.eq["summary records --efference";sumOf["intl";`efference];1b];
.t.ok["summary records the mean satiety cell (0 starving .. 4 full)";sumOf["a";`meanSatietyCell] within 0 4];
.t.ok["internal input changes learning vs default";not (wa 0)~(get hsym `$dir,"intl/weights_seed5.bin") 0];
/ --spontaneous was removed (2026-11-03): an old command line must fail loudly, not run without it
.t.ok["--spontaneous (removed) exits non-zero";`failed~@[run;"--seed 5 --bars 1 --spontaneous 0.05 --output-dir ",dir,"sp2";{`failed}]];

/ G.3(c) random tie-break, G.5 mouse step and field size: recorded, change the run, validated
run "--seed 5 --bars 300 --random-tie --output-dir ",dir,"tie";
run "--seed 5 --bars 300 --mouse-step 4 --field-size 140 --output-dir ",dir,"g5";
.t.eq["summary records tie, mouse step and field size defaults";(sumOf["a";`randomTie];sumOf["a";`mouseStep];sumOf["a";`fieldSize]);(0b;2f;100f)];
.t.eq["summary records --random-tie --mouse-step 4 --field-size 140";(sumOf["tie";`randomTie];sumOf["g5";`mouseStep];sumOf["g5";`fieldSize]);(1b;4f;140f)];
.t.ok["--random-tie changes the run";not (logOf "a")~logOf "tie"];
g5: ("IIIII";enlist ",") 0: hsym `$dir,"g5/simulation_log.csv";
.t.ok["--field-size 140: positions stay in the field and use it";(all (raze g5`mouseX`mouseY`catX`catY) within 0 140)&any (raze g5`catX`catY)>100];
.t.ok["--field-size 10 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --field-size 10 --output-dir ",dir,"fs";{`failed}]];

/ G.R5 two-timescale weights: an 11th component (the slow memory) is saved and restored
run "--seed 5 --bars 300 --two-timescale --tau-fast 20 --tau-slow 50 --output-dir ",dir,"tts";
wt: get hsym `$dir,"tts/weights_seed5.bin";
.t.eq["--two-timescale saves 11 components";count wt;11];
.t.eq["slow memory has one value per primary synapse (9 stacked rows)";count wt 10;9*12*64];
.t.eq["summary records two-timescale settings";(sumOf["tts";`twoTimescale];sumOf["tts";`tauFast];sumOf["tts";`tauSlow]);(1b;20f;50f)];
.t.ok["--two-timescale changes learning vs default";not (get hsym `$dir,"a/weights_seed5.bin")~10#wt];
run "--seed 5 --bars 0 --two-timescale --load-weights ",dir,"tts/weights_seed5.bin --output-dir ",dir,"tts2";
.t.ok["11-component round trip reproduces the state, slow memory included";wt~get hsym `$dir,"tts2/weights_seed5.bin"];
run "--seed 5 --bars 0 --load-weights ",dir,"tts/weights_seed5.bin --output-dir ",dir,"tts3";
.t.eq["an 11-component file loads without --two-timescale (memory ignored)";count get hsym `$dir,"tts3/weights_seed5.bin";10];
.t.ok["--tau-slow without --two-timescale exits non-zero";`failed~@[run;"--seed 5 --bars 1 --tau-slow 50 --output-dir ",dir,"ts0";{`failed}]];

/ Modular brain (default since 2026-11-02): sight 3 + touch 3 + central rows, stacked in the weights file
.t.eq["summary records the modular default";(sumOf["a";`architecture];sumOf["a";`centralRows];sumOf["a";`touch];sumOf["a";`metaEntry]);(`modular;3i;1b;`right)];
.t.eq["default weights stack 3+3+3 = 9 rows of 12 cells";count each (wa 0;wa 2;first wa 0);9 9 12];
run "--seed 5 --bars 100 --single-brain --output-dir ",dir,"single";
run "--seed 5 --bars 100 --central-rows 6 --output-dir ",dir,"c6";
.t.eq["--single-brain: one 6-row brain, recorded";(count (get hsym `$dir,"single/weights_seed5.bin") 0;sumOf["single";`architecture];sumOf["single";`touch]);(6;`single;0b)];
.t.eq["--central-rows 6: 12 stacked rows";count (get hsym `$dir,"c6/weights_seed5.bin") 0;12];
run "--seed 5 --bars 400 --world rooms --output-dir ",dir,"touchOn";
run "--seed 5 --bars 400 --world rooms --no-touch --output-dir ",dir,"touchOff";
wOn: (get hsym `$dir,"touchOn/weights_seed5.bin") 0; wOff: (get hsym `$dir,"touchOff/weights_seed5.bin") 0;
.t.ok["touch brain learns when it touches walls (its rows change from --no-touch)";not (3#3_wOn)~3#3_wOff];
.t.eq["--no-touch recorded";sumOf["touchOff";`touch];0b];
run "--seed 5 --bars 200 --meta-entry top --output-dir ",dir,"mtop";
run "--seed 5 --bars 200 --meta-entry none --output-dir ",dir,"mnone";
.t.eq["--meta-entry recorded";(sumOf["mtop";`metaEntry];sumOf["mnone";`metaEntry]);`top`none];
.t.ok["--meta-entry top and none change the run";(not (logOf "a")~logOf "mtop")&not (logOf "a")~logOf "mnone"];
.t.ok["--meta-entry xyz exits non-zero";`failed~@[run;"--seed 5 --bars 1 --meta-entry xyz --output-dir ",dir,"me0";{`failed}]];
.t.ok["--central-rows with --single-brain exits non-zero";`failed~@[run;"--seed 5 --bars 1 --single-brain --central-rows 5 --output-dir ",dir,"me1";{`failed}]];
/ Planner baseline (W.0): shortest path around walls; in rooms it beats the straight-line greedy cat
run "--seed 5 --bars 400 --world rooms --policy planner --output-dir ",dir,"plan";
run "--seed 5 --bars 400 --world rooms --policy greedy --output-dir ",dir,"greedyR";
.t.ok["planner catches more than greedy in rooms";sumOf["plan";`catches]>sumOf["greedyR";`catches]];

/ W.5 harder goals, W.6 layout redraws, I.2 state log, I.4 lesions
run "--seed 5 --bars 400 --mice 3 --evasive --output-dir ",dir,"mice";
run "--seed 5 --bars 400 --distractor --output-dir ",dir,"dog";
.t.eq["summary records mice, evasive, distractor defaults";(sumOf["a";`mice];sumOf["a";`evasive];sumOf["a";`distractor]);(1i;0b;0b)];
.t.eq["summary records --mice 3 --evasive";(sumOf["mice";`mice];sumOf["mice";`evasive]);(3i;1b)];
.t.ok["--distractor: the dog is met and counted";(sumOf["dog";`distractor])&0<sumOf["dog";`dogContacts]];
.t.ok["--mice 3 --evasive and --distractor change the run";(not (logOf "a")~logOf "mice")&not (logOf "a")~logOf "dog"];
/ A single evasive mouse is respawned away from the cat after a catch, not caught again at once (bug fixed
/ 2026-11-03: greedy "caught" it ~985 times per 1,000 bars). Greedy catches about 146 per 1,000 bars
/ without --evasive; a fleeing mouse can't be easier to catch than that.
run "--seed 5 --bars 400 --evasive --policy greedy --output-dir ",dir,"ev1";
run "--seed 5 --bars 400 --policy greedy --output-dir ",dir,"ev0";
.t.ok["--evasive with one mouse: no faster catching than a mouse that doesn't flee";sumOf["ev1";`catches]<=sumOf["ev0";`catches]];
run "--seed 5 --bars 300 --blink 20:20 --log-state --output-dir ",dir,"st";
sl: get hsym `$dir,"st/state_log.bin";
.t.eq["--log-state: one brain state (9 rows x 12) per bar";(count sl`br;count first sl`br);300 108];
.t.ok["--log-state: last-seen slice is held through hidden bars";all (sl[`lastSeen] where not sl`visible) within 0 7];
run "--seed 5 --bars 50 --no-learning --lesion 7:5 --load-weights ",dir,"a/weights_seed5.bin --output-dir ",dir,"les";
wl: (get hsym `$dir,"les/weights_seed5.bin") 0;
.t.eq["--lesion 7:5: every route into the cell is zero";{[w;d] w[7-.dirs.dr d;5-.dirs.dc d;;d]}[wl] each 1 6;2#enlist 8#0i];
.t.ok["--lesion 7:5: routes into the cell from the brain above (sight/touch) are untouched";all 0<raze wl[5;4 5 6;;]];
.t.ok["--lesion without --no-learning exits non-zero";`failed~@[run;"--seed 5 --bars 1 --lesion 7:5 --output-dir ",dir,"les0";{`failed}]];
run "--seed 5 --bars 600 --world random --layout-every 200 --output-dir ",dir,"lay";
.t.eq["--layout-every recorded";sumOf["lay";`layoutEvery];200i];
.t.ok["--layout-every without --world random exits non-zero";`failed~@[run;"--seed 5 --bars 1 --layout-every 200 --output-dir ",dir,"lay0";{`failed}]];

run "--seed 5 --bars 300 --sat-edges 1,4,10,20 --output-dir ",dir,"sat";
.t.eq["summary records satiety edges (default and --sat-edges)";(sumOf["a";`satEdges];sumOf["sat";`satEdges]);`$("0.5|2|5|10";"1|4|10|20")];
.t.ok["--sat-edges 3,2,1,4 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --sat-edges 3,2,1,4 --output-dir ",dir,"sat0";{`failed}]];

/ Catch-rate plan flags (plans/inProgress/CATCH_RATE_PLAN.md): recorded, change the run, keep their bounds, validated.
/ Their defaults reproducing the original run exactly is test_golden.q's job.
.t.eq["summary records the catch-rate defaults (4 calls, gate 3, floor 100, decay x0.5)";sumOf["a"] each `punishCalls`strategicGate`floor`decayMult;(4i;3i;100i;0.5)];
run "--seed 5 --bars 300 --punish-calls legacy --floor 25 --decay-mult 1 --output-dir ",dir,"legacy";
.t.eq["legacy flags recorded (original punish rule, floor 25, decay x1), cap kept at 2,000,000";sumOf["legacy"] each `punishCalls`floor`decayMult`weightRatio;(0Ni;25i;1f;80000f)];
.t.ok["legacy flags change learning vs the new default";not (wa 0)~(get hsym `$dir,"legacy/weights_seed5.bin") 0];
run "--seed 5 --bars 300 --punish-calls 4 --output-dir ",dir,"p4";
.t.ok["--punish-calls 4 changes learning (neutral moves punished from bar 1)";not (wa 0)~(get hsym `$dir,"p4/weights_seed5.bin") 0];
run "--seed 5 --bars 300 --punish-calls 4 --strategic-gate 20 --floor 100 --weight-ratio 20000 --decay-mult 0.5 --output-dir ",dir,"cr";
.t.eq["summary records --punish-calls, --strategic-gate, --floor, --decay-mult";sumOf["cr"] each `punishCalls`strategicGate`floor`decayMult;(4i;20i;100i;0.5)];
wcr: get hsym `$dir,"cr/weights_seed5.bin";
.t.ok["--floor 100 --weight-ratio 20000 keeps primary weights within [100, 2,000,000]";all (raze raze/[wcr 0]) within 100 2000000];
.t.ok["--floor 100 lifts the floor (some primary weights sit at 100)";any 100=raze raze/[wcr 0]];
run "--seed 5 --bars 0 --floor 100 --output-dir ",dir,"fl0";
.t.ok["--floor leaves the initial weights unchanged (25-1000)";wi~get hsym `$dir,"fl0/weights_seed5.bin"];
run "--seed 5 --bars 300 --strategic-gate 1000 --output-dir ",dir,"sg";
.t.ok["--strategic-gate 1000 (strategic never on) changes learning";not (wa 0)~(get hsym `$dir,"sg/weights_seed5.bin") 0];
run "--seed 5 --bars 300 --decay-mult 0.5 --output-dir ",dir,"dm";
.t.ok["--decay-mult 0.5 changes learning";not (wa 0)~(get hsym `$dir,"dm/weights_seed5.bin") 0];
bad:("--punish-calls 0";"--punish-calls x";"--strategic-gate 0";"--floor 0";"--floor 1000";"--floor 100 --weight-ratio 5";"--decay-mult -1";"--decay-mult 51";"--decay-mult 0.5 --two-timescale");
{[b] .t.ok[b," exits non-zero";`failed~@[run;"--seed 5 --bars 1 ",b," --output-dir ",dir,"crbad";{`failed}]]} each bad;

/ Phase R: constants derived from the brain's configuration (q/tests/test_scaling.q checks the rules themselves)
.t.eq["today's brain derives 12 steps, 4 calls, hebb 252, decay x0.5";sumOf["a"] each `stepsPerBar`punishCalls`hebb`decayMult;(12i;4i;252i;0.5)];
run "--seed 5 --bars 50 --central-rows 6 --output-dir ",dir,"c6";
.t.eq["central rows 6 derives 21 steps, 3 calls, hebb 332";sumOf["c6"] each `stepsPerBar`punishCalls`hebb;(21i;3i;332i)];
run "--seed 5 --bars 50 --central-rows 6 --steps-per-bar 12 --punish-calls 4 --hebb 250 --output-dir ",dir,"c6f";
.t.eq["explicit flags override every derived constant";sumOf["c6f"] each `stepsPerBar`punishCalls`hebb;(12i;4i;250i)];
.t.eq["legacy punishment keeps hebb 250";sumOf["legacy";`hebb];250i];
run "--seed 5 --bars 50 --separation-margin 2 --output-dir ",dir,"sm2";
.t.eq["--separation-margin 2 needs 7 calls (7 x 0.2097 >= 2 x 0.733)";sumOf["sm2";`punishCalls];7i];
bad2:("--steps-per-bar 0";"--separation-margin 0";"--hebb-margin -1";"--catch-ref -0.1");
{[b] .t.ok[b," exits non-zero";`failed~@[run;"--seed 5 --bars 1 ",b," --output-dir ",dir,"prbad";{`failed}]]} each bad2;

/ A misspelt flag must abort, not silently run the control condition
.t.ok["unknown flag exits non-zero";`failed~@[run;"--seed 5 --bars 1 --output-dir ",dir,"c --ablate_meta";{`failed}]];

system "rm -rf ",dir;
.t.done[];
