/ Weight persistence and CLI behaviour (runs the simulation as a subprocess)
/ save -> --load-weights -> save (0 bars) must reproduce the saved state exactly.
/ Run from repo root: q q/tests/test_weights.q

\l q/tests/harness.q

-1 "test_weights";
dir: "results/_test_weights/";
/ "env q", not "q": system "q ..." can be taken as the q command \q and silently do nothing
run:{[a] system "env q q/programs/cat_mouse.q ",a," < /dev/null > /dev/null 2>&1"}

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

/ --cos-reward is accepted, recorded in summary.csv, and changes learning (same seed, same bars)
run "--seed 5 --bars 200 --cos-reward --output-dir ",dir,"cos";
SUM_TYPES:`seed`bars`directRewards`metaRewards`ablateMeta`noLearning`weightFile`catches`totalMoves`towards`away`perp`meanPressure`meanAlignment`acquisitionTime`cosReward`rewardScale!"IIBBBBSIIIIIFFIBF"
sumOf:{[d;c] f:hsym `$dir,d,"/summary.csv"; first ((SUM_TYPES `$"," vs first read0 f;enlist ",") 0: f) c}
.t.eq["summary records cosReward=0 by default";sumOf["a";`cosReward];0b];
.t.eq["summary records cosReward=1 with --cos-reward";sumOf["cos";`cosReward];1b];
.t.ok["--cos-reward changes primary synapses vs default";not (wa 0)~(get hsym `$dir,"cos/weights_seed5.bin") 0];

/ --reward-scale is recorded; 1 is the default; a non-positive value aborts
.t.eq["summary records rewardScale=1 by default";sumOf["a";`rewardScale];1f];
run "--seed 5 --bars 200 --reward-scale 0.25 --output-dir ",dir,"rs";
.t.eq["summary records --reward-scale 0.25";sumOf["rs";`rewardScale];0.25];
.t.ok["--reward-scale 0.25 changes primary synapses vs default";not (wa 0)~(get hsym `$dir,"rs/weights_seed5.bin") 0];
.t.ok["--reward-scale 0 exits non-zero";`failed~@[run;"--seed 5 --bars 1 --reward-scale 0 --output-dir ",dir,"rs0";{`failed}]];

/ A misspelt flag must abort, not silently run the control condition
.t.ok["unknown flag exits non-zero";`failed~@[run;"--seed 5 --bars 1 --output-dir ",dir,"c --ablate_meta";{`failed}]];

system "rm -rf ",dir;
.t.done[];
