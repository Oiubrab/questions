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
wa: get hsym `$dir,"a/weights_seed5.bin";
wb: get hsym `$dir,"b/weights_seed5.bin";
.t.eq["weights file has 10 components";count wa;10];
.t.ok["round trip reproduces saved state";wa~wb];
.t.ok["summary.csv written";`summary.csv in key hsym `$dir,"a"];
.t.ok["epoch_summary.csv written";`epoch_summary.csv in key hsym `$dir,"a"];

/ A misspelt flag must abort, not silently run the control condition
.t.ok["unknown flag exits non-zero";`failed~@[run;"--seed 5 --bars 1 --output-dir ",dir,"c --ablate_meta";{`failed}]];

system "rm -rf ",dir;
.t.done[];
