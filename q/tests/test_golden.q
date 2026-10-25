/ Seeded 300-bar golden state: protects tier-1 performance work from behavior drift.
/ The digest is md5(-8! finalState) for seed 42 with the current sparse-learning code.
/ Run from repo root: q q/tests/test_golden.q

\l q/tests/harness.q

-1 "test_golden";
dir: "results/_test_golden/";
system "rm -rf ",dir;
run:{[a] system "env q q/programs/cat_mouse.q ",a," < /dev/null > /dev/null 2>&1"};
run "--seed 42 --bars 300 --output-dir ",dir;
state: get hsym `$dir,"weights_seed42.bin";
payloadPath: hsym `$dir,"state.serialized";
payloadPath 1: -8! state;
.t.ok["serialized state bytes round-trip";(read1 payloadPath)~(-8!state)];
md5Path: hsym `$dir,"state.md5";
md5Output: system "md5sum ",dir,"state.serialized";
.t.ok["md5sum produced output";count md5Output];
digest: 32#first md5Output;
.t.eq["seed 42 300-bar final state matches golden MD5";digest;"4e7d460ae07a59feb626e8dcdcf615f5"];
system "rm -rf ",dir;
.t.done[];
