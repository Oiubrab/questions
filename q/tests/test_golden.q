/ Seeded 300-bar golden state: protects tier-1 performance work from behavior drift.
/ The digest is md5(-8! finalState) for seed 42 with the current sparse-learning code.
/ Updated 2026-10-29: the meta-brain input became the satiety scale (starving left .. full right) and
/ its reward follows it (REVIVAL_PLAN.md §2 points 8-9); every default run changed by design.
/ Updated 2026-10-29 again: the revival plan's working configuration became the default (cosine reward,
/ reward scale 0.25, hebb 250, wrapped world, angle wiring). Verified first: a default 20k-bar run of
/ seed 1 is byte-identical (weights and simulation log) to the same run with those flags given explicitly.
/ Updated 2026-11-02: the modular brain became the default (sight, touch and reward brains feeding a
/ central brain; OBSTACLES_PLAN.md §7 decision 4). Verified first: --single-brain reproduces the
/ previous default byte for byte (seed 1, 20k bars: weights and simulation log).
/ Updated 2026-11-08: the catch-rate plan's main arm became the default (4 punish calls from bar 1, floor 100 with the
/ cap kept at 2,000,000, primary decay x0.5; plans/inProgress/CATCH_RATE_PLAN.md). Verified first: a default 20k-bar
/ run of seed 1 is byte-identical (weights and simulation log) to the same run with those flags given explicitly.
/ The legacy flags must still reproduce the previous default's digest, checked below.
/ Run from repo root: q q/tests/test_golden.q

\l q/tests/harness.q

-1 "test_golden";
dir: "results/_test_golden/";
system "rm -rf ",dir;
run:.t.run
run "--seed 42 --bars 300 --output-dir ",dir;
state: get hsym `$dir,"weights_seed42.bin";
payloadPath: hsym `$dir,"state.serialized";
payloadPath 1: -8! state;
.t.ok["serialized state bytes round-trip";(read1 payloadPath)~(-8!state)];
md5Path: hsym `$dir,"state.md5";
md5Output: system "md5sum ",dir,"state.serialized";
.t.ok["md5sum produced output";count md5Output];
digest: 32#first md5Output;
.t.eq["seed 42 300-bar final state matches golden MD5";digest;"5af7ecb8be45bee0854c0a3f5e987ec9"];
system "rm -rf ",dir;
/ The pre-2026-11-08 default, restored by flags
run "--seed 42 --bars 300 --punish-calls legacy --floor 25 --decay-mult 1 --output-dir ",dir;
payloadPath 1: -8! get hsym `$dir,"weights_seed42.bin";
.t.eq["legacy flags reproduce the previous default's golden MD5";32#first system "md5sum ",dir,"state.serialized";"3e7bda69bb94a1854b66b70a859b894c"];
system "rm -rf ",dir;
.t.done[];
