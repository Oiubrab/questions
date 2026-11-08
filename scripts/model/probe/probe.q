/ CATCH_RATE_ANALYSIS.md §2 ledger: cat_mouse.q with three hooks in the bar loop that record each reward term per route.
/ The hooks use no RNG, so a probed run reproduces the plain run exactly. Build the hooked loop first:
/   scripts/model/probe/make_probe.sh      (writes results/model/probe/bar_loop_probe.q from the current bar_loop.q)
/ Run (repo root): q scripts/model/probe/probe.q --seed N --bars 10000 --output-dir DIR/ < /dev/null  -> DIR/probe.bin
/ Summarise: q scripts/model/probe/ledger.q PARENT < /dev/null   (PARENT/pr1 .. pr16); ctl.q: per-seed controls
/ The load list mirrors q/programs/cat_mouse.q; keep it in step if that changes.
\l q/modules/brain/trinary.q
\l q/modules/brain/directions.q
\l q/modules/brain/synapses.q
\l q/modules/brain/brain.q
\l q/modules/world/vision.q
\l q/modules/brain/brain_engine.q
\l q/modules/viz/viz.q
\l q/modules/world/relearn.q
\l q/modules/world/world.q
\l q/modules/brain/modular.q
\l q/programs/cat_mouse/config.q
\l q/programs/cat_mouse/params.q
\l q/programs/cat_mouse/state.q
\l scripts/model/probe/probe_wrap.q
\l results/model/probe/bar_loop_probe.q
\l q/programs/cat_mouse/outputs.q
(hsym `$outputDir,"probe.bin") set (.p.L;first .p.phase;.p.U;.p.C;.p.N;.p.B;.p.snaps;.p.flat prim`syns)
