/ Sim-side publisher for the KX Dashboards viewer (see plans/inProgress/KX_DASHBOARDS_PLAN.md).
/ Off by default. Enabled by --viz-port; sends to q/programs/viz_server.q. Never touches the RNG or the learning path.

\d .viz

enabled: 0b
h: 0Ni
delayMs: 0

/ Connect to the viz server. A missing server leaves publishing off rather than stopping the run.
init:{[port;delay]
  h:: @[hopen; `$"::",string port; {-2 "viz: cannot connect (",x,"), continuing without"; 0Ni}];
  enabled:: not null h;
  delayMs:: delay}

/ Async send, flushed at once because the sim never goes idle. A dead viewer disables publishing.
send:{[msg]
  if[enabled; @[{neg[h] x; neg[h][]}; msg; {-2 "viz: lost connection, publishing off"; enabled:: 0b}]]}

/ Energy matrices after one propagation cycle. delayMs slows the sim down so the flow is watchable.
pubStep:{[bar;step;br;mbr]
  send (`.viz.onStep; bar; step; br; mbr);
  if[delayMs>0; system "sleep ",string delayMs%1000]}

/ rest: (slice;energy;action;catches;align;pressure). Grouped because q lambdas take at most 8 arguments.
pubBar:{[bar;mouse;cat;rest]
  send (`.viz.onBar; bar; mouse; cat; rest)}

close:{if[enabled; @[hclose; h; ::]; enabled:: 0b]}

\d .
