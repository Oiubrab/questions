/ V0 spike for KX Dashboards: fake 6x12 energy wave + cat/mouse tables, no sim involved.
/ Run: q q/programs/viz_spike.q -p 6814 < /dev/null &   then add a q connection to localhost:6814 in Dashboards.
/ Poll energyNow[] / fieldNow[], or subscribe to the energyStream table.

ROWS:6; COLS:12
frame:0

/ wavefront enters the top row (cols 2-9) and sweeps to the bottom, one row per 3 frames
energyAt:{[f]
  r:raze COLS#'til ROWS; c:(ROWS*COLS)#til COLS;
  head:(f div 3) mod ROWS+2;
  e:?[(r=head)&c within 2 9; 2i; ?[(r=head-1)&c within 2 9; 1i; 0i]];
  ([] row:`int$r; col:`int$c; energy:e)}

energyNow:{energyAt frame}
fieldNow:{
  t:frame%20f;
  ([] who:`mouse`cat; x:50f+40 30f*cos t-0 0.6; y:50f+40 30f*sin t-0 0.6)}

energyStream:update time:`timestamp$() from 0#energyAt 0

/ minimal pub/sub with the shape the Dashboards gateway calls: .u.sub[table;syms] returns (table;snapshot)
.u.w:()!()
.u.sub:{[t;s] .u.w[t],:.z.w; (t; $[t=`energyStream; update time:.z.p from energyAt frame; ()])}
.u.snap:{[x] $[x[0]=`energyStream; update time:.z.p from energyAt frame; ()]}
.z.pc:{.u.w:.u.w except\: x}

.z.ts:{
  frame+:1;
  d:update time:.z.p from energyAt frame;
  {(neg x)(`upd;`energyStream;y)}[;d] each raze .u.w`energyStream}
\t 100
