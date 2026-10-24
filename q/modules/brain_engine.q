/ Brain engine module
/ Orchestrates a complete brain system (neurons + synapses + I/O).
/ Provides run_cycle and run_cycle_simple for primary and meta-brain respectively.
/ Mirrors brain_engine_module.f90

\d .engine

/ Initialize a complete brain system. Returns a dictionary with all components.
/ Keys: br,syns,su,inc0,inc1,out,inp,rows,cols,inOff,inLen,outOff,outLen
initSystem:{[nR;nC;inLen;outLen;inOff;outOff]
  brState: .brain.initBrain[nR;nC];
  `br`syns`su`inc0`inc1`out`inp`rows`cols`inOff`inLen`outOff`outLen!
    (brState 0;
     .syn.init[nR;nC];
     .syn.initSU[nR;nC];
     brState 1;
     brState 2;
     `int$outLen#0i;
     `int$inLen#0i;
     nR;nC;inOff;inLen;outOff;outLen)}

/ Run one propagation cycle:
/  1. Copy inputter to brain top row
/  2. Clear outputter
/  3. Propagate one brain step
/  4. Clear inputter (consumed on first call per bar)
/ Returns updated system dictionary.
/ Mirrors run_brain_cycle in brain_engine_module.f90
runCycle:{[sys]
  br:sys`br; inc0:sys`inc0;
  / Step 1: inject inputter into top row
  r0: .brain.copyInputToTopRow[sys`inp;sys`inOff;br;inc0];
  br:r0 0; inc0:r0 1;
  / Step 2: clear outputter
  out: `int$(sys`outLen)#0i;
  / Step 3: propagate
  r1: .brain.propagate[(sys`rows;sys`cols;sys`inOff;sys`inLen;sys`outOff;sys`outLen);
                        br;sys`syns;sys`su;inc0;sys`inc1;out];
  / Step 4: clear inputter
  sys: sys, `br`syns`su`inc0`inc1`out`inp!
    (r1 0;r1 1;r1 2;r1 3;r1 4;r1 5;`int$(sys`inLen)#0i);
  sys}

/ Simple cycle (no pressure/cross-flow effects): used for the meta-brain.
/ Same as runCycle but pressure is effectively 0 (no special side effects).
/ Mirrors run_brain_cycle_simple in brain_engine_module.f90
runCycleSimple:{[sys]
  / Identical to runCycle for now; pressure effects are applied externally
  runCycle[sys]}

\d .
