/ Scaling module (.scale): constants derived from the brain's configuration at start-up
/ (researchNotes/CONSTANT_SCALING.md; revival plan §2 point 10: "if I request a larger brain, some of these
/ constants ... should scale based on the mathematics automatically").
/ Requires .dirs and .syn. Used by q/programs/cat_mouse/params.q; tested by q/tests/test_scaling.q.

\d .scale

/ Bias-only routing on one brain's cells: an untrained brain's weights are equal on average, so a unit's next
/ cell follows the direction bias, masked to targets inside the grid or to the outputter (bottom row, columns
/ outOff..outOff+outLen-1) and renormalised, as in .brain.stepNeuron and .theory.chainG. With equal weights
/ the incoming direction doesn't matter, so cells are the states.
/ Returns (M;E): M[k;k'] cell to cell, E[k;j] cell to outputter slot j.
grid:{[nR;nC;outOff;outLen;bias]
  n:nR*nC; M:(n;n)#0f; E:(n;outLen)#0f; k:0;
  while[k<n;
    r:k div nC; c:k mod nC; tr:r+`long$.dirs.dr; tc:c+`long$.dirs.dc;
    inG:(tr>=0)&(tr<nR)&(tc>=0)&tc<nC;
    ex:(r=nR-1)&(tr=nR)&(tc>=outOff)&tc<outOff+outLen;
    w:bias*`float$inG|ex; p:w%sum w;
    gi:where inG; M[k;(nC*tr gi)+tc gi]:p gi;
    ei:where ex; E[k;(tc ei)-outOff]:p ei;
    k+:1];
  (M;E)}

/ F(t), t = 1..T: share of a vision unit's energy that has reached the outputter within t steps, averaged over the
/ input columns. g is the geometry, a dict `modular`senseRows`centRows`nC`inOff`inLen`outOff`outLen. Modular: the
/ sight brain (senseRows) steps first and what leaves it enters the central brain's top row (column inOff+slot) and
/ moves again in the same step, as in the bar loop. Single brain: senseRows is ignored; the brain has centRows rows.
exitCdf:{[g;bias;T]
  C:grid[g`centRows;g`nC;g`outOff;g`outLen;bias];
  x0:@[(g[`nC]*$[g`modular;g`senseRows;g`centRows])#0f;g[`inOff]+til g`inLen;:;1f%g`inLen];
  if[not g`modular; e:(); x:x0; do[T; e,:sum x mmu C 1; x:x mmu C 0]; :sums e];
  G:grid[g`senseRows;g`nC;g`outOff;g`outLen;bias];
  e:(); xs:x0; xc:(g[`nC]*g`centRows)#0f;
  do[T; o:xs mmu G 1; xs:xs mmu G 0; xc:@[xc;g[`inOff]+til g`outLen;+;o]; e,:sum xc mmu C 1; xc:xc mmu C 0];
  sums e}
/ The reference: the brain the constants were tested on (CATCH_RATE_PLAN.md §6): modular, sight and central 3x12,
/ vision and outputs at columns 2-9, 12 steps per bar. S is chosen so any brain passes the share REF passes per bar.
REF: `modular`senseRows`centRows`nC`inOff`inLen`outOff`outLen!(1b;3;3;12;2;8;2;8)
REF_STEPS: 12
HORIZON: 400                   / steps searched for S (a brain needing more is rejected)

/ Steps per bar: the fewest steps that let a bar pass at least the reference share of the input's energy
steps:{[F;Fref] i:first where F>=Fref; if[null i; '"steps: the brain passes too little energy within the horizon"]; `int$1+i}

/ Log-step of one punish call per route use at S steps per bar and reward scale lam (synapses.q's rule)
punishStep:{[S;lam] lam*neg log .syn.PUNISH_MULT*.syn.STEP_MULT xexp S}

/ Undirected (common-mode) gain per route use a wrong route can collect without the meta-brain raising it:
/ strategic at its floor scope and magnitude, plus the catch boost at the best memoryless catch rate cRef
commonMode:{[lam;scopeMin;magMin;stratMult;catchWin;cRef;catchBoost]
  lam*(scopeMin*magMin*log stratMult)+catchWin*cRef*log catchBoost}

/ Punish calls: the fewest calls n with n*P >= margin*C (separation, CATCH_RATE_ANALYSIS.md §3.1)
punishCalls:{[P;C;margin] `int$1|ceiling (margin*C)%P}

/ Hebb: re-learning needs ln(1+h/floor) > n*P (CATCH_RATE_ANALYSIS.md §3.2); keep it by factor margin
hebb:{[fl;nP;margin] `int$fl*(exp margin*nP)-1}

\d .
