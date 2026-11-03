/ Theory module (.theory) — the brain as an absorbing Markov chain (researchNotes/THEORY.md §2–3)
/ Used by q/programs/theory_checks.q and q/programs/policy_ceiling.q; the simulation doesn't load it.
/ Requires .dirs and .vision (load after q/load.q).

\d .theory

nR:6; nC:12; NS:nR*nC*8              / primary brain: directed states (cell, incoming label)
dr:`long$.dirs.dr; dc:`long$.dirs.dc

/ Single-unit transfer matrix on directed states k = 8*(r*nC+c) + d (d = incoming label - 1).
/ Q: brain -> brain; R: brain -> outputter slot (cols 2..9 under the bottom row).
/ Off-grid targets are masked and the rest renormalised, as in .brain.stepNeuron.
/ Full targets are ignored (low-density limit). Temperature 1, as in cat_mouse.q.
chain:{[syns;b]
  Q:(NS;NS)#0f; R:(NS;8)#0f; k:0;
  while[k<NS;
    cell:k div 8; d:k mod 8; r:cell div nC; c:cell mod nC;
    tr:r+dr; tc:c+dc;
    inG:(tr>=0)&(tr<nR)&(tc>=0)&(tc<nC);
    ex:(r=nR-1)&(tr=nR)&(tc>=2)&(tc<=9);
    w:(b*`float$syns[r;c;d])*`float$inG|ex;
    p:w%sum w;
    gi:where inG; Q[k;(8*(nC*tr gi)+tc gi)+7-gi]:p gi;   / target label = opposite of move
    ei:where ex; R[k;(tc ei)-2]:p ei;
    k+:1];
  (Q;R)}

/ Policy for one bar: A[s;a] = P(unit entering for slice s exits at slot a within 12 steps)
policy:{[QR] {[Q;R;s] x:NS#0f; x[(8*2+s)+6]:1f; e:8#0f; do[12; e+:x mmu R; x:x mmu Q]; e}[QR 0;QR 1] each til 8}

th:(0.5+til 8)*.vision.PI%4                           / slice centre angles
mv:flip (`float$dc; `float$dr)                        / move (dx,dy) for each action/slot
mvU:mv%{sqrt sum x*x} each mv
cosM:{[t] (mvU[;0]*cos t)+mvU[;1]*sin t} each th      / slice x action alignment
tow:cosM>0.01
ent:{neg sum {x*2 xlog x} x where x>0}

/ Summary of a policy: P(vision unit exits within the bar), towards rate, expected alignment
/ of the chosen move (slice-centre angles), and mutual information between slice and action
/ statsW: the same for output wiring p (slot a moves in .dirs direction p a; cat_mouse.q --wiring)
statsW:{[A;p] pm:sum each A; An:A%pm;
  `pMove`towards`expCos`MIbits!(avg pm; avg sum each An*tow[;p]; avg sum each An*cosM[;p]; (ent avg An)-avg ent each An)}
stats:{[A] statsW[A;til 8]}

/ ---- Any brain shape, and the modular brain (obstacles plan W.4) ----
/ chainG: chain for a brain of any size (rows = count syns; outputter = cols 2..9 under the bottom row)
chainG:{[syns;b]
  nR:count syns; nC:count first syns; ns:nR*nC*8;
  Q:(ns;ns)#0f; R:(ns;8)#0f; k:0;
  while[k<ns;
    cell:k div 8; d:k mod 8; r:cell div nC; c:cell mod nC;
    tr:r+dr; tc:c+dc;
    inG:(tr>=0)&(tr<nR)&(tc>=0)&(tc<nC);
    ex:(r=nR-1)&(tr=nR)&(tc>=2)&(tc<=9);
    w:(b*`float$syns[r;c;d])*`float$inG|ex;
    p:w%sum w;
    gi:where inG; Q[k;(8*(nC*tr gi)+tc gi)+7-gi]:p gi;
    ei:where ex; R[k;(tc ei)-2]:p ei;
    k+:1];
  (Q;R)}
/ Joint chain of sight brain then central brain: a unit leaving sight slot j enters the central
/ brain's top row at column 2+j with label 7 (cat_mouse.q SIGHT_LABEL). Sight states come first, so
/ a slice's start state has the same index as in a single brain.
chainMod:{[sightSyns;centSyns;b]
  S:chainG[sightSyns;b]; C:chainG[centSyns;b];
  ns:count S 0; nc:count C 0; nC:count first centSyns;
  E:{.[x;y;:;1f]}/[(8;nc)#0f;flip (til 8;(8*2+til 8)+6)];   / exit slot j -> central (0, 2+j), label 7
  Q:(S[0],'S[1] mmu E),((nc;ns)#0f),'C 0;
  (Q;((ns;8)#0f),C 1)}
/ Policy over any chain: A[s;a] = P(a unit entering for slice s exits at slot a within n steps)
policyG:{[QR;n] {[Q;R;n;s] x:(count Q)#0f; x[(8*2+s)+6]:1f; e:8#0f; do[n; e+:x mmu R; x:x mmu Q]; e}[QR 0;QR 1;n] each til 8}
/ Carry-over (obstacles plan I.1) from the start state of slice s: expected steps before the unit
/ leaves (row sums of N = (I-Q)^-1), and P(still in the brain) after each step count in ks
carry:{[Q;ks] x0:{[Q;s] @[(count Q)#0f;(8*2+s)+6;:;1f]}[Q] each til 8;
  N:inv (`float$(til count Q)=/:til count Q)-Q;
  (`residence,`$"alive",/:string ks)!(avg {[N;x] sum x mmu N}[N] each x0),{[Q;x0;k] avg {[Q;k;x] sum k {x mmu y}[;Q]/x}[Q;k] each x0}[Q;x0] each ks}

/ Survival curve: mass still in the brain after each of n steps, from start distribution x0
surv:{[Q;x0;n] s:(); x:x0; do[n; x:x mmu Q; s,:sum x]; s}
/ Start distribution: one unit at the vision input cell for slice s (label 7 -> d index 6)
top:{[s] x:NS#0f; x[(8*2+s)+6]:1f; x}

\d .
