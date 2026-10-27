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
stats:{[A] pm:sum each A; An:A%pm;
  `pMove`towards`expCos`MIbits!(avg pm; avg sum each An*tow; avg sum each An*cosM; (ent avg An)-avg ent each An)}

/ Survival curve: mass still in the brain after each of n steps, from start distribution x0
surv:{[Q;x0;n] s:(); x:x0; do[n; x:x mmu Q; s,:sum x]; s}
/ Start distribution: one unit at the vision input cell for slice s (label 7 -> d index 6)
top:{[s] x:NS#0f; x[(8*2+s)+6]:1f; x}

\d .
