/ Modular brain (.mod) — OBSTACLES_PLAN.md W.4 / §7 decision 4. User, 2026-11-02: "touch should be
/ an entirely new brain. There should be a reward brain, a sight brain and a touch brain, that all
/ feeds into a central brain, before its output is actually translated into action."
/ Sight, touch and central brains are separate grids of the same kind, each propagated on its own.
/ Between bars they are kept stacked top to bottom (sight; touch; central) in one primary state, so
/ every reward, history, decay and save rule works on all three unchanged: a usage index for row r
/ of a brain whose first stacked row is r0 is the index for row r0+r of the stack.
/ Loaded by cat_mouse.q and the tests.

\d .mod

/ Rows r0 .. r0+n-1 of a stacked primary state p, as an engine system for one brain (fresh usage,
/ empty outputter and inputter)
sub:{[p;r0;n;inOff;inLen;outOff;outLen]
  ix: r0+til n;
  `br`syns`su`inc0`inc1`out`inp`rows`cols`inOff`inLen`outOff`outLen!
    (p[`br] ix; p[`syns] ix; .syn.emptySparseUsage[]; p[`inc0] ix; p[`inc1] ix;
     `int$outLen#0i; `int$inLen#0i; `int$n; p`cols; inOff; inLen; outOff; outLen)}

/ Stack brains bs (top to bottom) back into one primary state. Usage indices are offset by each
/ brain's first stacked row (64 synapses per cell); the result keeps the last brain's outputter.
stack:{[bs]
  nC: `int$(first bs)`cols;
  offs: `int$0,-1_sums {count x`br} each bs;
  p: last bs;
  p[`br`syns`inc0`inc1]: {[bs;k] raze bs@\:k}[bs] each `br`syns`inc0`inc1;
  p[`su]: (1b; `int$raze {[b;o;nC] (b[`su] 1)+o*nC*64i}[;;nC]'[bs;offs]);
  p[`rows]: `int$count p`br;
  p}

/ Move units from an upstream outputter v into this brain's top row: v[j] units arrive at column
/ c0+j with incoming label d. Bookkeeping as in .brain.stepNeuron: the first unit in a cell sets
/ inc0, the second inc1. A cell holds at most HIGH; units that don't fit are lost (they had already
/ left the upstream brain, like any output). Returns (br;inc0;inc1;lost).
transferIn:{[br;inc0;inc1;c0;v;d]
  cs: c0+where 0i|v;
  r: {[d;s;c] x: s[0;0;c]; if[x>=.trinary.HIGH; s[3]+:1i; :s];
       s[0;0;c]: x+1i; $[x=0i; s[1;0;c]: d; s[2;0;c]: d]; s}[d]/[(br;inc0;inc1;0i); cs];
  r}

/ I.4 lesion: zero every route that leads into each stacked cell listed in spec ("R:C,R:C"), coming from
/ neighbours in the same brain (stacked brains are not neighbours across the brain boundary). modular:
/ whether the state is the stacked sight/touch/central brain (else one brain of nR rows).
lesion:{[modular;senseRows;nR;nC;syns;spec]
  blockOf:{[m;sr;nR;r] $[m; $[r<sr; (0;sr); r<2*sr; (sr;2*sr); (2*sr;nR)]; (0;nR)]}[modular;senseRows;nR];
  lesion1:{[blockOf;nC;syns;rc] R: rc 0; C: rc 1; bk: blockOf R;
    {[R;C;bk;nC;syns;d] r: R-.dirs.dr d; c: C-.dirs.dc d;
      $[(r>=bk 0)&(r<bk 1)&(c>=0)&c<nC; .[syns;(r;c;::;d);:;0i]; syns]}[R;C;bk;nC]/[syns;til 8]}[blockOf;nC];
  lesion1/[syns;{"I"$":" vs x} each "," vs spec]}

\d .
