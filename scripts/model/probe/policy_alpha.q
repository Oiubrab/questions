/ Scratch analysis: per-slice policy, temperature curve, route structure of trained modular brains.
\l q/load.q
\l q/modules/analysis/theory.q
\c 60 250
arm: first .z.x
ids: 1+til 16
\l q/modules/world/relearn.q
perm: .relearn.ANGLE_ORDER
/ chain with exponent alpha on (b*W) (alpha=1 is .theory.chainG); alpha=0w means argmax route
chainA:{[syns;b;alpha]
  nR:count syns; nC:count first syns; ns:nR*nC*8; dr:.theory.dr; dc:.theory.dc;
  Q:(ns;ns)#0f; R:(ns;8)#0f; k:0;
  while[k<ns;
    cell:k div 8; d:k mod 8; r:cell div nC; c:cell mod nC;
    tr:r+dr; tc:c+dc;
    inG:(tr>=0)&(tr<nR)&(tc>=0)&(tc<nC);
    ex:(r=nR-1)&(tr=nR)&(tc>=2)&(tc<=9);
    bw:(b*`float$syns[r;c;d])*`float$inG|ex;
    w:$[alpha=0w; `float$bw=max bw; (bw%max bw) xexp alpha];
    p:w%sum w;
    gi:where inG; Q[k;(8*(nC*tr gi)+tc gi)+7-gi]:p gi;
    ei:where ex; R[k;(tc ei)-2]:p ei;
    k+:1];
  (Q;R)}
chainModA:{[S;C]
  ns:count S 0; nc:count C 0;
  E:{.[x;y;:;1f]}/[(8;nc)#0f;flip (til 8;(8*2+til 8)+6)];
  Q:(S[0],'S[1] mmu E),((nc;ns)#0f),'C 0;
  (Q;((ns;8)#0f),C 1)}
polA:{[w;alpha;n] QR: chainModA[chainA[3#w;.dirs.vBias;alpha];chainA[6_w;.dirs.vBias;alpha]]; .theory.policyG[QR;n]}
b: .dirs.vBias
rows: {[id]
  w: (get hsym `$arm,"/s",(string id),"/weights_seed",(string id),".bin") 0;
  e: ("IIIIIIF";enlist ",") 0: hsym `$arm,"/s",(string id),"/epoch_summary.csv";
  meas: last e`meanAlignment;
  al: 1 1.25 1.5 2 3 5 0w;
  ec: {[w;perm;a] (.theory.statsW[polA[w;a;24];perm])`expCos}[w;perm] each al;
  A: polA[w;1f;12];
  st: .theory.statsW[A;perm];
  An: A%sum each A;
  ps: sum each An*.theory.cosM[;perm];
  (`id`meas`pred12`pMove!(id;meas;st`expCos;st`pMove)),((`$"a",/:ssr[;".";"_"] each string al)!ec),(`$"s",/:string til 8)!ps
  } each ids;
t: rows
show `id xasc t
-1 "means:"; show (key first t)!avg each flip t
