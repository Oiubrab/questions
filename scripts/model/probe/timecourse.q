\l q/load.q
\l q/modules/analysis/theory.q
\l q/modules/world/relearn.q
\c 60 240
S: first .z.x
ids:1+til 16
perm:.relearn.ANGLE_ORDER
/ rows of the stacked flat vector -> syns nested 9x12x8x8
unflat:{[f] 9 12 8 8#f}
pol:{[w] P:.theory.policyG[.theory.chainMod[3#w;6_w;.dirs.vBias];24]; P%sum each P}
offs:{[P] top:{first idesc x} each P; ({[s;a] ((a-s)+4) mod 8}'[til 8;top])-4}
/ offsets per slice at each snapshot (every 500 bars) and E[cos]
tc:{[id] p:get hsym `$S,"/pr",(string id),"/probe.bin"; sn:p 6;
  r:{[f] w:unflat `int$f; P:pol w; (offs P; avg sum each P*.theory.cosM[;perm])} each sn;
  (id;r[;0];r[;1])}
R: tc each ids;
O: R[;1]; E: R[;2];
nc: {sum each x in 0 1} each O;                       / correct slices per seed per snapshot
bars: 500*1+til count first nc;
-1 "correct slices (of 8) by bar, mean over seeds; and predicted E[cos]";
show ([] bar:bars; nCorrect:avg nc; expCos:avg E; seeds7plus:sum each flip nc>=7);
cg:{[o] sum each not (1_o)=-1_o};
c: cg each O;
-1 "top-slot changes per seed (20 snapshots, 500 bars apart): bars 500-2000 ",(string avg sum each 3#'c),"; 2000-5000 ",(string avg sum each 6#'3_'c),"; 5000-10000 ",(string avg sum each 9_'c);
-1 "of slices wrong at bar 2000, share still wrong at 10000, and share of final wrong that were wrong at 2000:";
w2:{not x[3] in 0 1} each O; wE:{not (last x) in 0 1} each O;
-1 "  wrong@2k: ",(string sum sum each w2),"  wrong@10k: ",(string sum sum each wE),"  both: ",string sum sum each w2&wE;
-1 "correct at 2000 that went wrong later: ",string sum sum each (not w2)&wE;
/ decompose sight and central for final weights: sight top exit j for slice s; central top exit a from entry j
dec:{[id] p:get hsym `$S,"/pr",(string id),"/probe.bin"; w:unflat `int$p 7;
  Ps:.theory.policyG[.theory.chainG[3#w;.dirs.vBias];24]; Ps:Ps%sum each Ps;
  Pc:.theory.policyG[.theory.chainG[6_w;.dirs.vBias];24]; Pc:Pc%sum each Pc;
  js:{first idesc x} each Ps; as:{first idesc x} each Pc;
  jsh:max each Ps; ash:max each Pc;
  full:pol w; ft:{first idesc x} each full;
  ([] id:id; slice:til 8; sightJ:js; sShare:jsh; centA:as[js]; cShare:ash[js]; fullA:ft; off:offs full)}
D: raze dec each ids;
D: update sOff:((sightJ-slice+4) mod 8)-4, cOff:((centA-sightJ+4) mod 8)-4, cen0:{x}each 0 from D;
-1 "composition: sight offset (exit slot - slice) x central offset (action slot - entry slot), for correct and wrong slices";
show select n:count i by wrong:not off in 0 1, sOff, cOff from D;
-1 "wrong slices: mean shares"; show select n:count i, sShare:avg sShare, cShare:avg cShare by wrong:not off in 0 1 from D;
