#!/usr/bin/env python3
# Weights-only spontaneity of trained brains (CATCH_RATE_ANALYSIS.md §3.4), by the model's spont():
#   deviation: chance per bar that a vision unit leaves its slice's dominant path (mean over the 8 slices)
#   alt share: geometric mean of the best alternative's share at each step of that path
# Reads weights exported by export.q (results/model/w/<arm>_<id>.txt). The floor only enters through the weights.
# Usage (repo root): python3 scripts/model/spont_arm.py ARM [ARM ...]   (first arm is the paired reference)
import os,sys,glob,numpy as np
__file__=os.path.abspath(__file__)
exec(open(os.path.join(os.path.dirname(__file__),'model.py')).read())
def arm(a):
    out={}
    for f in sorted(glob.glob(os.path.join(M_DIR,'w',a+'_*.txt'))):
        i=int(f.rsplit('_',1)[1][:-4]); d,al=spont(to_th(load(a,i)),defaults()); out[i]=(d,al)
    return out
def boot(x,rng=np.random.default_rng(1)):
    b=np.sort([rng.choice(x,len(x)).mean() for _ in range(10000)]); return b[250],b[9749]
R={a:arm(a) for a in sys.argv[1:]}
ref=sys.argv[1]
print(f"{'arm':44}{'n':>3}{'dev med':>9}{'dev mean':>9}{'dev min':>8}{'dev max':>8}{'alt geo':>10}   paired vs {ref}: dev mean diff (CI); log10 alt diff (CI)")
for a,r in R.items():
    ids=sorted(r); d=np.array([r[i][0] for i in ids]); al=np.array([r[i][1] for i in ids])
    line=f"{a:44}{len(ids):>3}{np.median(d):>9.3f}{d.mean():>9.3f}{d.min():>8.3f}{d.max():>8.3f}{np.exp(np.log(al).mean()):>10.2e}"
    if a!=ref:
        c=[i for i in ids if i in R[ref]]
        dd=np.array([r[i][0]-R[ref][i][0] for i in c]); la=np.array([np.log10(r[i][1])-np.log10(R[ref][i][1]) for i in c])
        lo,hi=boot(dd); l2,h2=boot(la)
        line+=f"   {dd.mean():+.3f} ({lo:+.3f}, {hi:+.3f}); {la.mean():+.2f} ({l2:+.2f}, {h2:+.2f})"
    print(line)
