# Paired statistics for the first evaluation: both noise reps averaged per seed, compared with base; agreement with the noise-free version.
import json,numpy as np
R=json.load(open('sweep2.json')); NF=json.load(open('sweep_nonoise.json'))
ids=[str(i) for i in range(1,17)]
def perseed(n,k='a10k'): return np.array([np.mean([R[n][r][i][k] for r in ('1','2')]) for i in ids])
b=perseed('base'); rng=np.random.default_rng(1)
def ci(d):
    B=[d[rng.integers(0,16,16)].mean() for _ in range(4000)]; return np.percentile(B,[2.5,97.5])
print(list(NF.keys())[:8]); 
t=type(next(iter(NF.values()))); print(t)
print(json.dumps(next(iter(NF.values())))[:300])
rows=[]
for n in R:
    d=perseed(n)-b; lo,hi=ci(d)
    allv=[R[n][r][i]['a10k'] for r in ('1','2') for i in ids]
    print(f"{n:32s} a={np.mean(allv):.3f} d={d.mean():+.3f} [{lo:+.3f},{hi:+.3f}] real-map={1.262*np.mean(allv)-.269:.3f} >=.768:{np.mean(np.array(allv)>=.768)*16:4.1f}/16 surv={1-np.mean([R[n][r][i]['pm'] for r in ('1','2') for i in ids]):.3f} dev_med={np.median([R[n][r][i]['dev'] for r in ('1','2') for i in ids]):.3f} alt_gm={np.exp(np.mean(np.log([max(R[n][r][i]['alt'],1e-12) for r in ('1','2') for i in ids]))):.1e}")
