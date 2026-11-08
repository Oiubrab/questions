import json,sys,numpy as np
R=json.load(open(sys.argv[1])); base=R['base']
ids=sorted(base,key=int)
def ci(x):
    rng=np.random.default_rng(1); x=np.array(x); b=[rng.choice(x,len(x)).mean() for _ in range(2000)]; return np.percentile(b,[2.5,97.5])
print(f"{'lever':34}{'n':>3}{'a@100':>7}{'a@500':>7}{'a@10k':>7}{'>=.7':>5}{'good':>6}{'pMove':>6}{'c/1k':>6}{'dev':>6}{'alt':>9} d(a10k) vs base (CI)")
for n,r in R.items():
    if len(r)<16: print(n,'incomplete',len(r)); continue
    v=lambda k:np.array([r[i][k] for i in ids])
    a=v('a10k')*v('pm'); ab=np.array([base[i]['a10k']*base[i]['pm'] for i in ids])
    al=v('a10k'); d=al-np.array([base[i]['a10k'] for i in ids]); lo,hi=ci(d)
    c=np.maximum(190*a-45,1.6)
    print(f"{n:34}{len(r):3}{v('a100').mean():7.3f}{v('a500').mean():7.3f}{al.mean():7.3f}{(al>=.7).sum():5}{v('good').mean():6.2f}{v('pm').mean():6.3f}{c.mean():6.1f}{v('dev').mean():6.3f}{np.exp(np.log(np.maximum(v('alt'),1e-12)).mean()):9.2e} {d.mean():+.3f} ({lo:+.3f},{hi:+.3f})")
