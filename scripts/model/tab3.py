import json,numpy as np,multiprocessing as mp
import kin
R=json.load(open('sweep3.json')); ids=[str(i) for i in range(1,17)]
def kj(a):
    n,r,i=a; x=R[n][r][i]; A=np.array(x['A']); pm=A.sum(1); B=A/np.maximum(pm[:,None],1e-9); B=B/B.sum(1,keepdims=True)
    k=kin.run(list(B),bars=40000,seed=int(i)+100*int(r),moverate=float(min(pm.mean(),1.)),rot=x['rot'])
    return n,r,i,k['a'],k['c']
if __name__=="__main__":
    jobs=[(n,r,i) for n in R for r in ('0','1','2') for i in ids]
    K={}
    with mp.Pool(16) as pool:
        for n,r,i,a,c in pool.imap_unordered(kj,jobs,chunksize=8): K.setdefault(n,{}).setdefault(r,{})[i]=(a,c)
    json.dump(K,open('sweep3.kin.json','w'))
    rng=np.random.default_rng(2)
    bse=np.array([np.mean([K['base'][r][i][0] for r in '12']) for i in ids])
    bc=np.array([np.mean([K['base'][r][i][1] for r in '12']) for i in ids])
    print(f"{'config':24s} {'nf a':>5s} {'nf c':>5s} | {'a':>5s} {'da':>6s} {'ci':>16s} {'c':>6s} {'dc%':>5s} {'>=.7':>5s} | {'a500':>5s} {'surv':>5s} {'devmed':>6s} {'devmean':>7s} {'alt':>8s}")
    for n in R:
        a=np.array([np.mean([K[n][r][i][0] for r in '12']) for i in ids]); c=np.array([np.mean([K[n][r][i][1] for r in '12']) for i in ids])
        d=a-bse; B=[d[rng.integers(0,16,16)].mean() for _ in range(4000)]; lo,hi=np.percentile(B,[2.5,97.5])
        allA=[K[n][r][i][0] for r in '12' for i in ids]
        X=[R[n][r][i] for r in '12' for i in ids]
        print(f"{n:24s} {np.mean([K[n]['0'][i][0] for i in ids]):5.3f} {np.mean([K[n]['0'][i][1] for i in ids]):5.1f} | {a.mean():5.3f} {d.mean():+6.3f} [{lo:+.3f},{hi:+.3f}] {c.mean():6.1f} {100*(c.mean()/bc.mean()-1):+5.0f} {np.mean(np.array(allA)>=.7)*16:5.1f} | {np.mean([x['a500'] for x in X]):5.3f} {1-np.mean([x['pm'] for x in X]):5.3f} {np.median([x['dev'] for x in X]):6.3f} {np.mean([x['dev'] for x in X]):7.3f} {np.exp(np.mean(np.log([max(x['alt'],1e-12) for x in X]))):8.1e}")
    def rl(F,lab):
        RL=json.load(open(F)); print('\n'+lab)
        for n,v in RL.items():
            keys=[k[:-2] for k in v['1'] if k.endswith('_t')]
            s=f"{n:24s}"
            for k in keys:
                L=v['1'][k+'_len']; t=[x[k+'_t'] for x in v.values()]; cen=sum(q is None for q in t); med=np.median([L if q is None else q for q in t])
                lv=np.mean([x[k+'_lvl'] for x in v.values()]); s+=f" | {k:11s} t={med:5.0f} cens={cen:2d} lvl={lv:.3f}→{1.262*lv-.269:.3f} min={np.mean([x[k+'_min'] for x in v.values()]):+.2f}"
            print(s)
    rl('sweep3.json.rel.json','RELEARN short A3000 L180 3000 A3000 (t at real-equivalent .6; lvl uniform→mapped)')
    rl('sweep3.json.relv.json','RELEARN long A10000 L180 5000 A5000')
