import json,numpy as np,multiprocessing as mp
import kin
R=json.load(open('sweep5.json')); ids=[str(i) for i in range(1,17)]
K0=json.load(open('sweep3.kin.json'))
def kj(a):
    n,r,i=a; x=R[n][r][i]; A=np.array(x['A']); pm=A.sum(1); B=A/np.maximum(pm[:,None],1e-9); B=B/B.sum(1,keepdims=True)
    k=kin.run(list(B),bars=40000,seed=int(i)+100*int(r),moverate=float(min(pm.mean(),1.)),rot=x['rot'])
    return n,r,i,k['a'],k['c']
if __name__=="__main__":
    K={}
    with mp.Pool(16) as pool:
        for n,r,i,a,c in pool.imap_unordered(kj,[(n,r,i) for n in R for r in '012' for i in ids],chunksize=8): K.setdefault(n,{}).setdefault(r,{})[i]=(a,c)
    json.dump(K,open('sweep5.kin.json','w'))
    rng=np.random.default_rng(3)
    bse=np.array([np.mean([K0['base'][r][i][0] for r in '12']) for i in ids]); bc=np.mean([K0['base'][r][i][1] for r in '12' for i in ids])
    for n in R:
        a=np.array([np.mean([K[n][r][i][0] for r in '12']) for i in ids]); d=a-bse; lo,hi=np.percentile([d[rng.integers(0,16,16)].mean() for _ in range(4000)],[2.5,97.5])
        c=np.mean([K[n][r][i][1] for r in '12' for i in ids]); X=[R[n][r][i] for r in '12' for i in ids]
        print(f"{n:28s} nf a={np.mean([K[n]['0'][i][0] for i in ids]):.3f} c={np.mean([K[n]['0'][i][1] for i in ids]):5.1f} >=.7:{sum(K[n]['0'][i][0]>=.7 for i in ids):2d} | noisy a={a.mean():.3f} [{lo:+.3f},{hi:+.3f}] c={c:5.1f} >=.7:{np.mean([K[n][r][i][0]>=.7 for r in '12' for i in ids])*16:4.1f} a500={np.mean([x['a500'] for x in X]):.3f} surv={1-np.mean([x['pm'] for x in X]):.3f} devmed={np.median([x['dev'] for x in X]):.3f} devmean={np.mean([x['dev'] for x in X]):.3f} alt={np.exp(np.mean(np.log([max(x['alt'],1e-12) for x in X]))):.1e}")
    RV=json.load(open('sweep5.json.rel.json')); M=lambda l:1.262*l-.269
    for v in ('noisy','nf'):
        print(v)
        for n,X in RV.items():
            x=X[v]; g=lambda k:[s[k] for s in x.values()]
            def tt(k,L):
                t=g(k+'_t'); return np.median([L if q is None else q for q in t]),sum(q is None for q in t)
            l=tt('L180@10000',5000); b=tt('A@15000',5000)
            print(f"{n:28s} fresh {np.mean(g('A@0_lvl')):.3f} | L180 t={l[0]:5.0f} c{l[1]:2d} lvl={np.mean(g('L180@10000_lvl')):+.3f}({M(np.mean(g('L180@10000_lvl'))):+.3f}) | back t={b[0]:5.0f} c{b[1]:2d} lvl={np.mean(g('A@15000_lvl')):.3f}({M(np.mean(g('A@15000_lvl'))):.3f})")
