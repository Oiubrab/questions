import sys,numpy as np,multiprocessing as mp,json,math
exec(open(sys.argv[2]).read())
MK=MEAN[0.]; CM=np.array([[MK[(a-s)%8] for a in range(8)] for s in range(8)])
def pol(th,p):
    P_=[]
    for b in (0,1):
        y=th[b]+np.log(p.bias); w=np.exp(y-y.max(1,keepdims=True))*mask; P_.append(w/np.maximum(w.sum(1,keepdims=True),1e-300))
    ps,pc=P_; A=np.zeros((8,8))
    for s in range(8):
        xs=np.zeros(NS);xs[ENT[s]]=1;xc=np.zeros(NS)
        for i in range(12):
            ms=xs[:,None]*ps; nxs=np.bincount(to[to>=0],weights=ms[to>=0],minlength=NS); e=np.bincount(ex[ex>=0],weights=ms[ex>=0],minlength=8)
            xc=xc.copy(); xc[ENT]+=e
            mc=xc[:,None]*pc; nxc=np.bincount(to[to>=0],weights=mc[to>=0],minlength=NS); A[s]+=np.bincount(ex[ex>=0],weights=mc[ex>=0],minlength=8)
            xs,xc=nxs,nxc
    return A
def met(A):
    a=(A*CM).sum()/max(A.sum(),1e-9); k=report(A); return a,sum(x in (0,1) for x in k),k
def job(a):
    id,rep=a
    th0=to_th(load("o_untrained_20261103",id)); p=defaults()
    th,o=run(th0,p,record=(500,2000,10000),seed=1000*rep+id)
    return id,rep,{b:met(o[b][0])[:2] for b in o},met(o[10000][0])[2].tolist()
if __name__=="__main__":
    p=defaults(); R=[]
    for id in range(1,17):
        th=to_th(load("o_open_brain_20261103",id)); R.append(met(pol(th,p)))
    print(f"REAL baseline, saved weights at 10k: a={np.mean([r[0] for r in R]):.3f}  correct slices={np.mean([r[1] for r in R]):.2f}  seeds>=.7: {sum(r[0]>=.7 for r in R)}  seeds with >=7 correct: {sum(r[1]>=7 for r in R)}; correct-slice histogram {np.bincount([r[1] for r in R],minlength=9).tolist()}")
    with mp.Pool(16) as pool: X=pool.map(job,[(i,r) for r in range(1,4) for i in range(1,17)])
    for b in (500,2000,10000):
        aa=[x[2][b][0] for x in X]; cc=[x[2][b][1] for x in X]
        print(f"model, stochastic, bar {b:6d}: a={np.mean(aa):.3f} correct slices={np.mean(cc):.2f}  seeds>=.7 {np.mean(np.array(aa)>=.7)*16:.1f}/16  >=7 correct {np.mean(np.array(cc)>=7)*16:.1f}/16 hist {np.bincount(cc,minlength=9).tolist()}")
    # does the initial state decide it? agreement of slice correctness across noise replicates and with the real run
    real=np.array([[x in (0,1) for x in r[2]] for r in R])
    rep={r:np.array([[k in (0,1) for k in x[3]] for x in sorted([y for y in X if y[1]==r],key=lambda y:y[0])]) for r in (1,2,3)}
    e=lambda u,v:(u==v).mean()
    print(f"slice-level agreement correct/wrong: replicate vs replicate {e(rep[1],rep[2]):.3f}; model vs real {e(rep[1],real):.3f}; expected by chance {real.mean()*rep[1].mean()+(1-real.mean())*(1-rep[1].mean()):.3f}")
