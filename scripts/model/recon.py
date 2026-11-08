# Reconcile the uniform-slice metric with real alignment: the same 16 saved policies, three ways.
import sys,numpy as np,multiprocessing as mp,json
import os; exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'model.py')).read())
import kin
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
def job(id):
    p=defaults(); A=pol(to_th(load("o_open_brain_20261103",id)),p)
    pm=A.sum(1); B=A/np.maximum(pm[:,None],1e-9)
    # guard: normalise rows for the kinematic lookup; moverate is the mean move probability
    uni=float((B*CM).sum()/8)
    r=kin.run([b/b.sum() for b in B],bars=60000,seed=id,moverate=float(pm.mean()))
    wrong=[s for s in range(8) if CM[s,B[s].argmax()]<.85]
    return id,uni,r['a'],r['c'],float(pm.mean()),r['occ'].round(3).tolist(),wrong,float(r['occ'][wrong].sum()) if wrong else 0.
if __name__=="__main__":
    with mp.Pool(16) as pool: X=pool.map(job,range(1,17))
    print("id uniform kin_a kin_c  pm  wrongslices occ_in_wrong naive")
    for x in X: print(f"{x[0]:2d} {x[1]:.3f} {x[2]:.3f} {x[3]:6.1f} {x[4]:.2f}  {x[6]}  {x[7]:.2f}  {len(x[6])/8:.2f}")
    U=np.array([x[1] for x in X]);K=np.array([x[2] for x in X]);C=np.array([x[3] for x in X])
    print("means",U.mean(),K.mean(),C.mean())
    b=np.polyfit(U,K,1); print("kin_a = %.3f*uni %+.3f  r=%.3f"%(b[0],b[1],np.corrcoef(U,K)[0,1]))
    b2=np.polyfit(U,C,1); print("kin_c = %.1f*uni %+.1f  r=%.3f"%(b2[0],b2[1],np.corrcoef(U,C)[0,1]))
    print("uniform needed for kin_a=.7:",(0.7-b[1])/b[0])
    print("share in wrong slices vs naive, seeds with wrong:",np.mean([x[7] for x in X if x[6]]),np.mean([len(x[6])/8 for x in X if x[6]]))
    json.dump(X,open('recon.json','w'))
