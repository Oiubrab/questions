# Baseline only: weights -> ideal moves -> actual moves. No simulator runs.
import sys,csv,numpy as np,importlib.util,re
M=sys.argv[1]; sys.path.insert(0,M)
import kin
src=open(M+'/model.py').read()
g={}; sys.argv=[sys.argv[0],M]; exec(src,g)
to,ex,mask,ENT,NS=g['to'],g['ex'],g['mask'],g['ENT'],g['NS']; BIAS=g['BIAS0']
def pol(f):
    th=np.log(np.stack([f[0:3].reshape(NS,8),f[6:9].reshape(NS,8)]))
    P_=[]
    for b in (0,1):
        y=th[b]+np.log(BIAS); w=np.exp(y-y.max(1,keepdims=True))*mask; P_.append(w/np.maximum(w.sum(1,keepdims=True),1e-300))
    ps,pc=P_; A=np.zeros((8,8))
    for s in range(8):
        xs=np.zeros(NS);xs[ENT[s]]=1;xc=np.zeros(NS)
        for i in range(12):
            ms=xs[:,None]*ps; nxs=np.bincount(to[to>=0],weights=ms[to>=0],minlength=NS); e=np.bincount(ex[ex>=0],weights=ms[ex>=0],minlength=8)
            xc=xc.copy(); xc[ENT]+=e
            mc=xc[:,None]*pc; nxc=np.bincount(to[to>=0],weights=mc[to>=0],minlength=NS); A[s]+=np.bincount(ex[ex>=0],weights=mc[ex>=0],minlength=8)
            xs,xc=nxs,nxc
    return A
rows=[]
for id in range(1,17):
    f=np.loadtxt(f"{M}/w/o_open_brain_20261103_{id}.txt").reshape(9,12,8,8)
    A=pol(f); pm=A.sum(1).mean(); An=A/np.maximum(A.sum(1,keepdims=True),1e-9)
    r=kin.run([An[s] for s in range(8)],bars=30000,seed=id,moverate=min(pm,1))
    L=list(csv.DictReader(open(f"results/o_open_brain_20261103/s{id}/simulation_log.csv")))[-2000:]
    sl=np.array([int(x['visionSlice']) for x in L]); act=np.array([int(x['outAction']) for x in L]); md=np.array([int(x['moveDist']) for x in L]); oe=np.array([int(x['outEnergy']) for x in L]); be=np.array([int(x['brainEnergy']) for x in L])
    al=np.array([float(x['alignment']) if x['alignment'] else np.nan for x in L]); ca=int(L[-1]['catches'])-int(L[0]['catches'])
    top=An.argmax(1); mv=md>0
    foreign=mv&(act!=top[sl])
    # is a move consistent with the policy? probability it would have been drawn
    pa=An[sl,act]
    rare=mv&(pa<.02)
    rows.append(dict(id=id,kin_a=r['a'],kin_c=r['c'],meas_a=np.nanmean(al),meas_c=ca/2,pm=pm,moved=mv.mean(),foreign=foreign.mean(),rare=rare.mean(),al_top=np.nanmean(al[mv&~foreign]),al_foreign=np.nanmean(al[foreign]) if foreign.any() else np.nan,al_rare=np.nanmean(al[rare]) if rare.any() else np.nan,multi=(oe>1).mean(),bE=be.mean()))
k=list(rows[0])
print(' '.join(f'{x:>9}' for x in k))
for r in rows: print(' '.join(f'{r[x]:9.3f}' if x!='id' else f'{r[x]:9d}' for x in k))
print(' '.join(f'{np.nanmean([r[x] for r in rows]):9.3f}' for x in k))
v=lambda x:np.array([r[x] for r in rows])
print('corr(meas_a, kin_a)=%.3f  corr(meas_c,kin_c)=%.3f corr(a loss, rare)=%.3f corr(c loss, rare)=%.3f corr(a loss,multi)=%.3f'%(np.corrcoef(v('meas_a'),v('kin_a'))[0,1],np.corrcoef(v('meas_c'),v('kin_c'))[0,1],np.corrcoef(v('kin_a')-v('meas_a'),v('rare'))[0,1],np.corrcoef(v('kin_c')-v('meas_c'),v('rare'))[0,1],np.corrcoef(v('kin_a')-v('meas_a'),v('multi'))[0,1]))
