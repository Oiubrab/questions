#!/usr/bin/env python3
# CATCH_RATE_ANALYSIS.md §2: the stochastic mean-field learning model (noise-free with seed=None, noisy otherwise).
# Loaded by the other scripts with exec(), so its names land in their globals.
import os
# Solved tables and exported weights live under results/model (gitignored); MODEL_OUT overrides. Weights: <M_DIR>/w/<arm>_<id>.txt (export.q)
M_DIR=os.environ.get('MODEL_OUT',os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','results','model'))
# Mean-field learning model of the modular brain: expected change in log-weights per bar, from the rules in
# brain.q, synapses.q and bar_loop.q. One vision unit per bar; low density; no world, no shot noise.
# Sight (3x12) feeds central (3x12); the action is the central exit slot. A mean slice policy is computed each update.
import numpy as np, math, sys, json
NR,NC=3,12
DR=np.array([-1,-1,-1,0,0,1,1,1]); DC=np.array([-1,0,1,-1,1,-1,0,1])
BIAS0=np.array([.5,.5,.5,1,1,1.5,1.8,1.5])
ENTRY=6   # label 7
def idx(r,c,d): return ((r*NC+c)*8+d)
NS=NR*NC*8
to=np.full((NS,8),-1); ex=np.full((NS,8),-1); mask=np.zeros((NS,8),bool)
for r in range(NR):
  for c in range(NC):
    for d in range(8):
      for o in range(8):
        tr,tc=r+DR[o],c+DC[o]
        if 0<=tr<NR and 0<=tc<NC: to[idx(r,c,d),o]=idx(tr,tc,7-o); mask[idx(r,c,d),o]=True
        elif r==NR-1 and tr==NR and 2<=tc<=9: ex[idx(r,c,d),o]=tc-2; mask[idx(r,c,d),o]=True
ENT=np.array([idx(0,2+j,ENTRY) for j in range(8)])
def reinf(lam): return math.log(1.2/.95**12)*lam
def pun(lam): return -math.log(.8*.95**12)*lam
def direct(rot,thr,ncalls,bcalls,lam,mom=1,n=720):
    """D[k]: expected direct log-gain per use for action k slots from the slice (k=a-s mod 8), true angle uniform in slice."""
    D=np.zeros(8); Pr=np.zeros(8); M=np.zeros(8)
    u=(np.arange(n)+.5)/n*45+rot
    for k in range(8):
        c=np.cos(np.radians(45*k-u)); M[k]=c.mean()
        calls=np.round(2*(.5+1.5*c))+mom-1
        D[k]=np.mean(np.where(c>thr,calls*reinf(lam),np.where(c<-.01,-bcalls*pun(lam),-ncalls*pun(lam))))
        Pr[k]=np.mean(c>thr)
    return D,Pr,M
MEAN={r:direct(r,.7,1,1,.25,n=720)[2] for r in (0.,-22.5,-11.25,22.5)}
class P: pass
def defaults():
    p=P(); p.K=12;p.lam=.25;p.hebb=250.;p.F=25.;p.cap=2e6;p.dec=-math.log(.9875)/5;p.thr=.7;p.scope=20.;p.mag=1.;p.ss=1.;p.catch_w=20.;p.cs=1.
    p.np_e0=0;p.np=1;p.rot=0.;p.bias=BIAS0.copy();p.alpha=1.;p.rate_gate=.015;p.bars=10000;p.bp=None
    p.dmult=1.; return p
def catch_rate(a): return max((190*a-45),1.6)/1000.
def run(th0,p,record=(100,500,1000,2000,5000,10000),seed=None):
    """th0: log weights, shape (2,NS,8) sight then central."""
    th=th0.copy(); out={}; rng=np.random.default_rng(seed)
    t=0; sw_t=-1; world='A'; trace=[]
    logb=np.log(p.bias)
    lF,lC=math.log(p.F),math.log(p.cap)
    S=np.arange(8)
    while t<p.bars:
        ts=t-sw_t
        dt=2 if ts<600 else (5 if ts<2000 else 40)
        for sb,sw in getattr(p,'sched',[]):
            if t>=sb and sb>sw_t: sw_t=sb; world=sw; ts=0; dt=2
        # choice probabilities
        P_=[]
        for b in (0,1):
            z=p.alpha*(th[b]+logb); z=z-z.max(1,keepdims=True); w=np.exp(z)*mask; P_.append(w/np.maximum(w.sum(1,keepdims=True),1e-300))
        ps,pc=P_
        dth=np.zeros_like(th); use=np.zeros_like(th); sec=np.zeros_like(th)
        # epoch-dependent punishment
        ep=int(t//2000); nE=max(p.bars//2000,1); bcalls=(int(round(1+ep/nE*2)) if p.bp is None else p.bp)
        ncalls=(p.np if (ep>0 or p.np_e0) else 0)
        D,_,_=direct(p.rot,p.thr,ncalls,bcalls,p.lam,n=90)
        # forward for each slice
        A=np.zeros((8,8)); Xs=np.zeros((8,p.K,NS)); Xc=np.zeros((8,p.K,NS))
        for s in S:
            xs=np.zeros(NS); xs[ENT[s]]=1; xc=np.zeros(NS)
            for i in range(p.K):
                Xs[s,i]=xs
                ms=xs[:,None]*ps
                nxs=np.bincount(to[to>=0],weights=ms[to>=0],minlength=NS)
                e=np.bincount(ex[ex>=0],weights=ms[ex>=0],minlength=8)
                xc=xc.copy(); xc[ENT]+=e
                Xc[s,i]=xc
                mc=xc[:,None]*pc
                nxc=np.bincount(to[to>=0],weights=mc[to>=0],minlength=NS)
                A[s]+=np.bincount(ex[ex>=0],weights=mc[ex>=0],minlength=8)
                xs,xc=nxs,nxc
        # value of each action for each slice, and mean direct credit for units that stay in the brain (bar's action comes from elsewhere)
        pm=A.sum(1)
        legs=(lambda a:(a+4)%8) if world=='L180' else (lambda a:a)
        eyes=(lambda s:(s+4)%8) if world=='E180' else (lambda s:s)
        Mk=MEAN[p.rot]
        Dx=D+(getattr(p,'sg',0.)*p.scope*p.mag*math.log(1.1)*p.lam*(Mk-p.thr))
        Ds=np.array([[Dx[(legs(a)-eyes(s))%8] for a in range(8)] for s in S])
        Cos=np.array([[Mk[(legs(a)-eyes(s))%8] for a in range(8)] for s in S])
        a_all=float((A*Cos).sum()/8)            # mean move cosine over bars, counting no-move bars as 0
        a_est=float(((A*Cos).sum(1)/np.maximum(pm,1e-9)).mean())
        ct=catch_rate(a_all)
        trace.append((t,a_est))
        C=0.
        gn=getattr(p,'gate_n',None)
        if gn is None:
            if ct>=p.rate_gate: C+=p.ss*p.scope*p.mag*math.log(1.1)*p.lam
        else:
            # chance a 200-bar window holds >=gn catches: Poisson tail, with catches from the fit to the saved brains
            lam_=200*max(225.7*a_all-87.9,1.6)/1000.
            term=math.exp(-lam_); cdf=term
            for j in range(1,gn): term*=lam_/j; cdf+=term
            C+=max(0.,1-cdf)*p.ss*p.scope*p.mag*math.log(1.1)*p.lam
        C+=p.cs*p.catch_w*ct*math.log(1.5)*p.lam
        for s in S:
            L=(A[s]*Ds[s]).sum()
            # backward values with r steps remaining
            Vc=[np.full(NS,L)]; Vs=[np.full(NS,L)]
            for r in range(1,p.K+1):
                vp=Vc[r-1]
                tv=np.where(to>=0,vp[np.maximum(to,0)],0.)+np.where(ex>=0,Ds[s][np.maximum(ex,0)],0.)
                vc=(pc*tv).sum(1); Vc.append(vc)
            for r in range(1,p.K+1):
                vp=Vs[r-1]; vce=Vc[r][ENT]
                tv=np.where(to>=0,vp[np.maximum(to,0)],0.)+np.where(ex>=0,vce[np.maximum(ex,0)],0.)
                Vs.append((ps*tv).sum(1))
            for i in range(p.K):
                r=p.K-i
                tvs=np.where(to>=0,Vs[r-1][np.maximum(to,0)],0.)+np.where(ex>=0,Vc[r][ENT][np.maximum(ex,0)],0.)
                tvc=np.where(to>=0,Vc[r-1][np.maximum(to,0)],0.)+np.where(ex>=0,Ds[s][np.maximum(ex,0)],0.)
                us=Xs[s,i][:,None]*ps; uc=Xc[s,i][:,None]*pc
                dth[0]+=us*tvs/8; dth[1]+=uc*tvc/8; use[0]+=us/8; use[1]+=uc/8
                sec[0]+=us*(tvs+C)**2/8; sec[1]+=uc*(tvc+C)**2/8
        W=np.exp(th)
        dth=dth*p.dmult+use*(C+np.log1p(p.hebb/W))-p.dec
        nz=rng.standard_normal(th.shape)*np.sqrt(dt*sec) if getattr(p,'noise',True) and seed is not None else 0.
        th=np.clip(th+dt*dth+nz,lF,lC)
        t+=dt
        for b in record:
            if t-dt<b<=t: out[b]=(A.copy(),pm.mean(),a_est)
    p.trace=trace
    return th,out
def report(A):
    top=A.argmax(1); k=((top-np.arange(8)+4)%8)-4
    return k
def load(a,id):
    f=np.loadtxt(f"{M_DIR}/w/{a}_{id}.txt",dtype=float)
    f=f.reshape(9,12,8,8)
    return f
def to_th(f): # stacked sight, touch, central -> (2,NS,8)
    return np.log(np.stack([f[0:3].reshape(NS,8),f[6:9].reshape(NS,8)]))

def spont(th,p):
    """On each slice's dominant path: chance per bar of leaving it; share of the best alternative; alternatives' shares at each step."""
    out=[]
    z=[]
    for b in (0,1):
        y=p.alpha*(th[b]+np.log(p.bias)); y=y-y.max(1,keepdims=True); w=np.exp(y)*mask; z.append(w/np.maximum(w.sum(1,keepdims=True),1e-300))
    ps,pc=z
    res=[]
    for s in range(8):
        k=ENT[s]; b=0; prob=1.; alts=[]
        for _ in range(40):
            P_=ps if b==0 else pc
            o=P_[k].argmax(); pmax=P_[k,o]
            q=np.sort(P_[k])[::-1]
            alts.append(q[1]); prob*=pmax
            if ex[k,o]>=0:
                if b==0: k=ENT[ex[k,o]]; b=1; continue
                break
            k=to[k,o]
        res.append((1-prob,np.exp(np.mean(np.log(np.maximum(alts,1e-12))))))
    r=np.array(res).mean(0)
    return r[0],r[1]
