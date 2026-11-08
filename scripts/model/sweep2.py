import sys,numpy as np,multiprocessing as mp,json,math
exec(open(sys.argv[2]).read())
OUT=sys.argv[3]
def S(p): p.ss=.03
def sa(**k):
    def f(p):
        for a,b in k.items(): setattr(p,a,b)
    return f
def comb(*fs):
    def f(p):
        for g in fs: g(p)
    return f
def decay(v):
    def f(p): p.dec=p.dec*v
    return f
def bias(*a):
    def f(p): p.bias=np.array(a,float)
    return f
L={ 'base':sa(),
 'strategic x0':sa(ss=0.),'strategic x0.03':S,'strategic x0.1':sa(ss=.1),'strategic x0.3':sa(ss=.3),'signed strategic':sa(ss=0.,sg=1.),
 'catch x0.25':sa(cs=.25),'catch x0.5':sa(cs=.5),'catch x2':sa(cs=2.),'catch window 8':sa(catch_w=8.),
 'threshold .6':sa(thr=.6),'threshold .85':sa(thr=.85),'neutral from bar 1':sa(np_e0=1),'bad calls 3':sa(bp=3),
 'reward scale .1':sa(lam=.1),'reward scale .5':sa(lam=.5),'hebb 100':sa(hebb=100.),'hebb 500':sa(hebb=500.),'hebb 1000':sa(hebb=1000.),
 'decay x.5':decay(.5),'decay x2':decay(2),'decay x5':decay(5),'cap 2e5':sa(cap=2e5),'cap 2e7':sa(cap=2e7),'floor 250':sa(F=250.),
 'alpha .9':sa(alpha=.9),'alpha 1.25':sa(alpha=1.25),'alpha 1.5':sa(alpha=1.5),'steps 8':sa(K=8),'steps 24':sa(K=24),
 'init x.25':sa(initc=.25),'init flat':sa(initc=0.),'bias down 3x':bias(.5,.5,.5,1,1,1.5,3,1.5),'bias flat':bias(1,1,1,1,1,1,1,1),'rot -22.5':sa(rot=-22.5)}
PAIR={'S+catch.5':comb(S,sa(cs=.5)),'S+neutral1':comb(S,sa(np_e0=1)),'S+init.25':comb(S,sa(initc=.25)),'S+initflat':comb(S,sa(initc=0.)),'S+rot':comb(S,sa(rot=-22.5)),'S+rot+thr.92':comb(S,sa(rot=-22.5,thr=.92)),
 'S+scale.5':comb(S,sa(lam=.5)),'S+hebb100':comb(S,sa(hebb=100.)),'S+hebb500':comb(S,sa(hebb=500.)),'S+decay2':comb(S,decay(2)),'S+alpha.9':comb(S,sa(alpha=.9)),'S+thr.85':comb(S,sa(thr=.85)),
 'S+catch.5+neutral1':comb(S,sa(cs=.5,np_e0=1)),'S+catch.5+neutral1+init.25':comb(S,sa(cs=.5,np_e0=1,initc=.25)),'signed+catch.5+neutral1':sa(ss=0.,sg=1.,cs=.5,np_e0=1)}
C=dict(L); C.update(PAIR)
def setup(name,id):
    p=defaults(); C[name](p)
    th0=to_th(load("o_untrained_20261103",id)); ic=getattr(p,'initc',None)
    if ic is not None:
        mn=th0.mean(); th0=mn+(th0-mn)*ic
    return p,np.clip(th0,math.log(p.F),math.log(p.cap))
def job(a):
    name,id,rep=a
    p,th0=setup(name,id)
    th,out=run(th0,p,record=(100,500,2000,10000),seed=(None if rep==0 else 1000*rep+id))
    Mk=MEAN[p.rot]; c=np.array([[Mk[(a_-s)%8] for a_ in range(8)] for s in range(8)])
    f=lambda b:float((out[b][0]*c).sum()/max(out[b][0].sum(),1e-9))
    A=out[10000][0]; d,alt=spont(th,p)
    return name,id,rep,dict(a100=f(100),a500=f(500),a2k=f(2000),a10k=f(10000),pm=float(A.sum(1).mean()),good=int(sum(c[s,A[s].argmax()]>=.85 for s in range(8))),bad=int(sum(c[s,A[s].argmax()]<.5 for s in range(8))),dev=float(d),alt=float(alt))
def rel(a):
    name,id=a
    p,th0=setup(name,id); p.bars=9000; p.sched=[(3000,'L180'),(6000,'A')]
    th,out=run(th0,p,record=(3000,6000,9000),seed=7000+id)
    tr=np.array(p.trace)
    r={}
    for lab,t0,t1 in (('fresh',0,3000),('L180',3000,6000),('backA',6000,9000)):
        w=tr[(tr[:,0]>=t0)&(tr[:,0]<t1)]
        hit=w[w[:,1]>=.6]; r[lab+'_t']=(float(hit[0,0]-t0) if len(hit) else None); r[lab+'_lvl']=float(w[-20:,1].mean()); r[lab+'_min']=float(w[:,1].min())
    return name,id,r
if __name__=="__main__":
    jobs=[(n,i,r) for n in C for i in range(1,17) for r in (1,2)]+[(n,i,0) for n in PAIR for i in range(1,17)]+[('signed strategic',i,0) for i in range(1,17)]
    R={}
    with mp.Pool(15) as pool:
        for n,i,r,x in pool.imap_unordered(job,jobs,chunksize=4):
            R.setdefault(n,{}).setdefault(str(r),{})[str(i)]=x
            json.dump(R,open(OUT,'w'))
        print('levers done',flush=True)
        RL={}
        names=['base','strategic x0.03','strategic x0.1','signed strategic','catch x0.5','neutral from bar 1','hebb 100','hebb 500','reward scale .5','decay x2','S+catch.5+neutral1','S+catch.5+neutral1+init.25','signed+catch.5+neutral1','S+rot']
        for n,i,x in pool.imap_unordered(rel,[(n,i) for n in names for i in range(1,17)],chunksize=2):
            RL.setdefault(n,{})[str(i)]=x
            json.dump(RL,open(OUT+'.rel.json','w'))
        print('relearn done',flush=True)
