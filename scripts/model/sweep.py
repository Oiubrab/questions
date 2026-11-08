import sys,numpy as np,multiprocessing as mp,json,math
exec(open(sys.argv[2]).read())
OUT=sys.argv[3]
def cfgs():
    c={}
    c['base']=lambda p:None
    for v in (0,.03,.1,.3,.5,1,2): c[f'strategic x{v}']=(lambda v:lambda p:setattr(p,'ss',v))(v)
    for v in (0,.25,.5,1,2): c[f'catch boost x{v}']=(lambda v:lambda p:setattr(p,'cs',v))(v)
    for v in (3,5,8,12,20): c[f'catch window {v}']=(lambda v:lambda p:setattr(p,'catch_w',v))(v)
    for v in (.6,.65,.7,.75,.85,.92): c[f'threshold {v}']=(lambda v:lambda p:setattr(p,'thr',v))(v)
    c['neutral punish from bar 1']=lambda p:setattr(p,'np_e0',1)
    for v in (2,3): c[f'neutral calls {v}']=(lambda v:lambda p:setattr(p,'np',v))(v)
    for v in (1,2,3): c[f'bad calls fixed {v}']=(lambda v:lambda p:setattr(p,'bp',v))(v)
    for v in (.05,.1,.5,1): c[f'reward scale {v}']=(lambda v:lambda p:setattr(p,'lam',v))(v)
    for v in (50,100,500,1000,2000): c[f'hebb {v}']=(lambda v:lambda p:setattr(p,'hebb',float(v)))(v)
    for v in (.25,.5,2,5,10,25): c[f'decay x{v}']=(lambda v:lambda p:setattr(p,'dec',p.dec*v))(v)
    for v in (2e4,2e5,2e7,2e8): c[f'cap {v:.0e}']=(lambda v:lambda p:setattr(p,'cap',v))(v)
    for v in (5,100,250): c[f'floor {v}']=(lambda v:lambda p:setattr(p,'F',float(v)))(v)
    for v in (.75,.9,1.25,1.5,2,3): c[f'alpha {v}']=(lambda v:lambda p:setattr(p,'alpha',v))(v)
    for v in (8,10,16,24,36): c[f'steps per bar {v}']=(lambda v:lambda p:setattr(p,'K',v))(v)
    c['init narrow x0.5 log']=lambda p:setattr(p,'initc',.5)
    c['init narrow x0.25 log']=lambda p:setattr(p,'initc',.25)
    c['init flat']=lambda p:setattr(p,'initc',0.)
    def b(*a): 
        def f(p): p.bias=np.array(a,float)
        return f
    c['bias flat']=b(1,1,1,1,1,1,1,1)
    c['bias down 3x']=b(.5,.5,.5,1,1,1.5,3,1.5)
    c['bias no-up .1']=b(.1,.1,.1,1,1,1.5,1.8,1.5)
    c['bias no-side .3']=b(.5,.5,.5,.3,.3,1.5,1.8,1.5)
    c['bias straight 3x, no side/up']=b(.1,.1,.1,.3,.3,1.5,3,1.5)
    c['bias skew to SE (k=+1)']=b(.5,.5,.5,1,1,1.0,1.8,2.5)
    for v in (-22.5,-11.25,22.5): c[f'slice rotation {v}']=(lambda v:lambda p:setattr(p,'rot',v))(v)
    c['rot -22.5 + thr .85']=lambda p:(setattr(p,'rot',-22.5),setattr(p,'thr',.85))
    c['rot -22.5 + thr .92']=lambda p:(setattr(p,'rot',-22.5),setattr(p,'thr',.92))
    return c
C=cfgs()
def job(a):
    name,id=a
    p=defaults(); C[name](p)
    th0=to_th(load("o_untrained_20261103",id))
    ic=getattr(p,'initc',None)
    if ic is not None:
        mn=th0.mean(); th0=mn+(th0-mn)*ic
    th0=np.clip(th0,math.log(p.F),math.log(p.cap))
    th,out=run(th0,p,record=(100,500,2000,5000,10000))
    k5=report(out[500][0]); k10=report(out[10000][0])
    A=out[10000][0]; Mk=MEAN[p.rot]
    c=np.array([[Mk[(a_-s)%8] for a_ in range(8)] for s in range(8)])
    a10=float((A*c).sum(1).mean()/max(A.sum(1).mean(),1e-9)); pm=float(A.sum(1).mean())
    A5=out[500][0]; a5=float((A5*c).sum(1).mean()/max(A5.sum(1).mean(),1e-9))
    A1=out[100][0]; a1=float((A1*c).sum(1).mean()/max(A1.sum(1).mean(),1e-9))
    best=(c.argmax(1)-np.arange(8))%8
    good=sum(c[s,A[s].argmax()]>=.85 for s in range(8))
    d,alt=spont(th,p)
    return name,id,dict(a100=a1,a500=a5,a10k=a10,pm=pm,good=int(good),dev=float(d),alt=float(alt))
if __name__=="__main__":
    names=list(C); jobs=[(n,i) for n in names for i in range(1,17)]
    with mp.Pool(14) as pool:
        R={}
        for n,i,r in pool.imap_unordered(job,jobs,chunksize=4):
            R.setdefault(n,{})[i]=r
            if len(R[n])==16:
                print(n,'done',flush=True)
                json.dump(R,open(OUT,'w'))
    json.dump(R,open(OUT,'w'))
