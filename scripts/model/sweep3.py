import sys,numpy as np,multiprocessing as mp,json,math
exec(open(sys.argv[2]).read())
OUT=sys.argv[3]
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
S=sa(ss=.03); SCN=sa(ss=.03,cs=.5,np_e0=1)
P3=sa(np=3,bp=3,np_e0=1); P4=sa(np=4,bp=4,np_e0=1); P6=sa(np=6,bp=6,np_e0=1)
C={'base':sa(),'S':S,'str.1':sa(ss=.1),'catch.5':sa(cs=.5),'neutral1':sa(np_e0=1),'bad3':sa(bp=3),
 'P3':P3,'P4':P4,'P6':P6,'P4+catch.5':comb(P4,sa(cs=.5)),
 'gate10':sa(gate_n=10),'gate15':sa(gate_n=15),'gate20':sa(gate_n=20),'gate25':sa(gate_n=25),
 'gate20+neutral1':sa(gate_n=20,np_e0=1),'gate20+P3':comb(P3,sa(gate_n=20)),
 'S+floor250':comb(S,sa(F=250.)),'S+decay.5':comb(S,decay(.5)),'S+cap2e5':comb(S,sa(cap=2e5)),
 'SCN':SCN,'SCN+floor250+decay.5':comb(SCN,sa(F=250.),decay(.5)),
 'str.1+catch.5+neutral1':sa(ss=.1,cs=.5,np_e0=1),
 'P4+floor250+decay.5':comb(P4,sa(F=250.),decay(.5)),
 'S+rot+thr.92':comb(S,sa(rot=-22.5,thr=.92)),'P4+rot+thr.92':comb(P4,sa(rot=-22.5,thr=.92)),
 'P4+init.25':comb(P4,sa(initc=.25))}
REL=[n for n in C if n not in ('catch.5','bad3','str.1','gate10','S+cap2e5','P6')]
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
    return name,id,rep,dict(a100=f(100),a500=f(500),a2k=f(2000),a10k=f(10000),pm=float(A.sum(1).mean()),good=int(sum(c[s,A[s].argmax()]>=.85 for s in range(8))),bad=int(sum(c[s,A[s].argmax()]<.5 for s in range(8))),dev=float(d),alt=float(alt),A=A.round(5).tolist(),rot=p.rot)
UNI6=(.6+.269)/1.262   # model metric equal to real alignment 0.6, from the fit to the 16 saved brains
def legs(name,id,sched,tag):
    p,th0=setup(name,id); p.bars=sched[-1][1]; p.sched=[(a,w) for a,b,w in [(x[0],x[1],x[2]) for x in sched[1:]]]
    th,out=run(th0,p,record=(p.bars,),seed=7000+id)
    tr=np.array(p.trace); r={}
    for t0,t1,w in [(x[0],x[1],x[2]) for x in sched]:
        k=f"{w}@{t0}"; q=tr[(tr[:,0]>=t0)&(tr[:,0]<t1)]
        hit=q[q[:,1]>=UNI6]
        r[k+'_t']=(float(hit[0,0]-t0) if len(hit) else None); r[k+'_lvl']=float(q[-20:,1].mean()); r[k+'_min']=float(q[:,1].min()); r[k+'_len']=t1-t0
    return name,id,tag,r
def rel(a): return legs(a[0],a[1],[(0,3000,'A'),(3000,6000,'L180'),(6000,9000,'A')],'short')
def relv(a): return legs(a[0],a[1],[(0,10000,'A'),(10000,15000,'L180'),(15000,20000,'A')],'long')
if __name__=="__main__":
    jobs=[(n,i,r) for n in C for i in range(1,17) for r in (0,1,2)]
    R={}
    with mp.Pool(15) as pool:
        for n,i,r,x in pool.imap_unordered(job,jobs,chunksize=4):
            R.setdefault(n,{}).setdefault(str(r),{})[str(i)]=x
        json.dump(R,open(OUT,'w')); print('levers done',flush=True)
        RL={}
        for n,i,tag,x in pool.imap_unordered(rel,[(n,i) for n in REL for i in range(1,17)],chunksize=2):
            RL.setdefault(n,{})[str(i)]=x
        json.dump(RL,open(OUT+'.rel.json','w')); print('relearn done',flush=True)
        RV={}
        for n,i,tag,x in pool.imap_unordered(relv,[(n,i) for n in ('base','S','P4','gate20','SCN') for i in range(1,17)],chunksize=2):
            RV.setdefault(n,{})[str(i)]=x
        json.dump(RV,open(OUT+'.relv.json','w')); print('long relearn done',flush=True)
