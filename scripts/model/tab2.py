import json,statistics as st
R=json.load(open('sweep2.json'))
RL=json.load(open('sweep2.json.rel.json'))
def m(xs): 
    xs=[x for x in xs if x is not None]
    return sum(xs)/len(xs) if xs else float('nan')
rows=[]
for n,reps in R.items():
    d={}
    for rep,seeds in reps.items():
        for k in ('a100','a500','a2k','a10k','pm','good','bad','dev','alt'):
            d[(rep,k)]=m([s.get(k) for s in seeds.values()])
        vals=[s['a10k'] for s in seeds.values()]
        d[(rep,'n7')]=sum(v>=0.7 for v in vals)/len(vals)
    rows.append((n,d,list(reps.keys())))
print("reps",rows[0][2])
print(f"{'name':34s} {'nf.a10k':>7s} {'nf.good':>7s} {'a10k':>6s} {'a500':>6s} {'good':>5s} {'>=.7':>5s} {'pm':>5s} {'dev':>6s} {'alt':>9s}")
base=[r for r in rows if r[0]=='base'][0][1]
for n,d,reps in rows:
    nr=[r for r in reps if r!='0']
    def g(k):
        return m([d[(r,k)] for r in nr]) if nr else float('nan')
    nf=d.get(('0','a10k'),float('nan')); nfg=d.get(('0','good'),float('nan'))
    print(f"{n:34s} {nf:7.3f} {nfg:7.2f} {g('a10k'):6.3f} {g('a500'):6.3f} {g('good'):5.2f} {g('n7'):5.2f} {g('pm'):5.2f} {g('dev'):6.3f} {g('alt'):9.2e}")
print()
print("RELEARN")
keys=sorted({k for v in RL.values() for s in v.values() for k in s})
print(keys)
print(f"{'name':34s} {'f_lvl':>6s} {'f_t':>6s} {'L_min':>6s} {'L_t':>6s} {'L_lvl':>6s} {'bA_min':>6s} {'bA_t':>6s} {'bA_lvl':>6s}  L_rec")
for n,v in RL.items():
    def g(k): return m([s.get(k) for s in v.values()])
    rec=sum(1 for s in v.values() if s.get('L180_t') is not None)
    print(f"{n:34s} {g('fresh_lvl'):6.3f} {g('fresh_t'):6.0f} {g('L180_min'):6.3f} {g('L180_t'):6.0f} {g('L180_lvl'):6.3f} {g('backA_min'):6.3f} {g('backA_t'):6.0f} {g('backA_lvl'):6.3f}  {rec}/{len(v)}")
