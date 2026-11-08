import json,numpy as np
RV=json.load(open('sweep4.json'))
M=lambda l:1.262*l-.269
print("Real angle-wiring G.4 (single brain): fresh t.6=516 (1 cens) lvl .673 | A->L180 trough -.77 t.6=1538 (13 cens) recovered .323 | L180->A trough -.15 t.6=839 (9 cens) recovered .463")
for v in ('noisy','nf'):
    print('\n',v)
    print(f"{'config':24s} | fresh lvl(map) | L180: min  t.6 cens lvl(map) | back A: min t.6 cens lvl(map)")
    for n,X in RV.items():
        x=X[v]; g=lambda k:[s[k] for s in x.values()]
        def tt(k,L):
            t=g(k+'_t'); return np.median([L if q is None else q for q in t]),sum(q is None for q in t),np.median([q for q in t if q is not None]) if any(q is not None for q in t) else float('nan')
        a=tt('A@0',10000); l=tt('L180@10000',5000); b=tt('A@15000',5000)
        print(f"{n:24s} | {np.mean(g('A@0_lvl')):.3f}({M(np.mean(g('A@0_lvl'))):.3f}) c{a[1]} | {np.mean(g('L180@10000_min')):+.2f} {l[0]:5.0f}/{l[2]:5.0f} c{l[1]:2d} {np.mean(g('L180@10000_lvl')):.3f}({M(np.mean(g('L180@10000_lvl'))):.3f}) | {np.mean(g('A@15000_min')):+.2f} {b[0]:5.0f}/{b[2]:5.0f} c{b[1]:2d} {np.mean(g('A@15000_lvl')):.3f}({M(np.mean(g('A@15000_lvl'))):.3f})")
