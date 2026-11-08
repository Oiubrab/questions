#!/usr/bin/env python3
# Task kinematics only: no brain. A cat follows a fixed lookup (slice -> move direction) on the wrapped field,
# with the rules of bar_loop.q: mouse steps 2 units every 10 bars, catch if the move path passes within 2 units,
# respawn 30-45 units from the cat. Gives catches per 1,000 bars and alignment for any lookup table.
import numpy as np, math
F=100.0
ANG=np.radians(45*np.arange(8))       # slot s moves at angle 45*s
MOV=np.stack([np.round(np.cos(ANG)),np.round(np.sin(ANG))],1)  # unit-grid moves (diagonals are length sqrt 2)
def tor(d): return d-F*np.floor(.5+d/F)
def run(table,bars=200000,rot=0.0,dist=1,seed=1,moverate=1.0,tiebreak=None):
    """table[s]=slot (or probability row). rot: slices rotated by rot degrees. dist: move multiplier."""
    rng=np.random.default_rng(seed)
    cat=rng.random(2)*F; a=rng.random()*2*math.pi; m=(cat+37.5*np.array([math.cos(a),math.sin(a)]))%F
    c=0;als=[];slc=np.zeros(8);alsl=np.zeros(8)
    u=rng.random(bars);v=rng.random(bars)
    for b in range(1,bars+1):
        if b%10==0:
            t=rng.random()*2*math.pi; m=(m+2*np.array([math.cos(t),math.sin(t)]))%F
        d=tor(m-cat); ang=(math.degrees(math.atan2(d[1],d[0]))-rot)%360
        s=int(ang//45)%8
        if u[b-1]>moverate: continue
        row=table[s]
        if np.ndim(row)==0: slot=int(row)
        else: slot=int(np.searchsorted(np.cumsum(row),v[b-1]))
        mv=MOV[slot]*5.0*dist
        al=(mv@d)/(np.linalg.norm(mv)*np.linalg.norm(d)+1e-12)
        als.append(al);slc[s]+=1;alsl[s]+=al
        L=mv@mv; tt=np.clip((d@mv)/L,0,1); near=np.linalg.norm(d-tt*mv)
        cat=(cat+mv)%F
        if near<2.0:
            c+=1; a=rng.random()*2*math.pi; r=30+15*rng.random(); m=(cat+r*np.array([math.cos(a),math.sin(a)]))%F
    return dict(c=1000*c/bars,a=float(np.mean(als)),occ=slc/slc.sum())
if __name__=="__main__":
    B=120000
    print("--- validation against baseline scripted cats (reactive: 126.5/1k, alignment 0.896)")
    r=run([(s+i%1) for s,i in zip(range(8),[0]*8)],bars=B)
    print(f"lookup slice s -> slot s (one edge):   c={r['c']:.1f} a={r['a']:.3f}")
    # reactive picks edge at random (u2)
    row=lambda s:np.eye(8)[s]*.5+np.eye(8)[(s+1)%8]*.5
    r=run([row(s) for s in range(8)],bars=B); print(f"lookup, either edge at random:         c={r['c']:.1f} a={r['a']:.3f}  (reactive measured 126.5, 0.896)")
    print("--- ceiling by slice rotation: slices centred on slots (rot=-22.5) vs today (rot=0)")
    print(f"closed form mean cos: today sin45/(pi/4)={math.sin(math.pi/4)/(math.pi/4):.4f}; centred sin22.5/(pi/8)={math.sin(math.pi/8)/(math.pi/8):.4f}")
    for rot in (0,-11.25,-22.5):
        r=run([s for s in range(8)],bars=B,rot=rot); print(f"rot {rot:7.2f}: identity lookup c={r['c']:.1f} a={r['a']:.3f}")
    print("--- speed: value-2 output (move 10 units)")
    for d,rot in ((1,0),(2,0),(1,-22.5),(2,-22.5)):
        r=run([s for s in range(8)],bars=B,rot=rot,dist=d); print(f"dist {d} rot {rot}: c={r['c']:.1f} a={r['a']:.3f}")
    print("--- wrong slices (today's geometry): w slices locked one slot off (k=-1), rest correct; dwell effect")
    rng=np.random.default_rng(3)
    for w in range(0,5):
        cs=[];aa=[];oc=[]
        for rep in range(6):
            idx=rng.choice(8,w,replace=False); t=[(s-1)%8 if s in idx else s for s in range(8)]
            r=run(t,bars=40000,seed=rep+10); cs.append(r['c']);aa.append(r['a']);oc.append(r['occ'][idx].sum() if w else 0)
        print(f"w={w}: c={np.mean(cs):6.1f} a={np.mean(aa):.3f}  naive a={((8-w)*.9003+w*.373)/8:.3f}  share of bars in wrong slices={np.mean(oc):.2f} (naive {w/8:.2f})")
    print("--- move rate (P(moving) per bar), identity lookup")
    for mr in (1,.95,.85,.7):
        r=run([s for s in range(8)],bars=60000,moverate=mr); print(f"moverate {mr}: c={r['c']:.1f} a={r['a']:.3f}")
