#!/usr/bin/env python3
# Expected net log-gain per use of a route, by how far the action slot is from the slice (k = slot - slice).
# Direct-reward arithmetic from bar_loop.q; true angle uniform in the 45-degree slice.
import math
lam=.25
REIN=math.log(1.2/.95**12)*lam; PUN=-math.log(.8*.95**12)*lam
print(f"reinforce call {REIN:+.4f}  punish call {-PUN:+.4f}   floor->cap range {math.log(2e6/25):.2f}")
def direct(k,thr,mom=1,cb=1,epoch0=False,np_=1):
    n=4000; tot=0; pr=0
    for i in range(n):
        u=(i+.5)/n*45
        phi=math.radians(45*k-u)
        c=math.cos(phi)
        if c>thr:
            dm=.5+1.5*c
            calls=round(dm*2)+mom-1
            tot+=calls*REIN; pr+=1
        elif c<-.01: tot+=-cb*PUN
        elif not epoch0: tot+=-np_*PUN
    return tot/n,pr/n
S=.0238*20*1.0   # scope 20, mag 1: ln(1.1)*.25*20 = .4766
S=math.log(1.1)*lam*20
catch=math.log(1.5)*lam*0.08*20
print(f"strategic per use (scope 20, m 1) {S:.3f};  catch boost per use at 80/1k {catch:.3f}")
print("k: slot-slice; 0,+1 correct edges. Direct gain per use, P(rewarded), then net G with common mode")
for thr in (.6,.7,.85):
    for cb,e in ((1,'bad calls 1'),(3,'bad calls 3')):
        print(f"threshold {thr}, {e}")
        for k in (-3,-2,-1,0,1,2,3,4):
            d,p=direct(k,thr,cb=cb)
            print(f"  k={k:+d} meanCos {sum(math.cos(math.radians(45*k-(i+.5)/40*45)) for i in range(40))/40:+.3f}  direct {d:+.3f}  P(rew) {p:.2f}  G(today) {d+S+catch:+.3f}  G(strat 0) {d+catch:+.3f}")
delta=-math.log(.9875)/5
print(f"decay per bar {delta:.5f}")
for v,G in ((1/8,.43),(1/8,1.5)):
    print(f"p*=delta/(vG): v={v} G={G} -> {delta/(v*G):.4f}; W_alt next to cap at p*: {delta/(v*G)*2e6:.0f}")
