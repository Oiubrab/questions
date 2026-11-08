import math
R=math.log(1.2/.95**12)*.25; P=-math.log(.8*.95**12)*.25; dec=-math.log(.9875)/5; L=math.log(2e6/25)
print(f"reinforce call R={R:.4f}, punish call P={P:.4f}, decay/bar={dec:.5f}, log range={L:.2f}")
strat=lambda s=1,sc=20,m=1:s*sc*m*math.log(1.1)*.25
catch=lambda c,cs=1,w=20:cs*w*c*math.log(1.5)*.25
print("-- separation: a wrong route at high W falls only if n*P > C. C by (strategic scale, catch scale, catches/1k)")
for s,cs,c in [(1,1,.08),(1,1,.12),(.1,1,.08),(.03,1,.08),(.03,.5,.08),(.03,.5,.12),(.1,.5,.12),(1,.5,.12)]:
    C=strat(s)+catch(c,cs); print(f" s={s:<5} cs={cs:<4} c={c*1000:.0f}/1k: C={C:.3f}; calls needed>{C/P:.2f}")
print("-- sustained share p*=dec/(v*G), v=1 use per bar")
for G in (.43,.1,.05,.9,1.4): print(f" G={G}: p*={dec/G:.4f}")
print("-- trap: route used every bar, no reward. W_eq=h/dec; escape T=(1/d)ln(1+d*R/(7n)) while decaying from cap")
for h in (100,250,1000): print(f" h={h}: W_eq={h/dec:,.0f}")
def T(R_,d=dec,n=6):
    return math.log(1+d*R_/(7*n))/d
for name,R_,d in [("today R=80000",80000,dec),("floor 100",20000,dec),("floor 250",8000,dec),("cap 2e7",800000,dec),("cap 2e5",8000,dec),("decay x2",80000,2*dec),("decay x.5",80000,.5*dec),("floor250+decay.5",8000,.5*dec)]:
    print(f" {name:18s} R={R_:>7} d={d:.5f}: T_esc={T(R_,d):6.0f} bars; share of floor route next to cap 1/R={1/R_:.2e}")
print("-- relearn fall time from cap T_fall=ln(cap/W_eq)/(v|G|), W_eq=h/|G|")
for h,G in [(250,.21),(250,.42),(250,.63),(250,.84),(100,.21),(1000,.21)]:
    print(f" h={h} |G|={G}: W_eq={h/G:,.0f} T_fall={math.log(2e6/(h/G))/G:5.1f} uses")
print("-- memory, Little's law: stock N=inflow*residence; survival s gives residence 1/(1-s)")
for H in (5,20,50):
    s=math.exp(-1/H); print(f" hidden {H} bars: s>={s:.3f}; stock ~{1/(1-s):.1f} units, of 72 per 3x12 brain at HIGH")
for s in (.063,.49,.402): print(f" s={s}: residence {1/(1-s):.2f} bars; bridges H~{-1/math.log(s):.2f} bars at 1/e")
