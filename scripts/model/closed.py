import sys,math,numpy as np
M=sys.argv[1]; sys.argv=[sys.argv[0],M]
g={}; exec(open(M+'/model.py').read(),g)
to,ex,mask,ENT,NS,spont,defaults,direct=g['to'],g['ex'],g['mask'],g['ENT'],g['NS'],g['spont'],g['defaults'],g['direct']
BIAS=g['BIAS0']
lam=.25; R=math.log(1.2/.95**12)*lam; P=-math.log(.8*.95**12)*lam
dec=-math.log(.9875)/5
print(f"one reinforce call +{R:.3f}, one punish call -{P:.3f}, decay {dec:.5f}/bar, range ln(cap/floor) {math.log(2e6/25):.2f}")
# Common mode
Sg=math.log(1.1)*lam*20; 
print("\nCommon-mode gain per use, whatever the move's quality:")
print(f"  strategic: ln(1.1)*lam*scope*mag = {Sg:.3f} at scope 20 mag 1; {math.log(1.1)*lam*80*2:.2f} at scope 80 mag 2")
for c in (40,80,120): print(f"  catch boost at {c}/1k: ln(1.5)*lam*20*rate = {math.log(1.5)*lam*20*c/1000:.3f}")
print("  Hebb ln(1+h/W), h=250:", {W:round(math.log(1+250/W),4) for W in (25,100,500,1000,1e4,1e5,2e6)})
print("\nBreak-even: a route whose downstream outcome is good a fraction q of uses, direct only: q* = P/(P+Rg)")
D,Pr,Mk=direct(0.,.7,1,1,lam); print('  D(k) today, thr .7:',np.round(D,3).tolist(),' mean cos',np.round(Mk,3).tolist())
Rg=D[0]; print(f"  q* = {P/(P+Rg):.3f}; with common mode C the route is positive for any q once C > P: C today = {Sg+math.log(1.5)*lam*20*.08:.3f} vs P = {P:.3f}")
print("\nBistable threshold p* = delta/(v G):")
for G in (.1,.43,.9,1.4): print(f"  G={G}: v=1/8 -> p*={dec/(G/8):.4f}  W_alt next to a capped route: {dec/(G/8)*2e6:,.0f}")
print("\nUses to reach the cap from W0: n = ln(cap/W0)/G");
for W0 in (25,512,1000): print(f"  W0={W0}: G=.9 -> {math.log(2e6/W0)/.9:.1f} uses; G=1.4 -> {math.log(2e6/W0)/1.4:.1f} uses (bars at v=1/8: x8)")
print("\nRelearning, once the dominant route has G<0 (per-use fall |G|, unit share ~1, state visit v=1/8):")
print("  T_fall = ln(cap/W_eq)/(v|G|);  used-route floor W_eq=h/|G| (Hebb balances punishment);  T_disc = (Pi+7)/v, Pi=h/(|G| F): alternatives at floor F next to W_eq")
for h,G in ((1000,P),(250,P),(100,P),(250,2*P),(250,3*P)):
    Weq=h/G; Pi=h/(G*25); Tf=math.log(2e6/Weq)/(G/8); Td=(Pi+7)*8
    print(f"  h={h:5d} |G|={G:.2f}: W_eq={Weq:7.0f}  Pi={Pi:6.1f}  T_fall={Tf:6.0f} bars, T_disc={Td:6.0f} bars (one chosen alternative; 7 candidates: /7)  T_fall+T_disc/2={Tf+Td/2:6.0f}")
print("  measured G.4: half the loss back in 150-300 bars; 0.6 regained in 1,000-2,000; h=250 at least as fast as h=1000")
print("\nSpontaneity: chance per use of a floor route next to a capped dominant one = F*b/(cap*b_max)")
for R_,F in ((80000,25),(8000,25),(800,25)):
    print(f"  cap/floor {R_}: floor-route share {1/R_:.2e}; bars between tries of one given floor route at v=1/8: {8*R_:,.0f}")
print("  next to a dominant route at the used floor W_eq=1190: share 25/1190 = 2.1%, 380 bars")
# Saved baseline brains: dominant paths
d=[];a=[]
for id in range(1,17):
    f=np.loadtxt(f"{M}/w/o_open_brain_20261103_{id}.txt").reshape(9,12,8,8)
    th=np.log(np.stack([f[0:3].reshape(NS,8),f[6:9].reshape(NS,8)]))
    x,y=spont(th,defaults()); d.append(x);a.append(y)
print(f"\nSaved baseline brains: chance per bar of leaving a slice's dominant path {np.mean(d):.3f} (range {min(d):.3f}-{max(d):.3f}); geomean best-alternative share on those paths {np.exp(np.mean(np.log(a))):.5f}")
print("Bias: flat lateral arithmetic"); b=BIAS/BIAS.sum(); dr=g['DR'];dc=g['DC']
print(f"  drift {(b*dr).sum():.3f} rows/step; lateral var {(b*dc*dc).sum():.3f}; P(straight S)={b[6]:.3f} P(SE)={b[7]:.3f} P(SW)={b[5]:.3f} P(E)={b[4]:.3f} P(W)={b[3]:.3f} P(up)={b[:3].sum():.3f}")
# Untrained prior by k
Ak=[];pe=[]
for id in range(1,17):
    f=np.loadtxt(f"{M}/w/o_untrained_20261103_{id}.txt").reshape(9,12,8,8)
    th=np.log(np.stack([f[0:3].reshape(NS,8),f[6:9].reshape(NS,8)]))
    P_=[]
    for bb in (0,1):
        y=th[bb]+np.log(BIAS); w=np.exp(y-y.max(1,keepdims=True))*mask; P_.append(w/np.maximum(w.sum(1,keepdims=True),1e-300))
    ps,pc=P_
    for K in (12,):
        A=np.zeros((8,8));
        for s in range(8):
            xs=np.zeros(NS);xs[ENT[s]]=1;xc=np.zeros(NS)
            for i in range(60):
                ms=xs[:,None]*ps; nxs=np.bincount(to[to>=0],weights=ms[to>=0],minlength=NS); e=np.bincount(ex[ex>=0],weights=ms[ex>=0],minlength=8)
                xc=xc.copy(); xc[ENT]+=e
                mc=xc[:,None]*pc; nxc=np.bincount(to[to>=0],weights=mc[to>=0],minlength=NS); A[s]+=np.bincount(ex[ex>=0],weights=mc[ex>=0],minlength=8)
                xs,xc=nxs,nxc
        Ak.append(A)
Ak=np.array(Ak).mean(0); Ak=Ak/Ak.sum(1,keepdims=True)
pk=np.zeros(8)
for s in range(8):
    for a_ in range(8): pk[((a_-s+4)%8)]+=Ak[s,a_]/8
print("Untrained prior over k=slot-slice (k=-4..+3), units eventually exiting:",dict(zip(range(-4,4),np.round(pk,3).tolist())))
print(f"  today correct k in (0,1): {pk[4]+pk[5]:.3f}; neighbours k=-1,+2 (cos .37): {pk[3]+pk[6]:.3f}")
print(f"  slices centred on slots: best k=0: {pk[4]:.3f}; k=+-1 (cos .72): {pk[3]+pk[5]:.3f}")
print(f"  E[cos] prior today {sum(pk[i]*direct(0.,.7,1,1,lam)[2][(i-4)%8] for i in range(8)):.3f}; centred {sum(pk[i]*direct(-22.5,.7,1,1,lam)[2][(i-4)%8] for i in range(8)):.3f}")
for r_ in (0.,-22.5):
    D,Pr,Mk=direct(r_,.7,1,1,lam); print(f"rot {r_}: k  0..7 meanCos {np.round(Mk,3).tolist()}"); 
    for thr in (.7,.85,.92):
        D,Pr,_=direct(r_,thr,1,1,lam); print(f"   thr {thr}: D {np.round(D,3).tolist()}  P(rew) {np.round(Pr,2).tolist()}")
print("\nMemory: conservation budget. Inflow = 1 vision unit per visible bar; each move consumes >=1 unit. To store M units for hidden bars, inflow must exceed outflow.")
for H in (5,20,50): print(f"  bridge H={H}: per-bar survival s>={math.exp(-1/H):.3f}, and H units stored if one unit drives each hidden move")
