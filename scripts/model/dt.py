import os; exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'model.py')).read())
np.set_printoptions(precision=3,suppress=True)
for thr in (.7,.75,.8,.85):
    for n in (1,3,4):
        D,Pr,M=direct(0.,thr,n,n,.25,n=720)
        print(f"thr {thr} calls {n}: D(k=0,+1)={D[0]:+.3f} {D[1]:+.3f}  P(rewarded in correct slot)={Pr[0]:.3f}  D(k=-1,+2)={D[7]:+.3f} {D[2]:+.3f}  D(away)={D[4]:+.3f}")
