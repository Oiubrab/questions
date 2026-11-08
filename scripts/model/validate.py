import sys,numpy as np,multiprocessing as mp
sys.path.insert(0,sys.argv[2]) if False else None
exec(open(sys.argv[2]).read())
def job(id):
    th0=to_th(load("o_untrained_20261103",id))
    p=defaults()
    th,out=run(th0,p)
    return id,{b:(report(v[0]).tolist(),v[1],v[2]) for b,v in out.items()}
if __name__=="__main__":
    with mp.Pool(16) as pool: R=pool.map(job,range(1,17))
    bs=sorted(R[0][1])
    print("mean-field, today's constants, seeds 1-16 from their real initial weights")
    print("bar  nCorrect  E[cos]  pMove")
    for b in bs:
        print(b, round(np.mean([sum(x in (0,1) for x in r[1][b][0]) for r in R]),2), round(np.mean([r[1][b][2] for r in R]),3), round(np.mean([r[1][b][1] for r in R]),3))
    print("per-seed k at bar 10000:")
    for id,r in R: print(id,r[10000][0])
