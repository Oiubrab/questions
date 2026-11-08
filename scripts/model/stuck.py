# Baseline walled worlds: lengths of runs of consecutive blocked bars; moves while hidden. From world_log.csv only.
import csv,os,numpy as np
for w in ['pillars','wall','rooms','random','seethru']:
  for arm,lab in ((f'o_{w}_brain_20261103','brain'),(f'o_{w}_reactive_20261103','reactive')):
    bl=[];runs=[];vis=[];idle=0;mvh=[];N=0;c=0
    for s in range(1,17):
        p=f'results/{arm}/s{s}/world_log.csv'
        if not os.path.exists(p): continue
        L=list(csv.DictReader(open(p)))
        b=np.array([int(x['blocked']) for x in L]); m=np.array([int(x['moved']) for x in L]); v=np.array([int(x['visible']) for x in L]); c+=sum(int(x['caught']) for x in L)
        N+=len(L);bl.append(b.mean());vis.append(v.mean());mvh.append(m[v==0].mean() if (v==0).any() else np.nan)
        r=0
        for x in b:
            if x: r+=1
            else:
                if r: runs.append(r)
                r=0
        if r: runs.append(r)
    runs=np.array(runs) if runs else np.array([0])
    tot=runs.sum()
    print(f"{w:8} {lab:9} catches/1k {1000*c/N:6.1f} blocked {np.mean(bl):.2f} visible {np.mean(vis):.2f} moved|hidden {np.nanmean(mvh):.2f}  blocked runs: mean {runs.mean():6.1f} max {runs.max():5d}; share of blocked bars in runs>=20: {runs[runs>=20].sum()/max(tot,1):.2f}, >=100: {runs[runs>=100].sum()/max(tot,1):.2f}, >=500: {runs[runs>=500].sum()/max(tot,1):.2f}; share of all bars in runs>=100 {runs[runs>=100].sum()/N:.2f}")
