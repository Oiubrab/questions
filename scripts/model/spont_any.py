#!/usr/bin/env python3
# Weights-only spontaneity for a modular brain of ANY central depth (Phase R, R.5): the same definition as
# model.py's spont() (CATCH_RATE_ANALYSIS.md §3.4), with the central brain's row count read from the weights.
#   deviation: chance per bar that a vision unit leaves its slice's dominant path (mean over the 8 slices)
#   alt share: geometric mean of the best alternative's share at each step of that path
# Weights: results/model/w/<arm>_<id>.txt from export.q (stacked sight 3, touch 3, central R rows; 12 columns).
# Usage (repo root): python3 scripts/model/spont_any.py ARM [ARM ...]   (first arm is the paired reference)
import os, sys, glob, numpy as np
HERE = os.path.dirname(os.path.abspath(__file__))
W_DIR = os.path.join(os.environ.get('MODEL_OUT', os.path.join(HERE, '..', '..', 'results', 'model')), 'w')
NC, SENSE = 12, 3
DR = np.array([-1, -1, -1, 0, 0, 1, 1, 1]); DC = np.array([-1, 0, 1, -1, 1, -1, 0, 1])
BIAS = np.array([.5, .5, .5, 1, 1, 1.5, 1.8, 1.5])      # .dirs.vBias
ENTRY = 6                                                # label 7 (from above) -> incoming index 6

def shares(W, r, c, d, nr):
    """Route shares from cell (r,c) entered with incoming index d: bias x weight over in-grid or exit targets."""
    w = np.zeros(8)
    for o in range(8):
        tr, tc = r + DR[o], c + DC[o]
        if (0 <= tr < nr and 0 <= tc < NC) or (r == nr - 1 and tr == nr and 2 <= tc <= 9):
            w[o] = BIAS[o] * W[r, c, d, o]
    return w / w.sum()

def spont(stack):
    nr_c = stack.shape[0] - 2 * SENSE
    brains = [(stack[0:SENSE], SENSE), (stack[2 * SENSE:], nr_c)]
    res = []
    for s in range(8):
        b, r, c, d = 0, 0, 2 + s, ENTRY; prob = 1.; alts = []
        for _ in range(200):
            W, nr = brains[b]
            p = shares(W, r, c, d, nr); o = int(p.argmax()); q = np.sort(p)[::-1]
            prob *= q[0]; alts.append(q[1])
            tr, tc = r + DR[o], c + DC[o]
            if tr == nr:                                  # exit through the outputter at slot tc-2
                if b == 0: b, r, c, d = 1, 0, tc, ENTRY; continue
                break
            r, c, d = tr, tc, 7 - o                       # incoming label is the opposite of the move
        res.append((1 - prob, np.exp(np.mean(np.log(np.maximum(alts, 1e-12))))))
    return np.array(res).mean(0)

def arm(a):
    out = {}
    for f in sorted(glob.glob(os.path.join(W_DIR, a + '_*.txt'))):
        i = int(f.rsplit('_', 1)[1][:-4]); v = np.loadtxt(f, dtype=float)
        out[i] = spont(v.reshape(-1, NC, 8, 8))
    return out

def boot(x, rng=np.random.default_rng(1)):
    b = np.sort([rng.choice(x, len(x)).mean() for _ in range(10000)]); return b[250], b[9749]

if __name__ == '__main__':
    R = {a: arm(a) for a in sys.argv[1:]}; ref = sys.argv[1]
    print(f"{'arm':44}{'n':>3}{'dev med':>9}{'dev mean':>9}{'alt geo':>10}   paired vs {ref}: dev mean diff (CI); log10 alt diff (CI)")
    for a, r in R.items():
        ids = sorted(r); d = np.array([r[i][0] for i in ids]); al = np.array([r[i][1] for i in ids])
        line = f"{a:44}{len(ids):>3}{np.median(d):>9.3f}{d.mean():>9.3f}{np.exp(np.log(al).mean()):>10.2e}"
        if a != ref:
            c = [i for i in ids if i in R[ref]]
            dd = np.array([r[i][0] - R[ref][i][0] for i in c]); la = np.array([np.log10(r[i][1] / R[ref][i][1]) for i in c])
            lo, hi = boot(dd); l2, h2 = boot(la)
            line += f"   {dd.mean():+.3f} ({lo:+.3f}, {hi:+.3f}); {la.mean():+.2f} ({l2:+.2f}, {h2:+.2f})"
        print(line)
