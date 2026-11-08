import math
# Memory requirement: to bridge H hidden bars a stored unit must survive per bar with s >= exp(-1/H) (1/e left at H)
for H in (5,20,50):
    print(f"hidden span {H:3d} bars: per-bar survival needed s>={math.exp(-1/H):.3f}  per-step {math.exp(-1/(12*H)):.4f}")
for s,lab in ((.49,'untrained'),(.063,'trained today')):
    print(f"{lab}: s={s}  memory time {-1/math.log(s):.2f} bars")
