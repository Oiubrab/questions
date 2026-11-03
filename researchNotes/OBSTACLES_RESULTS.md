# Obstacles plan: results, and what to keep

**What this is.** The write-up of the [obstacles plan](../plans/completed/OBSTACLES_PLAN.md) (2026-10-29 to 2026-11-03): every element that was added or tested, what it did in every world, and a verdict on whether to keep it, with the context in which a weak result might still matter. Nothing is removed from the code until the user approves §7.

**How to read the numbers.**
- **Seeds and pairing:** 16 seeds per arm, paired by seed (the same seed gives the same starting weights and the same world in every arm).
- **Catches** are per 1,000 bars unless a table says "per run" (one run = 10,000 bars).
- **Significance:** a difference is called **significant** only when its 95% bootstrap confidence interval excludes zero; otherwise it is "no detectable difference" (ns), never a win or a loss.
- **Alignment** is the straight-line cosine to the mouse, through walls, so with walls it under-rates good detours. Catches are the primary measure there.
- **Raw outputs:** `results/obs_report_20261103.txt` (the close-out batch), `results/lesion_o_open_brain_20261103/report.txt`, and the earlier reports named in the plan's §6.

---

## 1. The answer on one page

**The north star: "the cat heads towards the mouse, then finds it when it can't see it".**
- **Heading towards a visible mouse: partly.** In the open world the brain reaches 54% of the way from a random cat to the shortest-path planner, against 88% for the scripted reactive cat. Its moves point at the mouse with alignment about 0.65, where a perfect chaser scores 0.97.
- **Finding it when hidden: no.** The brain has **no memory**, and **learning makes that worse**: training shortens how long energy stays in the brain, from about 15 steps in an untrained brain to about 8 (§4.4). Decoding where the mouse was last seen from the brain's state is at chance in every condition tested.

**What worked:**
- **Touch:** whiskers plus the blocked sense, in their own brain. Removing touch costs **47–89% of catches in every walled world** (5 of 5, all significant).
- **The modular brain** (sight, touch and reward brains feeding a central brain). It ties the single brain in open and blinking worlds and **beats it by 29–83% in every walled world** (5 of 5, all significant). See-through pillars is the one world where the brain beats the reactive cat (14 of 16 seeds).
- **The reward brain's energy reaching the central brain** (the satiety meta-brain). Without it the cat **stops moving when it can't see**: −75% catches in pillars, −85% in rooms.
- **No direct reward while the mouse is hidden** (the default): never significantly worse than rewarding the true direction, and better in rooms.

**What failed:**
- **Spontaneous activity at P = 0.05:** −62 to −93% in every world. It drowns the vision signal.
- **The efference copy:** −66% in the open world.
- **A deeper central brain:** 6 rows is significantly worse than 3.
- **Pretraining on layouts before a new layout:** negative transfer. A fresh brain beats pretrained ones by 28–42% on held-out layouts.
- **The curriculum** (open, then pillars, then rooms): no detectable effect.

**Where the brain stands.** Every scripted cat is a fixed rule, not a learner:
- **reactive:** heads for the mouse while it's visible and moves at random when it's not; this is the best memoryless rule.
- **persist:** like reactive, but keeps its last heading while the mouse is hidden.
- **greedy:** always heads straight for the mouse, even through walls.
- **planner:** takes the shortest path around walls.

Score: 0 = a random cat, 1 = the planner.

| World | Modular brain | Single brain | Reactive | Persist | Greedy | Brain beats reactive (seeds of 16) |
|---|---|---|---|---|---|---|
| Open | 0.54 | 0.58 | **0.88** | 0.88 | 1.01 | 0 |
| Blink 20:5 (hidden 20% of bars) | 0.44 | 0.49 | 0.68 | **0.69** | 1.01 | 0 |
| Blink 20:20 (hidden 50%) | 0.29 | 0.28 | 0.40 | **0.44** | 1.01 | 0 |
| Blink 20:50 (hidden 71%) | 0.16 | 0.17 | **0.22** | 0.22 | 1.01 | 1 |
| See-through pillars | **0.13** | 0.04 | 0.05 | 0.05 | 0.01 | **14** |
| Pillars | 0.06 | 0.04 | **0.63** | 0.01 | 0.01 | 0 |
| Wall with doors | 0.11 | 0.02 | **0.53** | 0.01 | 0.00 | 0 |
| Rooms | 0.01 | −0.01 | **0.15** | −0.01 | 0.00 | 0 |
| Random layouts | 0.07 | 0.02 | **0.64** | 0.00 | 0.01 | 0 |

Greedy scores about 1 in open worlds because it knows where the mouse is even when it's hidden; it scores about 0 with walls because it pushes into them.

**The three things holding the brain back**, from the evidence in §4:
1. **No memory.** The brain empties within one bar, so a hidden mouse is forgotten.
2. **It gets stuck where it can't see.** In walled worlds it sees the mouse on only 7–26% of bars, against 17–73% for the reactive cat, and 41–83% of its moves are blocked.
3. **Its input can't tell things apart.** The dog arrives in the same vision cells as the mouse, so the brain chases both: it touches the dog 2.7 times as often as the scripted cats do.

---

## 2. The verdicts

Five verdicts:
- **Keep (default):** it helps where it matters and costs nothing meaningful elsewhere.
- **Keep as an option:** it helps in some worlds and hurts in others; the flag stays and the context is recorded.
- **Not proven:** no detectable difference anywhere; it stays as it is, with a note on what would decide it.
- **Not at this setting:** this value is harmful, but the idea isn't refuted.
- **Remove:** harmful everywhere it was tested, with no plausible context in which it helps.

| # | Element | Verdict | The evidence in one line | When it might matter differently |
|---|---|---|---|---|
| 1 | **Modular brain** (sight + touch + reward → central) | **Keep (default)** | Ties the single brain without walls (ns in 4 of 4 open/blink worlds); beats it by 29–83% with walls (significant in 5 of 5) | — |
| 2 | **Central brain depth 3 rows** | **Keep 3** | 4 rows ns worse, 6 rows significantly worse (alignment −0.084, catches −19%) in the open world | Deeper brains hold energy longer (6 rows: 30% still inside after a bar, against 6%). If memory work needs carry-over, depth is a lever to re-test |
| 3 | **Touch brain** (whiskers + blocked sense) | **Keep (default)** | Removing it costs 47–89% of catches in 5 of 5 walled worlds (all significant); it is silent without walls, so it costs nothing there | — |
| 4 | **Reward-brain energy into the central brain's right column** | **Keep (default)**; keep `top` as an option | Ablating the reward brain: pillars −75%, rooms −85% (significant); blink ns. Without walls it costs a little alignment (+0.057 when ablated, revival §6). `top` doubles pillars catches but costs visible alignment (−0.12 in blink); `none` is ruinous with walls | In walled worlds, `top` (entering beside sight) is the better entry; the right edge is the best compromise across worlds |
| 5 | **No direct reward while hidden** (default) | **Keep (default)** | Rewarding the true direction while hidden: blink ns (but alignment −0.11, significant), pillars ns (+22%), rooms −38% (significant, borderline) | A geodesic "smell gradient" reward (the plan's option b) is untested and might do better than either |
| 6 | **Satiety scale edges** (0.5, 2, 5, 10) | **Not proven; keep default** | Alternative edges (1, 4, 10, 20): open ns (+10%), pillars ns (+15%) | The scale is used very differently by world: mean cell 3.7 of 4 in the open world (nearly always "full", so the reward brain's input hardly varies), about 1 in walled worlds, 0.26 in rooms. Re-test once the cat hunts well with walls |
| 7 | **Spontaneous activity, P = 0.05** (`--spontaneous`) | **Not at this setting. Removed 2026-11-03** (the user's decision); replaced by a sparse clock input, [plans/future/METRONOME_PLAN.md](../plans/future/METRONOME_PLAN.md) | −93% blink, −68% pillars, −62% rooms (all significant); visible alignment collapses from 0.44–0.71 to about 0.02 | The user's principle (a brain always receives input) stands. At P = 0.05 it raises the brain's mean energy from about 0.2 to 2.3 units, against vision's single unit a bar, so noise swamps the signal. The user's follow-up (2026-11-03): internal input should be a clock, not random: very sparse, at one fixed spot, never close to overpowering real input. That is the metronome plan |
| 8 | **Efference copy** (`--efference`) | **Remove from the candidates** (off, as now; the flag can stay as an ablation) | Pilot: open −66% (85 → 28), blink −58%; see-through pillars 7.2 → 8.8, pillars 6.2 → 4.9 (no confidence intervals in the pilot) | It entered where vision enters and made the cat repeat itself. A motor copy might help path integration once the brain can hold state; then it would need its own input, not vision's |
| 9 | **Training on many layouts** (`--layout-every`) | **Not proven** (a tool; keep the flag) | On held-out layouts, many layouts vs one: frozen ns (+24%), still learning ns (+7%) | Worth re-testing once learning transfers at all (see #10) |
| 10 | **Pretraining before a new layout** | A finding, not an element: **negative transfer** | Fresh learning for 5k bars on the held-out layouts beats every pretrained brain: −28% (ns) to −42% (significant). Pretrained brains still beat untrained ones (+43%, ns) | Consistent with the revival plan's re-learning results: strongly reinforced routes are slow to unlearn. Bears on the weight-cap ratio (G.R4) and decay |
| 11 | **Curriculum** (open 5k → pillars 5k → rooms 10k) | **Not proven** (don't adopt; the flags it uses stay) | Rooms catches −10% against rooms from scratch (ns) | Same as #9: until learning transfers, staging can't help |
| 12 | **Hidden-mouse rung settings** (`--blink`, `--see-through`) | Tools; keep | — | — |
| 13 | **Scripted cats and the planner** | Tools; keep | The planner reaches 117–144 catches in every world: the ceiling | — |

**What the user asked to decide.** The table implies **no change to any default**. Every default element is a "keep". The only "remove" candidate, the efference copy, is already off; removing its code is optional (§7).

---

## 3. Results by world

Catches per 1,000 bars, 16 seeds × 10,000 bars. Significance is paired against the modular brain (for the brain variants) and stated where it matters.

| World | Modular brain | Single brain | No touch | Reward brain ablated | Spontaneous 0.05 | Reward while hidden | Reactive | Persist | Greedy | Planner | Random |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Open | 78.5 | 84.6 (ns) | — | — | — | — | 126.5 | 126.5 | 146.3 | 144.2 | 1.6 |
| Blink 20:5 | 64.3 | 71.8 (ns) | — | — | — | — | 98.2 | 99.3 | 145.9 | 144.2 | 1.6 |
| Blink 20:20 | 42.7 | 42.0 (ns) | — | 45.1 (ns) | **2.8** | 41.1 (ns) | 59.0 | 64.8 | 145.9 | 144.3 | 1.5 |
| Blink 20:50 | 25.2 | 26.2 (ns) | — | — | — | — | 32.9 | 32.4 | 146.2 | 144.4 | 1.7 |
| See-through pillars | 19.5 | **7.2** | **4.2** | — | — | — | 7.7 | 7.7 | 3.2 | 141.6 | 1.2 |
| Pillars | 9.2 | **6.5** | **4.8** | **2.3** | **3.0** | 11.3 (ns) | 89.4 | 3.2 | 3.0 | 141.1 | 1.3 |
| Wall with doors | 16.5 | **4.3** | **3.2** | — | — | — | 73.7 | 2.8 | 2.0 | 137.6 | 1.4 |
| Rooms | 1.76 | **0.30** | **0.19** | **0.26** | **0.66** | **1.09** | 19.6 | 0.32 | 0.41 | 127.4 | 0.96 |
| Random layouts | 9.2 | **3.6** | **3.1** | — | — | — | 76.1 | 2.0 | 3.1 | 117.5 | 1.5 |

Bold = significantly different from the modular brain. "—" = not run in that world.

**Paired differences behind the bold cells** (arm minus modular brain, catches per run, 95% CI):

| Comparison | See-through | Pillars | Wall | Rooms | Random layouts |
|---|---|---|---|---|---|
| Single brain | −124 (−185 to −69), −63% | −27 (−53 to −2), −29% | −122 (−188 to −69), −74% | −14.6 (−18.6 to −10.4), −83% | −55 (−78 to −32), −60% |
| No touch | −154 (−210 to −102), −79% | −44 (−77 to −13), −47% | −134 (−199 to −79), −81% | −15.7 (−19.7 to −11.8), −89% | −61 (−83 to −38), −66% |

| Comparison | Blink 20:20 | Pillars | Rooms |
|---|---|---|---|
| Reward brain ablated | +24 (−33 to +76), ns | −69 (−98 to −42), −75% | −15.0 (−19.4 to −10.6), −85% |
| Reward while hidden (cosine) | −16 (−80 to +47), ns | +21 (−27 to +83), ns | −6.6 (−12.2 to −0.06), −38% |
| Spontaneous 0.05 | −399 (−446 to −356), −93% | −63 (−94 to −34), −68% | −10.9 (−14.8 to −7.3), −62% |

**Harder goals** (catches per 1,000 bars; the brain paired against the same world without the extra difficulty):

| Goal | Brain | Reactive | Greedy | Planner | Brain vs the plain world |
|---|---|---|---|---|---|
| Three mice, open | 101.2 | 165.8 | 194.5 | 192.5 | +29% (CI +61 to +405 per run) |
| Three mice, pillars | 52.8 | 155.6 | 77.3 | 183.1 | +473% (CI +366 to +506 per run) |
| Dog, open | 25.9 | 126.5 | 145.9 | 144.0 | **−67%** (CI −644 to −404 per run) |
| Dog, pillars | 10.9 | 87.9 | 3.2 | 140.9 | +18% (ns) |
| Evasive mouse, open (rerun after the fix, §6) | 62.8 | 101.1 | 125.7 | 125.2 | −20% (CI −297 to −2 per run) |

- **More mice help the brain more than anything else**, because a mouse is nearly always in view (pillars: visible 76% of bars, against 18% with one mouse). It still trails every scripted cat except random.
- **A fleeing mouse costs every cat about the same** (brain −20%, reactive −20%, greedy −14%), so evasion doesn't change where the brain stands.
- **The dog confuses the brain.** It touches the dog 6.0 times per 1,000 bars, against 2.2 for the scripted cats (which ignore it), and its visible alignment falls from 0.65 to 0.26. The dog enters the same vision cells as the mouse, only with a different label, and the brain doesn't separate them. This is the vision plan's input problem in its first real form.

---

## 4. Why it behaves this way

### 4.1 Visible mouse: it chases, imprecisely
- **Alignment while visible is 0.65–0.73 without walls and 0.44–0.61 with walls**, against 0.90 for the reactive cat (which heads along one edge of the seen slice). The brain's choice of output slot is probabilistic, and learning sharpens it only so far.
- **Ties with the single brain** in open and blink worlds: 3 of the 9 stacked rows (the touch brain) are silent there, so the modular layout costs nothing.

### 4.2 Hidden mouse, no walls: it stops
- **The modular cat moves on 2–3% of hidden bars** in the blink worlds; the single brain moves on 22–41%. With no vision input, the only energy left is the reward brain's, which reaches 2 rows of the 3-row central brain (5 rows in the single brain).
- **Its hidden moves point nowhere useful:** alignment −0.38 to +0.10, depending on bars since last seen.
- **A one-slot memory would pay,** because persist beats reactive in blink 20:20 (64.8 vs 59.0). The brain has no such memory.

### 4.3 Walls: it gets stuck where it can't see the mouse
- **It sees the mouse far less than reactive:** pillars 18% vs 73% of bars, wall 26% vs 61%, rooms 7% vs 17%, random layouts 19% vs 62%. It ends up behind obstacles and stays there.
- **Blocked moves:** 41–83% of its moves, against 3–12% for reactive and 97–99% for greedy and persist, in the worlds where walls hide the mouse. (With see-through pillars reactive is trapped too, at 94%, because the mouse never disappears.) Touch brought this down from the pilot's 78–97% (O.1).
- **Touch gives it input when blind:** with walls it moves on 47–85% of hidden bars (the whiskers keep injecting), against 20–22% without touch.
- **Why reactive wins:** it moves at random when it loses sight, which unsticks it. The brain's moves when blind are driven by whiskers and the reward brain, not by search.

### 4.4 Memory: none, and learning removes carry-over
From the weights (fundamental matrix of the route chain; `structure.q`):

| Brain (16 seeds) | Average steps an injected unit stays | Still inside after 1 bar / 2 bars | States on loops | Dominant path to an output (steps) |
|---|---|---|---|---|
| Untrained, modular | 14.3 | 49% / 8.7% | 4.2 | 7.0 |
| Untrained, single | 15.2 | 55% / 11.3% | 1.8 | 6.8 |
| Trained, open (10k) | 8.2 | 6.3% / 0.09% | 0.6 | 7.9 |
| Trained, pillars | 8.4 | 8.1% / 0.6% | 1.5 | 7.2 |
| Trained, rooms | 10.3 | 20% / 2.3% | 1.0 | 7.0 |
| Trained on many layouts (20k) | 8.3 | 4.4% / 0.45% | 1.9 | 7.5 |

- **Learning carves direct routes from input to output** and breaks loops. That suits chasing a visible mouse, and it is exactly wrong for remembering a hidden one.
- **Decoding the last-seen direction** from the end-of-bar brain state is at chance (1 in 8) in blink 20:20 and in pillars, in every band of bars since last seen. At the end of a hidden bar the brain holds any energy only 0.1–0.3% of the time in blink, and 8–12% in pillars (touch keeps it busy).

### 4.5 How the trained routes are laid out (lesions)
- **Method:** freeze each open-world brain, block one cell at a time (no route leads into it), and run 2,000 bars per cell.
- **The chase is spread out:** the worst single cell costs 30% of catches (−24 of 80 per 1,000 bars) and 0.18 alignment.
- **Damage concentrates on the middle columns** (mostly 2–7) of the sight brain's lower rows and the central brain's lower two rows: the input-to-output routes. The input rows themselves matter little, because their energy is injected rather than routed in.

---

## 5. Plan targets (O.6)

Target from the baselines: **the brain beats the reactive cat in at least 14 of 16 seeds**.
- **Met in 1 of 9 worlds:** see-through pillars (14/16), where reactive gets trapped pushing straight at a visible mouse and the brain's touch lets it get round.
- **Not met in the other 8** (0–1 of 16).

The targets stay as the bar for the next round.

---

## 6. Caveats

- **Many comparisons.** About 45 paired tests are reported here. At 95% confidence, two or three "significant" results are expected by chance. Single borderline results (reward while hidden in rooms, CI −12.2 to −0.06) should be treated as weak. The large, repeated effects (touch, the modular brain with walls, the reward brain with walls, spontaneous activity) are well clear of that.
- **The evasive-mouse arms in the batch were invalid.** With one mouse, the evasive step overwrote each respawn with the old position next to the cat, so greedy "caught" 985 mice per 1,000 bars. Fixed on 2026-11-03 (`bar_loop.q`, with a regression test in `test_weights.q`); the four evasive arms were rerun (`results/obs_evasive_rerun_20261103.txt`). The invalid outputs are kept as `results/o_evasive_*_20261103_buggy`.
- **The pilot's efference result** predates the modular brain and has no confidence intervals.
- **10,000-bar runs.** The walled-world arms weren't run beyond 10,000 bars; longer training might change their numbers.

---

## 7. Proposed changes, for the user's approval

1. **Defaults: no change.** Modular brain, touch, 3 central rows, reward-brain energy at the right edge, no direct reward while hidden, satiety edges as they are.
2. **Optional removal:** the `--efference` code (one flag and about five lines). It is off by default and harmful everywhere measured. Keep it if a motor copy is wanted later as its own input channel.
3. **Done on 2026-11-03 (the user's decisions):** `--spontaneous` removed (code, tests, close-out arm, docs). Its replacement, a sparse clock input at a fixed spot, is planned in [METRONOME_PLAN.md](../plans/future/METRONOME_PLAN.md). The efference copy stays as an off-by-default ablation; no default changed.
4. **Not removed, despite poor results here:**
   - `--layout-every`, the curriculum flags, `--central-rows` and `--meta-entry`, because they are cheap experiment tools whose results depend on problems not yet solved (memory, transfer).
5. **What the results say to do next,** in order:
   1. **Memory:** the brain can't hold state across a bar, and learning makes it worse. That is the 3D plan's Stage A (a sealed, self-sustaining substrate), and possibly a recurrent loop between the central brain and a dedicated memory module.
   2. **Search when blind:** the metronome plan's clock input, and route temperature while hidden.
   3. **Tell things apart:** the dog result is the vision plan's problem in practice.
   4. **Watch it:** the viewer plan, to see the stuck and blind behaviour directly.
