# Plan: evolve the setup (a population of learners, selected for catching mice)

**Status (2026-11-07):** written, not started. Waiting on the user's decisions in §5. Nothing has been run for this plan; every number below that is not cited is an estimate, and task E3 replaces it with a measurement.

**Why it exists.** The user (2026-11-07): "considering the amount of variables and things that can change in the system, I wonder whether … it might be useful to apply an evolutionary process to find the best mice catching setup … we set off a bunch of different models with different brain sizes, different return to equivalent probability rates, different reinforcement rates, etc, and just run them all against each other in different tests and then pick one that works, then spawn a bunch of that to test, etc. … this would mirror nature more closely." Asked to write it as a future plan for now.

**How to use this document:** the same rules as the other plans. Tasks are checkboxes (`[ ]` not started, `[~]` partly done, `[x]` done and verified). Tick only after the acceptance check passes, with the date. Every session appends to the Progress Log (§8). Decisions in §5 belong to the user. Claims need ≥ 16 paired seeds **on seeds the search never saw** (§2.4).

---

## 0. Goal

A search that finds settings (and later structure) under which the cat **learns to head towards the mouse, and finds it when it can't see it**, better than today's defaults, without anyone hand-tuning one value at a time.

It earns its place in two ways, and both count:
- **a better setup**, confirmed on fresh seeds against the current defaults (§3, E5);
- **knowledge**: which values matter, which don't, and which interact. Hand sweeps have tuned one value at a time; they cannot see interactions.

**What it does not do:** change the user's design principles (revival plan §2). Those are fixed; the search runs inside them (§2.1). It also does not touch the default until the user agrees to adopt a result (§5, decision 6).

## 1. What the earlier work says about the idea

- **Many values have only had one-at-a-time sweeps.** Reward scale has a plateau (0.1–0.5 all beat 1; 0.1 vs 0.25 indistinguishable), hebb 250 beat 1000 on a pre-registered confirmation, narrowing the weight ratio only hurt, a 6-row central brain is worse than 3 (revival plan §6, obstacles plan §6). Re-tuning those alone is unlikely to give much. The unexplored part is **interactions**, and the roughly 200 values that are hard-coded and never swept ([PARAMETERS_PLAN.md](PARAMETERS_PLAN.md) §1: reinforce ×1.2, punish ×0.8, per-step 0.95, the decay factor and period, steps per bar, the strategic factor, the meta-brain's size …).
- **The three blockers are structural** ([OBSTACLES_RESULTS.md](../../researchNotes/OBSTACLES_RESULTS.md) §1): no memory, getting stuck where it can't see, and input that can't tell things apart. A search over *numbers* can tune a learner; it cannot invent a memory. It could find one only if the genome includes structure (a clock, sizes, where the reward brain enters, later vision and substrate options). Hence the levels in §2.1.
- **Results vary hugely by seed.** An n = 3 dry run once gave a spurious +87.5% with a CI of −19…+42 (revival plan §1). A search that picks the best of hundreds of genomes will find lucky ones. This is the main technical risk, and §2.3–2.4 are built around it.
- **Pretraining hurt** on held-out layouts (a fresh brain beat pretrained ones by 28–42%). That argues for evolving the *learner* and starting each life from a fresh brain, not inheriting weights (§2.2).
- **Cost.** Sixteen parallel 20,000-bar runs from scratch took 781 s on the 16-core machine ([OPTIMIZATION_NOTES.md](../../researchNotes/OPTIMIZATION_NOTES.md)): about 40 ms a bar with every core busy. Acquisition takes a median of roughly 540–1,200 bars (revival plan §0, §6), so a short run still sees most of the learning. A bigger brain or a busier one costs more per bar.
- **Tooling that exists:** `scripts/run_trials.sh` (parallel seeds), the `summary.csv` columns (`catches`, `meanAlignment`, `alignVisible`, `alignHidden`, `acquisitionTime`, `blockedFrac`), `world_report.q` and the scripted `--policy` baselines (random = 0, planner = 1), `compare.q` (paired bootstrap), and `policy_ceiling.q` (alignment predicted from weights alone, a possible cheap screen).

## 2. Design

### 2.1 The genome: what evolves, in levels

Every gene is one line in a **genome spec file** (`config/evo_genome.conf`, data, not code): name, how it is set (a `cat_mouse.q` flag, or `--set name=value` once PARAMETERS_PLAN P1 exists), type, range, scale (linear or log) and default. A genome is turned into a flag list; the run itself is the normal `cat_mouse.q`.

| Level | Genes | Needs |
|---|---|---|
| **1. Existing flags** | `--hebb` (log, must stay > 0), `--reward-scale` (log), `--weight-ratio` (log, ≥ 40), `--central-rows` (integer, 2–5), `--sat-edges` (four sorted values), `--meta-entry` (`right` or `top`; `none` is known to cost 75–85% in walled worlds) | nothing: can start now |
| **2. All model parameters** | the learning-rule factors, decay factor and period, steps per bar, strategic factor and scopes, catch-rate window, history depth, strength floor and initial range, the meta-brain's size, the sight and touch brains' rows | PARAMETERS_PLAN P1 (so a parameter is settable by name); brain sizes also need the sense-brain rows (`SENSE_ROWS`, now a literal 3) and the meta size made settable. Columns can't simply grow: 12 = 8 directions plus the input and output margins |
| **3. Structure** | clock on/off, period and cell ([METRONOME_PLAN.md](METRONOME_PLAN.md)), touch on/off, vision options ([VISION_PLAN.md](VISION_PLAN.md)), substrate rule settings from the lab (`.sub`: route temperature, direction bias, wrap) | those plans |

**"Return to equivalent probability rates"** is read here as the rates at which weights relax back towards the floor: the decay factor and its period, and the weight-ratio cap. *To confirm (§5, decision 7).*

**Fixed, never evolved** (user-stated principles and invariants): the unconditional per-use reinforcement stays on (hebb's lower bound is above 0; the "don't remove it" rule in CLAUDE.md), energy conservation, the modular brain with a satiety reward brain, and the harmful options (efference copy, random spontaneous activity). Ranges are chosen so no genome can switch these off. A gene that makes the flag parser abort is a spec bug, caught by E1.

### 2.2 Darwinian, not Lamarckian

Each individual **starts from a fresh brain** and learns during its own life. Selection acts on how well that learning goes (speed and level), and only the genome is passed on, not the learned weights. This is what nature does, and it fits the goal ("the cat must actually learn"): what we want is a good *learner*. Inheriting weights is a later arm (§5, decision 1) and the pretraining result is a warning against making it the default.

### 2.3 Fitness

- **Per world:** catches per 1,000 bars over the whole run (so both fast learning and a high plateau count), averaged over the generation's seeds, then scaled to the 0 = random cat, 1 = planner score used in OBSTACLES_RESULTS §1. The two baselines per world are computed once in E2 and stored (learning off, cheap).
- **Fitness:** the mean score over the **training worlds**. Proposed: open, blink 20:20, pillars: the visible chase, the hidden mouse without walls, and walls.
- **Held-out worlds** (rooms, wall with doors, random layouts, and the other blinks) are never used for selection. They are the check that the winner learned something general, not the three training layouts.
- **Recorded, not selected on:** `alignVisible`, `alignHidden`, `acquisitionTime`, `blockedFrac`, and the north-star count (seeds with alignment ≥ 0.7). Whether alignment should be selected on is §5, decision 3.
- **"Against each other":** all individuals in a generation run the **same seeds** (common random numbers), so their scores are paired and their differences are less noisy. That is the head-to-head. Competition between cats in one world adds nothing here unless the environment is co-evolved (§7).

### 2.4 Keeping the search honest (noise, luck, overfitting)

1. **Fresh seeds every generation.** A genome can't adapt to specific mice and layouts.
2. **Elites are re-evaluated** on each new seed set, so a lucky score can't persist. A genome's fitness is its mean over all the generations it survives.
3. **A null control (E4):** the same machinery with selection replaced by random choice. It measures how much "improvement" noise alone produces, and the winner's-curse gap (score at selection minus score on fresh seeds).
4. **Independent repeats:** at least 3 searches from different random starts. If they land in the same region, the landscape is informative; if not, it is flat or noise, and that is a result too.
5. **Confirmation on seeds never used in any search**, 16 or more, paired against the current default, in all 9 worlds (E5), with predictions written down **before** it is run (see the stage-order note in memory: no looking at results before the plan and the predictions exist).
6. **Short runs are a proxy.** A fast learner can plateau lower. The search runs short (§2.5); the finalists are re-run at 10,000–20,000 bars before they count.
7. **~45 paired tests are already reported** (OBSTACLES_RESULTS §6), so a borderline CI is weak. Only effects that are large and repeat on fresh seeds are claimed.

### 2.5 The algorithm and the budget

A **(μ + λ) evolution strategy with elitism**, the simplest thing that does what the user describes:
1. Start from a population of random genomes **plus the current default** (the control that must be beaten).
2. Evaluate everyone (§2.3). Rank. Keep the top μ.
3. Spawn λ children: pick two parents by rank (tournament), cross over gene by gene, and mutate (Gaussian in log space for log genes, a bounded step for integers, a flip for categories). The mutation size shrinks over generations or is itself a gene (self-adaptation). Children are clamped to the spec ranges.
4. Repeat for G generations or until the best genome has not improved for several generations.

All the search's own settings (population size, μ, λ, mutation size, generations, seeds per genome, bars per run, training worlds, the search seed) are flags or environment variables with defaults documented at the top of the script, in line with the no-hard-coded-values rule. The search has its **own seeded random stream**, separate from the simulation's, so a search is reproducible.

Rough size, to be measured in E3: population 32, 4 seeds, 2,000 bars, one training world = 128 runs, 8 batches on 16 cores at about 80 s ≈ **10 minutes a generation**; 30 generations ≈ 5 hours; three training worlds, about three times that. Successive halving (screen everyone on a cheap setting, give only the top half the full one) is the first lever if it is too slow. The search takes every core while it runs.

Each generation writes `results/evo/LABEL/genNN/` with the genomes, per-world scores, seeds and the flag lines used, so any individual can be reproduced with one `cat_mouse.q` command.

## 3. Tasks

- [ ] **E0 Decisions** (§5).
- [ ] **E1 Genome library.** `q/modules/evo/evo.q` (`.evo`): read the spec, sample, clamp, mutate, cross over, genome → flag list, with its own RNG. Tests in `q/tests/test_evo.q`.
  *Accept:* tests for bounds (10,000 random and mutated genomes all in range), determinism (same search seed, same population), and sortedness of `--sat-edges`; **every gene at its lower and its upper bound is accepted by `cat_mouse.q`** (a 20-bar run each); the default genome produces exactly the default flags; no tunable literal in the code (§2.5); `q/tests/run_all.sh` passes.
- [ ] **E2 Fitness harness and baselines.** `q/programs/evo/evaluate.q` (or a script on `run_trials.sh`): run one genome on a seed list and world list, read the results, return the per-world scores. Failed or timed-out runs score worst and are logged, never silently dropped. Compute and store the random and planner baselines per training and held-out world.
  *Accept:* the default genome, evaluated twice on the same seeds, gives byte-identical fitness; it equals a hand-run `cat_mouse.q` with the same flags (so the genome → flags step is lossless); the baselines reproduce the 0 and 1 in OBSTACLES_RESULTS §1 within the paired CI.
- [ ] **E3 Calibrate noise and cost** (measurement only; no search). On the default genome and a few spread-out random genomes: the seed-to-seed spread of the fitness per world; how many seeds keep the ranking of genomes stable between two fresh seed sets; time per run at 2,000, 5,000 and 10,000 bars; whether `policy_ceiling.q` or a 2,000-bar score ranks genomes like a 10,000-bar score (rank correlation).
  *Accept:* a table in §6, and the population, seeds and bars of the search chosen from it and written into §6 **before E4 starts**.
- [ ] **E4 Pilot search, Level 1 genes.** At least 3 independent searches, plus the null control (§2.4, 3). Training worlds as decided.
  *Accept:* each search's log shows fitness by generation; the default genome's rank is recorded each generation; the winner's-curse gap is measured; the null control's improvement is reported next to the real one.
- [ ] **E5 Confirm.** The best 1–3 genomes from E4 (and the default) on **fresh seeds, ≥ 16, 10,000–20,000 bars, all 9 worlds**, paired with `compare.q`. Write the predictions first: which worlds should improve, which should not move, and what would count as no result.
  *Accept:* a results table with CIs, in §6. A genome is *better* only if its total score improves with a CI excluding 0, **no world is significantly worse**, open-world alignment is not significantly lower, and held-out worlds are no worse than the training ones.
- [ ] **E6 What the search learned.** Which genes moved from their defaults and which stayed; the spread of each gene across the 3 repeats; one-at-a-time sweeps of each gene around the winner; pairs of genes that must move together. Feed the findings back into the revival plan §6 or OBSTACLES_RESULTS as notes.
  *Accept:* a short `researchNotes/EVOLUTION_RESULTS.md` (the pattern OBSTACLES_RESULTS set), with the verdicts.
- [ ] **E7 Widen to Level 2** (after PARAMETERS_PLAN P1; brain sizes need the sense-brain rows and meta size made settable). Repeat E3–E5 with the larger genome. Watch `brainEnergy`: the CLAUDE.md warning on resizing the meta-brain applies to every size gene.
- [ ] **E8 Structure genes, Level 3** (after the clock, vision and 3D plans). The test the user's idea is best placed for: does the search find *a memory*? Include the hidden-mouse worlds with high weight and look at carry-over (`structure.q`, `decode.q`).
- [ ] **E9 Adopt.** Only if the user agrees (§5, decision 6) and E5 passes: the winner becomes the default; update the golden digest, the revival plan §2, BAR_STRUCTURE.md, the README flag table and CLAUDE.md status.

## 4. Risks

- **Selecting on luck.** Mitigated by §2.4; the null control quantifies what is left.
- **No headroom.** The sweeps already show plateaus. A null result at Level 1 is plausible and still useful (E6 says which values are flat). Level 2–3 is where the unexplored part is.
- **Overfit to the three training worlds.** The held-out worlds and E5 are the guard.
- **Short-run bias** towards quick learners with a lower ceiling (§2.4, 6).
- **Cost.** It uses every core for hours, and a bigger-brain gene raises the cost per run. A per-run time limit and successive halving are the defences.
- **A moving target.** Any change to the default code (the clock, vision, the parameter refactor) changes the fitness landscape; results hold for the code state they were run on, and a search is repeated after such a change if its winner is to be adopted.
- **Degenerate winners:** a genome that scores by freezing, or by exploiting a world's geometry. The recorded measures (move share, blocked fraction, alignment) and the held-out worlds are how that shows up.
- **It can't supply what the model lacks.** If the three blockers are structural, Levels 1–2 won't solve them; only Level 3 might.

## 5. Decisions for the user

1. **Darwinian or Lamarckian?** (a) Each life starts fresh; only the genome is inherited. (b) Offspring also inherit weights. **Recommendation: (a)** as the main line (§2.2); (b) as one arm later, knowing the pretraining result.
2. **Where to start: which genome level, and in what order?** Level 1 needs nothing and tests the whole machinery on a few values. Level 2 is the real prize but waits for PARAMETERS_PLAN P1. **Recommendation:** E1–E5 on Level 1 now as the pilot, then Level 2 after the parameters plan's registry exists; Level 3 with the plans it depends on.
3. **What to select on.** (a) The mean normalised catch score over the training worlds; (b) a Pareto front of several measures (catch score, open alignment, hidden-mouse score); (c) alignment, the stated north star. **Recommendation: (a)**, with alignment recorded and checked at confirmation: catches are comparable across worlds and less noisy, and alignment is not the right measure in walled worlds. Also: the training worlds (proposed open, blink 20:20, pillars; the rest held out).
4. **Compute budget.** A pilot of roughly the §2.5 size (about 5 hours a search, three repeats and a null control, before E5), or smaller or larger? E3 gives the real numbers; this asks how many core-hours you want to spend.
5. **What "against each other" means.** (a) All individuals run the same seeds and are ranked on the scores (§2.3). (b) Head-to-head or co-evolved environments (for example an evolving mouse: `--evasive`, the dog). **Recommendation: (a);** (b) as a later extension.
6. **Adoption.** A winner becomes the default only after E5 passes **and you agree**, and the golden digest is updated as in every earlier adoption. **Recommendation:** as stated.
7. **"Return to equivalent probability rates".** Read here as the decay factor and period and the weight-ratio cap (the rates weights fall back towards the floor). Is that what you meant, or something else (for example the reinforcement probabilities, or the drain rates, which never engage today)?

## 6. Results

None yet.

## 7. Later ideas (not tasks)

- **Co-evolve the world:** an evolving mouse (evasive) or dog, so the cats face a moving difficulty (decision 5b).
- **Niches:** keep separate sub-populations per world, and breed across them, so specialists in rooms and in blink don't lose to a generalist early.
- **Novelty or quality-diversity search (MAP-Elites)** over behaviour measures (move share when hidden, blocked fraction, carry-over), to find *different* ways to catch, not only the best one.
- **Evolving the rules, not the numbers** (the substrate lab's settings and beyond), which is where the 3D plan's research lives.

## 8. Progress Log

### 2026-11-07 — plan written
- The user asked for an evolutionary search over the setup (brain sizes, decay rates, reinforcement rates and so on) as a future plan.
- Written: a (μ + λ) strategy over a genome of flags and, later, named parameters and structure; fresh brains each life; fitness paired on shared seeds; guards against luck and overfitting (fresh seeds each generation, re-evaluated elites, a null control, held-out worlds, a pre-registered confirmation).
- Nothing built or run. Waiting on §5.
