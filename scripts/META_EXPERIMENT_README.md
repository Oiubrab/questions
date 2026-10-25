# Running Controlled Experiments

How to test a change to the learning rules, or a hypothesis about them, without fooling yourself. All commands run from the repository root.

## Why the care

Outcomes vary enormously between seeds. In the October 2026 re-test, the same condition gave one trained brain 1 catch and another 570. A difference between two single runs, or between a handful of runs, tells you almost nothing. A dry run with 3 seeds per arm showed an apparent **+87.5%** effect whose 95% CI ran from −19 to +42 catches. Small samples, combined with the design problems described under [History](#history-of-the-meta-only-claim), are how the project's earlier "+15%" claim arose.

Rules of thumb:
- **Pair by brain.** Run every condition from the same trained brains, with the same run seed per brain. Compare within-brain differences, not raw averages.
- **≥ 16 brains** for effects of the size seen so far.
- **Compare like with like.** Every arm starts from the same warm state, and arms are compared over the same number of bars. Don't compare a warm-started arm with a from-scratch training run that includes the acquisition phase.
- **Don't select.** Testing only the best trained brain picks out unusual cases.
- **Include an ablation.** If a component is supposed to matter, run a condition with it switched off (`--ablate-meta`, `--no-meta-rewards`).

## Tools

**`scripts/run_trials.sh`** — runs many brain ids in parallel.
```bash
scripts/run_trials.sh -l LABEL -i "101 102 103" [-o SEED_OFFSET] [-j JOBS] -- [cat_mouse.q flags]
```
- Output goes to `results/LABEL/s<id>/`.
- The run seed is `id + SEED_OFFSET`. Use the same offset across arms so they share randomness.
- `{id}` in the flags is replaced by the id, for per-brain weight files: `--load-weights 'results/train/s{id}/weights_seed{id}.bin'`.
- Exits non-zero if any run failed to write `summary.csv`. Failed runs keep `stdout.txt`.

**`q/programs/compare.q`** — paired analysis.
```bash
q q/programs/compare.q results/REF results/ARM1 results/ARM2 ... < /dev/null
```
It reports:
- a per-arm summary (mean, SD, range, catches per 1000 bars, towards %);
- paired differences vs the first (reference) arm, matched by id, with a t statistic and a bootstrap 95% CI;
- per-epoch catch rates.

**`scripts/phase3_experiment.sh [K] [TRAIN_BARS] [ARM_BARS]`** — the full re-test below (defaults 16 / 20000 / 10000). It skips training if `results/p3_train` is complete, and writes `results/p3_report.txt`.

## The reaction-dissociation re-test (Phase 3)

| Arm | Flags (all `--load-weights` from the trained brain) | Question |
|---|---|---|
| A | none — direct + meta rewards continue | baseline: keep training |
| B | `--meta-only` | does removing tactical rewards help? (B vs A) |
| C | `--meta-only --no-meta-rewards` | does strategic reinforcement do anything? (C vs B) |
| D | `--meta-only --ablate-meta` | do the meta-brain's outputs matter? (D vs B) |

**Results (October 2026, 16 brains):**

| Comparison | Mean paired difference | 95% CI |
|---|---|---|
| B − A | −153.7 catches (−49.5%) | −206.7 … −97.1 |
| C − B | +71.0 (+45%) | −5.4 … +136.1 |
| D − B | +2.8 (+2%) | −33.3 … +40.1 |

- Removing direct rewards halves performance, which keeps declining over the run.
- The meta-brain's outputs have no measurable effect.
- 3 of 16 brains did better in meta-only than with continued training.

Full tables and caveats are in [REVIVAL_PLAN.md §6](../plans/inProgress/REVIVAL_PLAN.md#6-results).

## History of the meta-only claim

The earlier version of this document reported that "meta-brain alone exceeds training by ~15%" and called the meta-brain a strategic controller. That came from the Fortran version via a script, since deleted, that:
1. trained 30 brains and picked the best one;
2. ran it 5 times in meta-only mode, starting warm;
3. divided the meta-only average by the average of *all 30* training runs from scratch.

That design favours meta-only twice over: by selection, and by skipping the acquisition phase. In addition, in that Fortran code the meta-brain's strategic reinforcement only ran during the first 100 bars of a run (REVIVAL_PLAN.md F6). So the meta-brain couldn't have caused the effect. The controlled re-test above replaces those results.
