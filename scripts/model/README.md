# The catch-rate model

The scripts behind [researchNotes/CATCH_RATE_ANALYSIS.md](../../researchNotes/CATCH_RATE_ANALYSIS.md), moved here from a session scratch directory on 2026-11-08 (CATCH_RATE_PLAN.md task T.2). They're analysis tools, not unit tests: nothing here is run by `q/tests/run_all.sh`.

Python 3 with numpy. Inputs and outputs live under `results/model/` (gitignored; `MODEL_OUT` overrides): exported weights in `results/model/w/<arm>_<id>.txt`, solved tables as JSON. Run everything from the repo root unless noted.

## The models

| File | What it is |
|---|---|
| `model.py` | The stochastic mean-field learning model of the sight and central brains (§2). `run(th0, p, seed=None)` is the noise-free version, any seed the noisy one. `defaults()` holds today's constants; the sweeps change them. `spont()` measures deviation and alternative share. The other scripts load it with `exec()`. |
| `kin.py` | The kinematic model: a fixed slice → move lookup on the wrapped field, no brain. Running it prints the validation against the scripted reactive cat (§2) and the task ceilings (§3.5). |
| `export.q` | Exports trained weights for the model: `q scripts/model/export.q results/model/w ARM [ARM ...] < /dev/null` |
| `spont_arm.py` | Weights-only spontaneity of any exported arm, paired against the first: `python3 scripts/model/spont_arm.py ARM [ARM ...]` (plan task T.3) |
| `spont_any.py` | The same measure for a modular brain of any central depth (Phase R): reproduces `spont_arm.py`'s deviation exactly on today's brains; alt. share agrees within 1% |

## What produced which table

| Note section | Scripts | Output (in `results/model/`) |
|---|---|---|
| §3.1 constants, §3.3 speeds, §3.7 escape times | `forms.py`, `closed.py` (`python3 scripts/model/closed.py scripts/model`), `drift.py` | printed |
| §3.6 memory | `mem.py` | printed |
| §3.7 wall trap, measured | `stuck.py` (reads the obstacles close-out's `world_log.csv` files under `results/`) | printed |
| §2 validation, §2.1 metric gap | `validate.py`, `val2.py`, `recon.py`, `decomp.py` | `recon.json` |
| §4.1 every constant alone | `sweep.py` (noise-free), `sweep2.py` (noisy), then `stats2.py`, `show.py` | `sweep_nonoise.json`, `sweep2.json` |
| §4.2 combinations | `sweep3.py`, `sweep4.py`, `sweep5.py` with `tab3.py`, `tab4.py`, `tab5.py` | `sweep3*.json`, `sweep4.json`, `sweep5*.json` |
| §6 threshold table | `dt.py` | printed |

Sweeps take the model's path and an output file: `cd results/model && python3 ../../scripts/model/sweep5.py - ../../scripts/model/model.py sweep5.json` (about 40 minutes on 16 cores). The table scripts read the JSON from the current directory, so run them from `results/model/` too.

**Files marked `INVALID_reflection` in `results/model/`** were solved with the re-learning bug (L180 as a reflection, §7) and must not be used. `model.py` has the fix; it differs from the buggy version only in that line and the performance-gate option, so the non-re-learning tables are unaffected.

## The ledger (§2 "baseline measurements", §3.1 ledger table, §5)

`probe/`: the simulator with three hooks in the bar loop that record each reward term per route. The hooks use no RNG, so a probed run reproduces the plain run exactly.

```bash
scripts/model/probe/make_probe.sh                       # hooked copy of the current bar loop -> results/model/probe/
q scripts/model/probe/probe.q --seed 1 --bars 10000 --output-dir results/model/probe/pr1/ < /dev/null
q scripts/model/probe/ledger.q results/model/probe < /dev/null    # pr1..pr16: per-route-class ledger, per-bar controls
```

`ctl.q` summarises the meta-brain's controls per seed; `timecourse.q`, `exec_loss.q` and `policy_alpha.q` are the per-slice policy, execution-loss and temperature analyses of saved brains.

## Not kept

The exploratory runs listed in the note's Appendix (a modified copy of the simulator and its arms) are excluded. They aren't evidence, and a second copy of the simulator would drift from the real one.
