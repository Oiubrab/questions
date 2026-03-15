# Meta-Brain Experiment Framework

Standardized approach for testing the meta-brain's ability to maintain pre-trained behaviour.

## Quick Start

### Full Pipeline (Train → Test)
Run complete experiment: train 30 brains, test best with meta-brain only:
```bash
q q/load.q q/programs/cat_mouse.q -- --seed 1 --bars 20000
# ... repeat for seeds 1-30, save results
# Then test best weights:
q q/load.q q/programs/cat_mouse.q -- --load-weights results/.../weights.q --meta-only
```

### Resume from Checkpoint
Test meta-brain maintenance using previously saved weights:
```bash
q q/load.q q/programs/cat_mouse.q -- --load-weights results/run_TIMESTAMP/weights.q --meta-only
```

### Baseline Test
Verify meta-brain cannot learn from scratch:
```bash
q q/load.q q/programs/cat_mouse.q -- --seed 42 --bars 20000 --meta-only
```

## Results Directory Structure

All experiments save to timestamped directories:
```
results/
└── run_YYYYMMDD_HHMMSS/
    ├── simLog.csv        # Exported kdb-x simulation log (all bars)
    ├── weights.q         # Brain weights — q binary (prim + meta synapses)
    └── epoch_summary.csv # Per-epoch catch rate, directionality, meta activity
```

**Note:** `results/` is gitignored. Weights use native q binary serialization (`set`/`get`), not the old Fortran `.bin` format.

## Experimental Design

### Three Test Modes

#### 1. Full Pipeline (`--bars 20000`)
**Purpose:** Complete acquisition → maintenance test

**Process:**
1. Train 30 fresh brains with direct + meta rewards (`--seed 1` through `--seed 30`)
2. Identify best performer (highest total catches from `simLog`)
3. Load best weights into 5 new trials with `--meta-only`
4. Compare meta-only performance vs training performance

**Answers:** "Can meta-brain maintain trained performance?"

#### 2. Resume from Checkpoint (`--load-weights FILE --meta-only`)
**Purpose:** Re-test maintenance with different conditions

**Process:**
1. Load saved weight file
2. Run with `--meta-only` flag (disables direct per-bar rewards)
3. Analyse performance retention via `simLog`

**Answers:** "How robust is meta-brain maintenance?"

#### 3. Baseline Test (`--meta-only` without `--load-weights`)
**Purpose:** Verify bootstrap requirement

**Process:**
1. Start with random weights (no `--load-weights`)
2. Run with `--meta-only` from the start
3. Measure failure to learn

**Answers:** "Does meta-brain need initial pathways?"

## Expected Results

### Full Pipeline
- **Training best:** 1,400-1,700 catches
- **Training average:** 1,100-1,200 catches
- **Meta-only average:** 1,800-2,000 catches (~115% retention)
- **Directionality:** 95%+ towards mouse

### From Scratch (meta-only, no pre-training)
- **Catches:** 0-6 (essentially random)
- **Directionality:** 2-10% towards (chance level)
- **Conclusion:** Meta-brain cannot bootstrap

## Key Findings

1. **Meta-brain EXCEEDS training performance** (+15% improvement)
   - Pre-trained + meta-only: ~1,900 catches
   - Original training: ~1,700 catches best

2. **Direct rewards may introduce noise**
   - Meta-brain operates at strategic timescale (20-120 bars)
   - Direct rewards operate at tactical timescale (1 bar)
   - Removing tactical noise improves strategic execution

3. **Bootstrap requirement validated**
   - Meta-brain alone from scratch: 0 catches (random walk)
   - Requires direct rewards to create initial pathways first

4. **Two-stage learning architecture**
   - **Stage 1 (Direct rewards):** Acquisition — build basic pathways
   - **Stage 2 (Meta-brain only):** Mastery — maintain and optimise strategies

## Querying Results in q

After a run, `simLog` is a kdb-x table. Query it directly:

```q
/ Total catches per epoch
select sum catches by epochN from simLog

/ Directional accuracy over time
select avg towardPct by epochN from simLog

/ Meta-brain activity (when did it fire?)
select bar, metaScope, metaMag from simLog where metaScope > 0

/ Best epoch
select from (select sum catches by epochN from simLog) where catches = max catches
```

## Biological Analogy

**Direct rewards:** Dopamine-based immediate feedback for basic skill acquisition.

**Meta-brain:** Executive function / prefrontal cortex strategy recognition.

**Finding:** Cannot develop hunting strategy without basic movement skills first. But once acquired, strategic control alone is *more* effective than mixing strategic + tactical feedback.

Similar to expert performance becoming "unconscious" — direct feedback during execution can disrupt flow state.

## Related Documentation

- [README.md](../README.md) — Project overview and quick start
- [BAR_STRUCTURE.md](../BAR_STRUCTURE.md) — Temporal organisation and credit assignment
- [CROSS_FLOW_ARCHITECTURE.md](../CROSS_FLOW_ARCHITECTURE.md) — Pressure-regulated brain I/O
