# Meta-Brain Experiment Framework

Standardized scripts for testing the meta-brain's ability to maintain pre-trained behavior.

## Quick Start

### Full Pipeline (Train → Test)
Run complete experiment: train 30 brains, test best with meta-brain only:
```bash
./scripts/run_meta_experiment.sh --full
```

**Time:** ~10-12 minutes (30 training + 5 testing trials)

### Resume from Checkpoint
Test meta-brain maintenance using previously trained weights:
```bash
./scripts/run_meta_experiment.sh --meta-only results/meta_experiments/run_TIMESTAMP/best_weights.bin
```

**Time:** ~2 minutes (5 testing trials)

### Baseline Test
Verify meta-brain cannot learn from scratch (no pre-training):
```bash
./scripts/run_meta_experiment.sh --from-scratch
```

**Time:** ~2 minutes (5 trials)

## Results Directory Structure

All experiments save to timestamped directories:
```
results/meta_experiments/
└── run_YYYYMMDD_HHMMSS/
    ├── training_results.csv       # Training phase catches per seed
    ├── meta_only_results.csv      # Meta-only phase detailed results
    ├── best_weights.bin           # Best performer's brain weights
    └── weights_seed*.bin          # All training trial weights (full mode)
```

## Experimental Design

### Three Test Modes

#### 1. Full Pipeline (`--full`)
**Purpose:** Complete acquisition → maintenance test

**Process:**
1. Train 30 fresh brains with direct + meta rewards
2. Identify best performer (highest catches)
3. Load best weights into 5 new trials
4. Test with meta-brain only (no direct rewards)

**Answers:** "Can meta-brain maintain trained performance?"

#### 2. Resume from Checkpoint (`--meta-only FILE`)
**Purpose:** Re-test maintenance with different conditions

**Process:**
1. Load specified weight file
2. Run 5 trials with meta-brain only
3. Analyze performance retention

**Answers:** "How robust is meta-brain maintenance?"

#### 3. Baseline Test (`--from-scratch`)
**Purpose:** Verify bootstrap requirement

**Process:**
1. Start 5 fresh brains (random weights)
2. Train with meta-brain only (no direct rewards)
3. Measure failure to learn

**Answers:** "Does meta-brain need initial pathways?"

## Expected Results

### Full Pipeline
- **Training best:** 1,400-1,700 catches
- **Training average:** 1,100-1,200 catches
- **Meta-only average:** 1,800-2,000 catches (~115% retention!)
- **Directionality:** 95%+ towards mouse

### From Scratch
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
   - Meta-brain alone: 0 catches (random walk)
   - Requires direct rewards to create initial pathways

4. **Two-stage learning architecture**
   - **Stage 1 (Direct rewards):** Acquisition - build basic pathways
   - **Stage 2 (Meta-brain):** Mastery - maintain & optimize strategies

## Command-Line Options

```bash
./scripts/run_meta_experiment.sh [OPTIONS]

Options:
  --full              Run full experiment: train 30 trials, test best with meta-only
  --meta-only FILE    Test meta-only mode using specified weight file
  --from-scratch      Test meta-only from scratch (no pre-training) as baseline
  --help              Show this help message
```

## Configuration

Edit script header to customize:
```bash
TRAINING_TRIALS=30        # Number of training trials (full mode)
META_TEST_TRIALS=5        # Number of meta-only test trials
SCRATCH_TEST_TRIALS=5     # Number of from-scratch baseline trials
```

## Output Files

### training_results.csv
```csv
seed,catches
1,1123
2,1456
...
```

### meta_only_results.csv
```csv
seed,catches,towards_pct,away_pct
1001,1923,95.3,4.5
1002,1876,94.8,4.9
...
```

### best_weights.bin
Binary file containing:
- Primary brain synapses: (6, 12, 8, 8) - 4D directional routing
- Meta-brain synapses: (7, 7, 8, 8) - 4D directional routing

## Related Scripts

- `run_learning_tests.sh` - Multi-trial learning with statistics
- `test_meta_only.sh` - Original meta-only experiment (deprecated)

## Biological Analogy

**Direct rewards:** Dopamine-based immediate feedback for basic skill acquisition

**Meta-brain:** Executive function / prefrontal cortex strategy recognition

**Finding:** Can't develop hunting strategy without basic movement skills first, but once acquired, strategic control alone is MORE effective than mixing strategic + tactical feedback.

Similar to how expert performance often becomes "unconscious" - direct feedback during execution can disrupt flow state!
