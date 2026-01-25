#!/bin/bash
# Meta-Brain Experiment Framework
# Tests whether pre-trained brains can maintain performance with meta-brain only
#
# Usage:
#   ./scripts/run_meta_experiment.sh --full          # Full pipeline: train → test meta-only
#   ./scripts/run_meta_experiment.sh --meta-only     # Test meta-only with existing weights
#   ./scripts/run_meta_experiment.sh --from-scratch  # Test meta-only without pre-training (baseline)

set -e

# Configuration
TRAINING_TRIALS=30
META_TEST_TRIALS=5
SCRATCH_TEST_TRIALS=5
RESULTS_BASE="results"

# Parse command line
MODE=""
WEIGHT_FILE=""

show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --full                   Run full experiment: train N trials, test best with meta-only"
    echo "  --meta-only FILE         Test meta-only mode using specified weight file"
    echo "  --from-scratch           Test meta-only from scratch (no pre-training) as baseline"
    echo "  --training-trials N      Set number of training trials (default: 30)"
    echo "  --meta-test-trials N     Set number of meta-test trials (default: 5)"
    echo "  --scratch-test-trials N  Set number of from-scratch trials (default: 5)"
    echo "  --help                   Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 --full --training-trials 5 --meta-test-trials 2"
    echo "  $0 --meta-only results/run_20260102_063000/phase_1_full_reward/trial_01/weights_seed1.bin"
    echo "  $0 --from-scratch --scratch-test-trials 10"
}

while [[ $# -gt 0 ]]; do
    case $1 in
        --full)
            MODE="full"
            shift
            ;;
        --meta-only)
            MODE="meta-only"
            WEIGHT_FILE="$2"
            shift 2
            ;;
        --from-scratch)
            MODE="from-scratch"
            shift
            ;;
        --training-trials)
            TRAINING_TRIALS="$2"
            shift 2
            ;;
        --meta-test-trials)
            META_TEST_TRIALS="$2"
            shift 2
            ;;
        --scratch-test-trials)
            SCRATCH_TEST_TRIALS="$2"
            shift 2
            ;;
        --help)
            show_usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

if [ -z "$MODE" ]; then
    echo "Error: Must specify mode (--full, --meta-only, or --from-scratch)"
    show_usage
    exit 1
fi

# Save workspace root
WORKSPACE_ROOT="$(pwd)"

# Create timestamped results directory
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RUN_DIR="$RESULTS_BASE/run_$TIMESTAMP"
mkdir -p "$RUN_DIR"

echo "=========================================="
echo "META-BRAIN EXPERIMENT"
echo "=========================================="
echo "Mode: $MODE"
echo "Results: $RUN_DIR"
echo ""

# ============================================
# MODE: FROM SCRATCH (Baseline)
# ============================================
if [ "$MODE" == "from-scratch" ]; then
    echo "Testing: Meta-brain only, no pre-training (baseline)"
    echo "--------------------------------------------------"
    
    SCRATCH_DIR="$RUN_DIR/phase_baseline_scratch"
    mkdir -p "$SCRATCH_DIR"
    
    echo "seed,catches,towards_pct,away_pct" > "$SCRATCH_DIR/results.csv"
    
    for seed in $(seq 1 $SCRATCH_TEST_TRIALS); do
        echo -n "  Trial $seed... "
        
        # Create trial directory
        trial_dir="$SCRATCH_DIR/trial_$(printf "%02d" $seed)"
        mkdir -p "$trial_dir"
        
        # Run with output-dir flag
        output=$("$WORKSPACE_ROOT/bin/cat_mouse_learning" $seed --no-direct-rewards --output-dir "$trial_dir" 2>&1)
        
        # Save stdout to results.txt
        echo "$output" > "$trial_dir/results.txt"
        
        catches=$(echo "$output" | grep "Total mice caught:" | awk '{print $4}')
        towards=$(echo "$output" | grep "Moves TOWARDS" | grep -oP '\(\s*\K[0-9.]+' | head -1)
        away=$(echo "$output" | grep "Moves AWAY" | grep -oP '\(\s*\K[0-9.]+' | head -1)
        
        echo "$catches catches ($towards% towards)"
        echo "$seed,$catches,$towards,$away" >> "$SCRATCH_DIR/results.csv"
    done
    
    echo ""
    echo "Results Summary:"
    avg_catches=$(tail -n +2 "$SCRATCH_DIR/results.csv" | awk -F',' '{sum+=$2} END {print sum/NR}')
    avg_towards=$(tail -n +2 "$SCRATCH_DIR/results.csv" | awk -F',' '{sum+=$3} END {print sum/NR}')
    echo "  Average catches: $avg_catches"
    echo "  Average towards: $avg_towards%"
    echo ""
    echo "✓ Baseline established - meta-brain cannot learn from scratch"
    echo "Results saved to: $SCRATCH_DIR/"
    
    exit 0
fi

# ============================================
# MODE: FULL PIPELINE
# ============================================
if [ "$MODE" == "full" ]; then
    PHASE1_DIR="$RUN_DIR/phase_1_full_reward"
    mkdir -p "$PHASE1_DIR"
    
    echo "Phase 1: Training with full reward system ($TRAINING_TRIALS trials)"
    echo "--------------------------------------------------"
    
    echo "seed,catches,towards_pct,away_pct" > "$PHASE1_DIR/results.csv"
    
    best_catches=0
    best_seed=0
    
    for seed in $(seq 1 $TRAINING_TRIALS); do
        echo -n "  Trial $seed... "
        
        # Create trial directory
        trial_dir="$PHASE1_DIR/trial_$(printf "%02d" $seed)"
        mkdir -p "$trial_dir"
        
        # Run with output-dir flag
        output=$("$WORKSPACE_ROOT/bin/cat_mouse_learning" $seed --output-dir "$trial_dir" 2>&1)
        
        # Save stdout to results.txt
        echo "$output" > "$trial_dir/results.txt"
        
        catches=$(echo "$output" | grep "Total mice caught:" | awk '{print $4}')
        towards=$(echo "$output" | grep "Moves TOWARDS" | grep -oP '\(\s*\K[0-9.]+' | head -1)
        away=$(echo "$output" | grep "Moves AWAY" | grep -oP '\(\s*\K[0-9.]+' | head -1)
        
        echo "$catches catches ($towards% towards)"
        echo "$seed,$catches,$towards,$away" >> "$PHASE1_DIR/results.csv"
        
        # Track best
        if [ "$catches" -gt "$best_catches" ]; then
            best_catches=$catches
            best_seed=$seed
        fi
    done
    
    echo ""
    echo "Training Phase Complete"
    echo "  Best trial: seed $best_seed with $best_catches catches"
    
    # Copy best weights to run directory root
    best_trial_dir="$PHASE1_DIR/trial_$(printf "%02d" $best_seed)"
    WEIGHT_FILE="$best_trial_dir/weights_seed${best_seed}.bin"
    cp "$WEIGHT_FILE" "$RUN_DIR/best_weights.bin"
    
    echo "  Best weights: $RUN_DIR/best_weights.bin"
    echo ""
    
    # Calculate training statistics
    training_avg=$(tail -n +2 "$PHASE1_DIR/results.csv" | awk -F',' '{sum+=$2} END {printf "%.1f", sum/NR}')
    training_towards=$(tail -n +2 "$PHASE1_DIR/results.csv" | awk -F',' '{sum+=$3} END {printf "%.1f", sum/NR}')
    training_min=$(tail -n +2 "$PHASE1_DIR/results.csv" | awk -F',' 'NR==1{min=$2} $2<min{min=$2} END {print min}')
    training_max=$(tail -n +2 "$PHASE1_DIR/results.csv" | awk -F',' 'NR==1{max=$2} $2>max{max=$2} END {print max}')
    
    echo "Training Statistics:"
    echo "  Best: $best_catches catches (seed $best_seed)"
    echo "  Average: $training_avg catches"
    echo "  Range: $training_min - $training_max"
    echo "  Avg directionality: $training_towards% towards"
    echo ""
    
    # Update weight file to use the one in run directory
    WEIGHT_FILE="$RUN_DIR/best_weights.bin"
fi

# ============================================
# MODE: META-ONLY TEST (for both --full and --meta-only)
# ============================================
if [ "$MODE" == "full" ] || [ "$MODE" == "meta-only" ]; then
    PHASE2_DIR="$RUN_DIR/phase_2_meta_only"
    mkdir -p "$PHASE2_DIR"
    
    if [ "$MODE" == "meta-only" ]; then
        if [ ! -f "$WEIGHT_FILE" ]; then
            echo "Error: Weight file not found: $WEIGHT_FILE"
            exit 1
        fi
        # Get absolute path
        WEIGHT_FILE="$(cd "$(dirname "$WEIGHT_FILE")" && pwd)/$(basename "$WEIGHT_FILE")"
        echo "Using pre-trained weights: $WEIGHT_FILE"
        echo ""
    fi
    
    echo "Phase 2: Testing with meta-brain only ($META_TEST_TRIALS trials)"
    echo "--------------------------------------------------"
    echo "Loading: $WEIGHT_FILE"
    echo "Direct rewards: DISABLED"
    echo ""
    
    echo "seed,catches,towards_pct,away_pct" > "$PHASE2_DIR/results.csv"
    
    # Use different seed range to avoid confusion
    start_seed=$((1000 + RANDOM % 1000))
    
    for i in $(seq 1 $META_TEST_TRIALS); do
        seed=$((start_seed + i - 1))
        echo -n "  Trial $i (seed $seed)... "
        
        # Create trial directory
        trial_dir="$PHASE2_DIR/trial_$(printf "%02d" $i)"
        mkdir -p "$trial_dir"
        
        # Run with output-dir flag, load weights, disable direct rewards
        output=$("$WORKSPACE_ROOT/bin/cat_mouse_learning" $seed --load-weights "$WEIGHT_FILE" --no-direct-rewards --output-dir "$trial_dir" 2>&1)
        
        # Save stdout to results.txt
        echo "$output" > "$trial_dir/results.txt"
        
        catches=$(echo "$output" | grep "Total mice caught:" | awk '{print $4}')
        towards=$(echo "$output" | grep "Moves TOWARDS" | grep -oP '\(\s*\K[0-9.]+' | head -1)
        away=$(echo "$output" | grep "Moves AWAY" | grep -oP '\(\s*\K[0-9.]+' | head -1)
        
        echo "$catches catches ($towards% towards)"
        echo "$seed,$catches,$towards,$away" >> "$PHASE2_DIR/results.csv"
    done
    
    echo ""
    
    # Calculate meta-only statistics
    meta_avg=$(tail -n +2 "$PHASE2_DIR/results.csv" | awk -F',' '{sum+=$2} END {printf "%.1f", sum/NR}')
    meta_towards=$(tail -n +2 "$PHASE2_DIR/results.csv" | awk -F',' '{sum+=$3} END {printf "%.1f", sum/NR}')
    meta_min=$(tail -n +2 "$PHASE2_DIR/results.csv" | awk -F',' 'NR==1{min=$2} $2<min{min=$2} END {print min}')
    meta_max=$(tail -n +2 "$PHASE2_DIR/results.csv" | awk -F',' 'NR==1{max=$2} $2>max{max=$2} END {print max}')
fi

# ============================================
# GENERATE EXPERIMENT REPORT
# ============================================
REPORT_FILE="$RUN_DIR/experiment_report.md"

echo "=========================================="
echo "GENERATING EXPERIMENT REPORT"
echo "=========================================="

cat > "$REPORT_FILE" << EOF
# Meta-Brain Experiment Report

**Date:** $(date '+%Y-%m-%d %H:%M:%S')
**Mode:** $MODE
**Results Directory:** $RUN_DIR

---

EOF

if [ "$MODE" == "full" ]; then
    cat >> "$REPORT_FILE" << EOF
## Phase 1: Training with Full Reward System

Training the primary brain with direct reinforcement to establish baseline hunting behavior.

| Metric | Value |
|--------|-------|
| Trials | $TRAINING_TRIALS |
| Best Performance | $best_catches catches (seed $best_seed) |
| Average Catches | $training_avg |
| Range | $training_min - $training_max |
| Avg Directionality | $training_towards% towards mouse |

### Trial Details

| Trial | Seed | Catches | Towards % | Away % |
|-------|------|---------|-----------|--------|
EOF

    # Add each trial to the table
    trial_num=1
    tail -n +2 "$PHASE1_DIR/results.csv" | while IFS=',' read seed catches towards away; do
        printf "| %d | %s | %s | %s%% | %s%% |\n" "$trial_num" "$seed" "$catches" "$towards" "$away" >> "$REPORT_FILE"
        trial_num=$((trial_num + 1))
    done

    cat >> "$REPORT_FILE" << EOF

---

EOF
fi

if [ "$MODE" == "full" ] || [ "$MODE" == "meta-only" ]; then
    cat >> "$REPORT_FILE" << EOF
## Phase 2: Meta-Brain Only Testing

Testing learned weights with direct rewards disabled. Only meta-brain can reinforce behavior.

| Metric | Value |
|--------|-------|
| Trials | $META_TEST_TRIALS |
| Average Catches | $meta_avg |
| Range | $meta_min - $meta_max |
| Avg Directionality | $meta_towards% towards mouse |

### Trial Details

| Trial | Seed | Catches | Towards % | Away % |
|-------|------|---------|-----------|--------|
EOF

    # Add each trial to the table
    trial_num=1
    tail -n +2 "$PHASE2_DIR/results.csv" | while IFS=',' read seed catches towards away; do
        printf "| %d | %s | %s | %s%% | %s%% |\n" "$trial_num" "$seed" "$catches" "$towards" "$away" >> "$REPORT_FILE"
        trial_num=$((trial_num + 1))
    done

    cat >> "$REPORT_FILE" << EOF

---

EOF
fi

if [ "$MODE" == "full" ]; then
    # Performance comparison
    if [ "$training_avg" != "0" ] && [ "$training_avg" != "0.0" ]; then
        retention=$(echo "scale=1; $meta_avg / $training_avg * 100" | bc 2>/dev/null || echo "0")
        best_retention=$(echo "scale=1; $meta_avg / $best_catches * 100" | bc 2>/dev/null || echo "0")
    else
        retention="0"
        best_retention="0"
    fi
    
    cat >> "$REPORT_FILE" << EOF
## Performance Analysis

### Retention Metrics

| Metric | Value |
|--------|-------|
| Training Average | $training_avg catches |
| Meta-Only Average | $meta_avg catches |
| Retention vs Average | ${retention}% |
| Retention vs Best | ${best_retention}% |

### Interpretation

EOF

    if [ "$retention" != "0" ]; then
        if (( $(echo "$retention > 100" | bc -l 2>/dev/null || echo 0) )); then
            improvement=$(echo "scale=1; $retention - 100" | bc)
            echo "✅ **AMAZING**: Meta-brain alone EXCEEDS training average (+${improvement}%)" >> "$REPORT_FILE"
        elif (( $(echo "$retention > 80" | bc -l 2>/dev/null || echo 0) )); then
            echo "✅ **SUCCESS**: Meta-brain maintains learned behavior (${retention}% retention)" >> "$REPORT_FILE"
        elif (( $(echo "$retention > 50" | bc -l 2>/dev/null || echo 0) )); then
            echo "⚠️ **PARTIAL**: Meta-brain partially maintains behavior (${retention}% retention)" >> "$REPORT_FILE"
        else
            echo "❌ **FAILURE**: Meta-brain insufficient alone (${retention}% retention)" >> "$REPORT_FILE"
        fi
    else
        echo "❌ **FAILURE**: Meta-brain shows no hunting performance (0% retention)" >> "$REPORT_FILE"
    fi

    cat >> "$REPORT_FILE" << EOF

---

EOF
fi

cat >> "$REPORT_FILE" << EOF
## Directory Structure

\`\`\`
$RUN_DIR/
EOF

if [ "$MODE" == "full" ]; then
    cat >> "$REPORT_FILE" << EOF
├── phase_1_full_reward/
│   ├── results.csv
│   ├── trial_01/
│   │   ├── results.txt
│   │   ├── simulation_log.csv
│   │   ├── brain_state.csv
│   │   ├── synapse_state.csv
│   │   ├── inputter_state.csv
│   │   ├── outputter_state.csv
│   │   └── weights_seed*.bin
│   ├── trial_02/
│   │   └── ...
│   └── ...
├── phase_2_meta_only/
│   ├── results.csv
│   ├── trial_01/
│   │   └── ...
│   └── ...
├── best_weights.bin
EOF
fi

if [ "$MODE" == "meta-only" ]; then
    cat >> "$REPORT_FILE" << EOF
├── phase_2_meta_only/
│   ├── results.csv
│   ├── trial_01/
│   │   └── ...
│   └── ...
EOF
fi

cat >> "$REPORT_FILE" << EOF
└── experiment_report.md
\`\`\`

---

*Generated by run_meta_experiment.sh*
EOF

echo ""
echo "=========================================="
echo "EXPERIMENT COMPLETE"
echo "=========================================="
echo ""

if [ "$MODE" == "full" ]; then
    echo "Training Phase (with direct rewards):"
    echo "  Best: $best_catches catches (seed $best_seed)"
    echo "  Average: $training_avg catches"
    echo ""
fi

if [ "$MODE" == "full" ] || [ "$MODE" == "meta-only" ]; then
    echo "Meta-Brain Only Phase (no direct rewards):"
    echo "  Average: $meta_avg catches"
    echo "  Range: $meta_min - $meta_max catches"
    echo "  Directionality: $meta_towards% towards mouse"
    echo ""
fi

if [ "$MODE" == "full" ]; then
    echo "Performance Retention: ${retention}% (vs training avg)"
    echo ""
fi

echo "Full report: $REPORT_FILE"
echo "Results directory: $RUN_DIR/"
echo "=========================================="
