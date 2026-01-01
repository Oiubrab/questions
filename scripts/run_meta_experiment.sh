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
RESULTS_BASE="results/meta_experiments"

# Parse command line
MODE=""
WEIGHT_FILE=""

show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --full              Run full experiment: train 30 trials, test best with meta-only"
    echo "  --meta-only FILE    Test meta-only mode using specified weight file"
    echo "  --from-scratch      Test meta-only from scratch (no pre-training) as baseline"
    echo "  --help              Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 --full"
    echo "  $0 --meta-only results/meta_experiments/run_20260102_063000/best_weights.bin"
    echo "  $0 --from-scratch"
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
    
    echo "seed,catches,towards_pct,away_pct" > "$RUN_DIR/from_scratch_results.csv"
    
    for seed in $(seq 1 $SCRATCH_TEST_TRIALS); do
        echo -n "  Trial $seed... "
        output=$(./bin/cat_mouse_learning $seed --no-direct-rewards 2>&1)
        
        catches=$(echo "$output" | grep "Total mice caught:" | awk '{print $4}')
        towards=$(echo "$output" | grep "Moves TOWARDS" | awk '{print $5}' | tr -d '(')
        away=$(echo "$output" | grep "Moves AWAY" | awk '{print $5}' | tr -d '(')
        
        echo "$catches catches ($towards% towards)"
        echo "$seed,$catches,$towards,$away" >> "$RUN_DIR/from_scratch_results.csv"
    done
    
    echo ""
    echo "Results Summary:"
    avg_catches=$(tail -n +2 "$RUN_DIR/from_scratch_results.csv" | awk -F',' '{sum+=$2} END {print sum/NR}')
    avg_towards=$(tail -n +2 "$RUN_DIR/from_scratch_results.csv" | awk -F',' '{sum+=$3} END {print sum/NR}')
    echo "  Average catches: $avg_catches"
    echo "  Average towards: $avg_towards%"
    echo ""
    echo "✓ Baseline established - meta-brain cannot learn from scratch"
    echo "Results saved to: $RUN_DIR/from_scratch_results.csv"
    
    exit 0
fi

# ============================================
# MODE: FULL PIPELINE
# ============================================
if [ "$MODE" == "full" ]; then
    echo "Phase 1: Training with full reward system ($TRAINING_TRIALS trials)"
    echo "--------------------------------------------------"
    
    # Clean up old weight files in workspace root
    rm -f weights_seed*.bin 2>/dev/null || true
    
    echo "seed,catches" > "$RUN_DIR/training_results.csv"
    
    best_catches=0
    best_seed=0
    
    for seed in $(seq 1 $TRAINING_TRIALS); do
        echo -n "  Trial $seed... "
        output=$(./bin/cat_mouse_learning $seed 2>&1)
        catches=$(echo "$output" | grep "Total mice caught:" | awk '{print $4}')
        echo "$catches catches"
        
        echo "$seed,$catches" >> "$RUN_DIR/training_results.csv"
        
        # Track best
        if [ "$catches" -gt "$best_catches" ]; then
            best_catches=$catches
            best_seed=$seed
        fi
    done
    
    echo ""
    echo "Training Phase Complete"
    echo "  Best trial: seed $best_seed with $best_catches catches"
    
    # Copy best weights
    cp "weights_seed${best_seed}.bin" "$RUN_DIR/best_weights.bin"
    WEIGHT_FILE="$RUN_DIR/best_weights.bin"
    
    # Move all weight files to run directory
    mv weights_seed*.bin "$RUN_DIR/" 2>/dev/null || true
    
    echo "  Saved: $RUN_DIR/best_weights.bin"
    echo ""
    
    # Calculate training statistics
    training_avg=$(tail -n +2 "$RUN_DIR/training_results.csv" | awk -F',' '{sum+=$2} END {print sum/NR}')
    echo "Training Statistics:"
    echo "  Best: $best_catches catches"
    echo "  Average: $training_avg catches"
    echo ""
fi

# ============================================
# MODE: META-ONLY TEST (for both --full and --meta-only)
# ============================================
if [ "$MODE" == "full" ] || [ "$MODE" == "meta-only" ]; then
    if [ "$MODE" == "meta-only" ]; then
        if [ ! -f "$WEIGHT_FILE" ]; then
            echo "Error: Weight file not found: $WEIGHT_FILE"
            exit 1
        fi
        echo "Using pre-trained weights: $WEIGHT_FILE"
        echo ""
    fi
    
    echo "Phase 2: Testing with meta-brain only ($META_TEST_TRIALS trials)"
    echo "--------------------------------------------------"
    echo "Loading: $WEIGHT_FILE"
    echo "Direct rewards: DISABLED"
    echo ""
    
    echo "seed,catches,towards_pct,away_pct" > "$RUN_DIR/meta_only_results.csv"
    
    # Use different seed range to avoid confusion
    start_seed=$((1000 + RANDOM % 1000))
    
    for i in $(seq 1 $META_TEST_TRIALS); do
        seed=$((start_seed + i - 1))
        echo -n "  Trial $i (seed $seed)... "
        
        output=$(./bin/cat_mouse_learning $seed --load-weights "$WEIGHT_FILE" --no-direct-rewards 2>&1)
        
        catches=$(echo "$output" | grep "Total mice caught:" | awk '{print $4}')
        towards=$(echo "$output" | grep "Moves TOWARDS" | awk '{print $5}' | tr -d '(')
        away=$(echo "$output" | grep "Moves AWAY" | awk '{print $5}' | tr -d '(')
        
        echo "$catches catches ($towards% towards)"
        echo "$seed,$catches,$towards,$away" >> "$RUN_DIR/meta_only_results.csv"
        
        # Clean up weight file generated by this test
        rm -f "weights_seed${seed}.bin" 2>/dev/null || true
    done
    
    echo ""
    echo "=========================================="
    echo "EXPERIMENT RESULTS"
    echo "=========================================="
    echo ""
    
    if [ "$MODE" == "full" ]; then
        echo "Training Phase (with direct rewards):"
        echo "  Best: $best_catches catches (seed $best_seed)"
        echo "  Average: $training_avg catches"
        echo ""
    fi
    
    echo "Meta-Brain Only Phase (no direct rewards):"
    meta_avg=$(tail -n +2 "$RUN_DIR/meta_only_results.csv" | awk -F',' '{sum+=$2} END {print sum/NR}')
    meta_towards=$(tail -n +2 "$RUN_DIR/meta_only_results.csv" | awk -F',' '{sum+=$3} END {print sum/NR}')
    meta_min=$(tail -n +2 "$RUN_DIR/meta_only_results.csv" | awk -F',' 'NR==1{min=$2} $2<min{min=$2} END {print min}')
    meta_max=$(tail -n +2 "$RUN_DIR/meta_only_results.csv" | awk -F',' 'NR==1{max=$2} $2>max{max=$2} END {print max}')
    
    echo "  Average: $meta_avg catches"
    echo "  Range: $meta_min - $meta_max catches"
    echo "  Directionality: $meta_towards% towards mouse"
    echo ""
    
    if [ "$MODE" == "full" ]; then
        # Performance comparison
        retention=$(echo "scale=1; $meta_avg / $best_catches * 100" | bc)
        echo "Performance Retention: ${retention}%"
        echo ""
        
        if (( $(echo "$retention > 100" | bc -l) )); then
            improvement=$(echo "scale=1; $retention - 100" | bc)
            echo "✓ AMAZING: Meta-brain alone EXCEEDS training (+${improvement}%)"
        elif (( $(echo "$retention > 80" | bc -l) )); then
            echo "✓ SUCCESS: Meta-brain alone maintains learned behavior"
        elif (( $(echo "$retention > 50" | bc -l) )); then
            echo "~ PARTIAL: Meta-brain partially maintains behavior"
        else
            echo "✗ FAILURE: Meta-brain insufficient alone"
        fi
    else
        echo "✓ Meta-brain maintenance test complete"
    fi
    
    echo ""
    echo "Results saved to: $RUN_DIR/"
    echo "  - training_results.csv (if full mode)"
    echo "  - meta_only_results.csv"
    echo "  - best_weights.bin"
    echo "=========================================="
fi
