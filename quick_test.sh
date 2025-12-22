#!/bin/bash
# Quick 5-trial test with enhanced learning parameters

NUM_TRIALS=5
RESULTS_DIR="learning_results"
SHOW_GUI=false

echo "==================================="
echo "ENHANCED LEARNING PARAMETER TEST"
echo "==================================="
echo "Running $NUM_TRIALS trials with stronger learning..."
echo ""

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# Use gfortran by default, nvfortran if available
if command -v nvfortran &> /dev/null; then
    COMPILER=nvfortran
    LEARNING_EXEC=cat_mouse_learning
elif command -v gfortran &> /dev/null; then
    COMPILER=gfortran
    LEARNING_EXEC=cat_mouse_learning
else
    echo "ERROR: No suitable Fortran compiler found!"
    exit 1
fi

echo "Using compiler: $COMPILER"
echo "Learning executable: $LEARNING_EXEC"
echo ""

# Run trials
for trial in $(seq 1 $NUM_TRIALS); do
    echo "--- Trial $trial/$NUM_TRIALS ---"
    
    # Run learning simulation with trial-specific seed
    ./"$LEARNING_EXEC" $((1000 + trial)) > "$RESULTS_DIR/trial_${trial}.log" 2>&1
    
    # Move the CSV file with correct name
    if [ -f "simulation_log.csv" ]; then
        mv "simulation_log.csv" "$RESULTS_DIR/trial_${trial}.csv"
    else
        echo "Warning: simulation_log.csv not found for trial $trial"
    fi
    
    # Analyze for oscillation patterns
    echo "Analyzing for oscillation patterns..."
    python3 detect_oscillation.py "$RESULTS_DIR/trial_${trial}.csv" > "$RESULTS_DIR/trial_${trial}_oscillation.txt" 2>&1
    
    echo "Trial $trial complete"
done

# Quick analysis
echo ""
echo "==================================="
echo "QUICK ANALYSIS"
echo "==================================="

python3 << 'PYTHON_SCRIPT'
import os
import csv
import math

results_dir = "learning_results"
num_trials = 5

print("Enhanced Parameter Test Results:")
print("=" * 50)

trial_catches = []
trial_towards_pct = []
trial_away_pct = []

for trial in range(1, num_trials + 1):
    filepath = f"{results_dir}/trial_{trial}.csv"
    
    if os.path.exists(filepath):
        with open(filepath, 'r') as f:
            reader = csv.reader(f)
            next(reader)  # Skip header
            
            rows = list(reader)
            num_bars = len(rows)
            
            if num_bars > 0:
                last_row = rows[-1]
                catches = int(last_row[-1])  # Last column is catches count
                trial_catches.append(catches)
                
                catch_rate = catches / num_bars
                print(f"Trial {trial}: {catches:3d} catches in {num_bars} Bars (rate: {catch_rate:.4f})")
                
                # Extract directionality data from log
                logfile = f"{results_dir}/trial_{trial}.log"
                if os.path.exists(logfile):
                    with open(logfile, 'r') as lf:
                        content = lf.read()
                        if "Moves TOWARDS mouse:" in content:
                            for line in content.split('\n'):
                                if "Moves TOWARDS mouse:" in line:
                                    pct = float(line.split('(')[1].split('%')[0].strip())
                                    trial_towards_pct.append(pct)
                                    break
                            for line in content.split('\n'):
                                if "Moves AWAY from mouse:" in line:
                                    pct = float(line.split('(')[1].split('%')[0].strip())
                                    trial_away_pct.append(pct)
                                    break
            else:
                trial_catches.append(0)
                print(f"Trial {trial}: ERROR - No data")
    else:
        trial_catches.append(0)
        print(f"Trial {trial}: ERROR - File missing")

if len(trial_catches) > 0 and max(trial_catches) > 0:
    avg_catches = sum(trial_catches) / len(trial_catches)
    print(f"\nAverage catches: {avg_catches:.1f}")
    print(f"Range: {min(trial_catches)} to {max(trial_catches)} catches")
    
    if len(trial_towards_pct) > 0:
        avg_towards = sum(trial_towards_pct) / len(trial_towards_pct)
        avg_away = sum(trial_away_pct) / len(trial_away_pct)
        print(f"\nDirectionality:")
        print(f"  Average TOWARDS: {avg_towards:.1f}%")
        print(f"  Average AWAY: {avg_away:.1f}%")
        print(f"  Improvement over baseline (61.1%): {avg_towards - 61.1:+.1f} percentage points")
        
        if avg_towards > 75:
            print(f"  ✓✓✓ EXCELLENT DIRECTIONAL LEARNING")
        elif avg_towards > 70:
            print(f"  ✓✓ STRONG DIRECTIONAL LEARNING")
        elif avg_towards > 65:
            print(f"  ✓ GOOD DIRECTIONAL LEARNING")
        elif avg_towards > 55:
            print(f"  ~ MODERATE DIRECTIONAL LEARNING")
        else:
            print(f"  ✗ POOR DIRECTIONAL LEARNING")
else:
    print("ERROR: No valid trial data")

PYTHON_SCRIPT

echo ""
echo "==================================="
echo "ENHANCED TEST COMPLETE"
echo "==================================="