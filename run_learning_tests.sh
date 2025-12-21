#!/bin/bash
# Run multiple learning trials and analyze results

NUM_TRIALS=5
RESULTS_DIR="learning_results"

echo "==================================="
echo "MULTI-TRIAL LEARNING EXPERIMENT"
echo "==================================="
echo "Running $NUM_TRIALS trials..."
echo ""

# Detect available Fortran compiler
if command -v nvfortran >/dev/null 2>&1; then
    FC="nvfortran"
    LEARNING_EXEC="cat_mouse_learning"
    GUI_EXEC="cat_mouse_gui_demo"
else
    FC="gfortran"
    LEARNING_EXEC="cat_mouse_learning"
    GUI_EXEC="cat_mouse_gui_demo"
fi

echo "Using compiler: $FC"
echo "Learning executable: $LEARNING_EXEC"
echo ""

# Create results directory
mkdir -p "$RESULTS_DIR"

# Run multiple trials
for trial in $(seq 1 $NUM_TRIALS); do
    echo "--- Trial $trial/$NUM_TRIALS ---"
    RANDOM_SEED=$((1000 + trial * 12345))  # Different seed per trial
    ./$LEARNING_EXEC $RANDOM_SEED > "$RESULTS_DIR/trial_${trial}.log" 2>&1
    mv simulation_log.csv "$RESULTS_DIR/trial_${trial}.csv"
    echo "Trial $trial complete"
done

echo ""
echo "==================================="
echo "ANALYZING RESULTS"
echo "==================================="

# Analyze all trials
python3 << 'PYTHON_SCRIPT'
import os
import csv
import math

results_dir = "learning_results"
num_trials = 5

print("\n" + "="*60)
print("INDIVIDUAL TRIAL RESULTS")
print("="*60)

trial_bars = []
trial_success = []

for trial in range(1, num_trials + 1):
    filepath = f"{results_dir}/trial_{trial}.csv"
    
    with open(filepath, 'r') as f:
        reader = csv.reader(f)
        next(reader)  # Skip header
        
        rows = list(reader)
        num_bars = len(rows)
        
        if num_bars > 0:
            last_row = rows[-1]
            mouse_x, mouse_y = float(last_row[1]), float(last_row[2])
            cat_x, cat_y = float(last_row[3]), float(last_row[4])
            
            dx = mouse_x - cat_x
            dy = mouse_y - cat_y
            final_distance = math.sqrt(dx*dx + dy*dy)
            
            success = final_distance < 2.0
            trial_success.append(success)
            trial_bars.append(num_bars)
            
            status = "✓ SUCCESS" if success else "✗ FAILED"
            print(f"Trial {trial}: {status} - Completed in {num_bars} Bars (distance: {final_distance:.2f})")
        else:
            print(f"Trial {trial}: ERROR - No data")
            trial_success.append(False)
            trial_bars.append(0)

print("\n" + "="*60)
print("AGGREGATE RESULTS")
print("="*60)

success_count = sum(trial_success)
if success_count > 0:
    avg_bars = sum(b for b, s in zip(trial_bars, trial_success) if s) / success_count
    print(f"Success rate: {success_count}/{num_trials} ({success_count/num_trials*100:.0f}%)")
    print(f"Average bars to success: {avg_bars:.0f}")
    
    if success_count == num_trials:
        print("\n✓✓✓ PERFECT LEARNING - All trials succeeded!")
    elif success_count >= num_trials * 0.8:
        print("\n✓✓ STRONG LEARNING - Most trials succeeded")
    elif success_count >= num_trials * 0.5:
        print("\n✓ MODERATE LEARNING - Some trials succeeded")
    else:
        print("\n✗ WEAK LEARNING - Few trials succeeded")
else:
    print(f"Success rate: 0/{num_trials} (0%)")
    print("\n✗ NO LEARNING DETECTED")

PYTHON_SCRIPT

echo ""
echo "==================================="
echo "RUNNING GUI VISUALIZATION"
echo "==================================="
echo "Replaying Trial 1 with GUI visualization..."
echo "Press ESC or Q to quit early"
echo ""

# Replay trial 1 CSV through GUI (using existing logged data)
python3 cat_mouse_gui.py < "$RESULTS_DIR/trial_1.csv"

echo ""
echo "==================================="
echo "BRAIN STATE VISUALIZATION"
echo "==================================="
echo "Creating visualization of final brain state..."
echo ""

# Move brain state files from last trial
if [ -f brain_state.csv ]; then
    mv brain_state.csv "$RESULTS_DIR/final_brain_state.csv"
    mv synapse_state.csv "$RESULTS_DIR/final_synapse_state.csv"
    mv inputter_state.csv "$RESULTS_DIR/inputter_state.csv"
    mv outputter_state.csv "$RESULTS_DIR/outputter_state.csv"
    
    # Create visualization
    python3 visualize_brain.py "$RESULTS_DIR/final_brain_state.csv" \
                              "$RESULTS_DIR/final_synapse_state.csv" \
                              "$RESULTS_DIR/brain_visualization.png"
    
    echo "Brain visualization saved to: $RESULTS_DIR/brain_visualization.png"
else
    echo "Warning: No brain state files found"
fi

echo ""
echo "==================================="
echo "EXPERIMENT COMPLETE"
echo "==================================="
echo "Results saved in: $RESULTS_DIR/"
