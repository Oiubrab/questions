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
    ./$LEARNING_EXEC > "$RESULTS_DIR/trial_${trial}.log" 2>&1
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

all_early_dists = []
all_late_dists = []
all_improvements = []

print("\n" + "="*60)
print("INDIVIDUAL TRIAL RESULTS (20,000 Bars per trial)")
print("="*60)

for trial in range(1, num_trials + 1):
    filepath = f"{results_dir}/trial_{trial}.csv"
    
    bin_sums = [0.0] * 20
    bin_counts = [0] * 20
    
    with open(filepath, 'r') as f:
        reader = csv.reader(f)
        next(reader)  # Skip header
        
        for row in reader:
            bar = int(row[0])
            mouse_x, mouse_y = float(row[1]), float(row[2])
            cat_x, cat_y = float(row[3]), float(row[4])
            
            # Calculate distance with toroidal wrapping
            dx = mouse_x - cat_x
            dy = mouse_y - cat_y
            if abs(dx) > 50: dx = dx - 100 if dx > 0 else dx + 100
            if abs(dy) > 50: dy = dy - 100 if dy > 0 else dy + 100
            dist = math.sqrt(dx*dx + dy*dy)
            
            bin_idx = (bar - 1) // 1000
            bin_sums[bin_idx] += dist
            bin_counts[bin_idx] += 1
    
    # Calculate averages
    bin_avgs = [bin_sums[i]/bin_counts[i] if bin_counts[i] > 0 else 0 
                for i in range(20)]
    
    early_avg = (bin_sums[0] + bin_sums[1]) / (bin_counts[0] + bin_counts[1])
    late_avg = (bin_sums[18] + bin_sums[19]) / (bin_counts[18] + bin_counts[19])
    improvement = ((early_avg - late_avg) / early_avg) * 100
    
    all_early_dists.append(early_avg)
    all_late_dists.append(late_avg)
    all_improvements.append(improvement)
    
    print(f"\nTrial {trial}:")
    print(f"  Early (1-2000):      {early_avg:.2f}")
    print(f"  Late (18001-20000):  {late_avg:.2f}")
    print(f"  Improvement:         {improvement:+.1f}%")

# Calculate overall statistics
avg_early = sum(all_early_dists) / len(all_early_dists)
avg_late = sum(all_late_dists) / len(all_late_dists)
avg_improvement = sum(all_improvements) / len(all_improvements)

# Calculate standard deviation
std_improvement = math.sqrt(sum((x - avg_improvement)**2 for x in all_improvements) / len(all_improvements))

print("\n" + "="*60)
print("AGGREGATE RESULTS ACROSS ALL TRIALS")
print("="*60)
print(f"Average Early Distance:  {avg_early:.2f} ± {math.sqrt(sum((x-avg_early)**2 for x in all_early_dists)/len(all_early_dists)):.2f}")
print(f"Average Late Distance:   {avg_late:.2f} ± {math.sqrt(sum((x-avg_late)**2 for x in all_late_dists)/len(all_late_dists)):.2f}")
print(f"Average Improvement:     {avg_improvement:+.1f}% ± {std_improvement:.1f}%")
print(f"\nLearning outcomes:")
for i, imp in enumerate(all_improvements, 1):
    status = "✓ Learning" if imp > 5 else "✗ No learning"
    print(f"  Trial {i}: {imp:+6.1f}%  {status}")

# Statistical significance
learning_trials = sum(1 for x in all_improvements if x > 5)
print(f"\nTrials showing learning (>5%): {learning_trials}/{num_trials} ({learning_trials/num_trials*100:.0f}%)")

if avg_improvement > 10:
    print("\n✓✓✓ STRONG CONSISTENT LEARNING DETECTED")
elif avg_improvement > 5:
    print("\n✓✓ MODERATE LEARNING DETECTED")
elif avg_improvement > 0:
    print("\n✓ WEAK LEARNING DETECTED")
else:
    print("\n✗ NO LEARNING DETECTED")

PYTHON_SCRIPT

echo ""
echo "==================================="
echo "RUNNING GUI VISUALIZATION"
echo "==================================="
echo "Starting GUI demo to show visual learning..."
echo "Press ESC or Q to quit early"
echo ""

# Compile and run GUI
$FC trinary_module.f90 synapses_module.f90 outputter_module.f90 inputter_module.f90 brain_module.f90 vision_simulation_module.f90 cat_mouse_gui_demo.f90 -o $GUI_EXEC 2>/dev/null
./$GUI_EXEC | python3 cat_mouse_gui.py

echo ""
echo "==================================="
echo "EXPERIMENT COMPLETE"
echo "==================================="
echo "Results saved in: $RESULTS_DIR/"
