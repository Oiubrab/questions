#!/bin/bash
# Run multiple learning trials and analyze results

NUM_TRIALS=30
RESULTS_DIR="learning_results"
SHOW_GUI=true

# Parse command line arguments
if [[ "$1" == "--no-gui" ]] || [[ "$1" == "-n" ]]; then
    SHOW_GUI=false
fi

echo "==================================="
echo "MULTI-TRIAL LEARNING EXPERIMENT"
echo "==================================="
echo "Running $NUM_TRIALS trials..."
if [ "$SHOW_GUI" = false ]; then
    echo "(GUI visualization disabled)"
fi
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
    
    # Run oscillation detection immediately after each trial
    echo "Analyzing for oscillation patterns..."
    python3 detect_oscillation.py "$RESULTS_DIR/trial_${trial}.csv" > "$RESULTS_DIR/trial_${trial}_oscillation.txt"
    
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
num_trials = 30

print("\n" + "="*60)
print("INDIVIDUAL TRIAL RESULTS (CONTINUOUS HUNTING)")
print("="*60)

trial_catches = []
trial_towards_pct = []
trial_away_pct = []

for trial in range(1, num_trials + 1):
    filepath = f"{results_dir}/trial_{trial}.csv"
    
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
            
            # Only print every 5th trial to keep output manageable
            if trial % 5 == 0 or trial <= 3:
                print(f"Trial {trial:2d}: {catches:3d} catches in {num_bars} Bars (rate: {catch_rate:.4f} per Bar)")
        else:
            print(f"Trial {trial}: ERROR - No data")
            trial_catches.append(0)

# Now read log files for directionality data
epoch_data = []  # Store (trial, epoch_num, catches) tuples
epoch_directionality = []  # Store (trial, epoch_num, towards_pct, away_pct, perp_pct)
for trial in range(1, num_trials + 1):
    logfile = f"{results_dir}/trial_{trial}.log"
    if os.path.exists(logfile):
        with open(logfile, 'r') as f:
            content = f.read()
            # Extract movement stats
            if "Moves TOWARDS mouse:" in content:
                for line in content.split('\n'):
                    if "Moves TOWARDS mouse:" in line:
                        # Parse: "Moves TOWARDS mouse:         10862 (    64.07126     %)"
                        pct = float(line.split('(')[1].split('%')[0].strip())
                        trial_towards_pct.append(pct)
                        break
                for line in content.split('\n'):
                    if "Moves AWAY from mouse:" in line:
                        pct = float(line.split('(')[1].split('%')[0].strip())
                        trial_away_pct.append(pct)
                        break
            # Extract epoch data
            if "Epoch " in content:
                lines = content.split('\n')
                i = 0
                while i < len(lines):
                    line = lines[i]
                    if line.strip().startswith("Epoch") and ":" in line and "catches" in line:
                        # Parse: "  Epoch            1 :           20 catches"
                        # Skip lines like "Epoch size:         2000 Bars"
                        parts = line.split(':')
                        if len(parts) >= 2 and "size" not in parts[0].lower():
                            try:
                                epoch_num = int(parts[0].split()[-1])
                                catches = int(parts[1].split()[0])
                                epoch_data.append((trial, epoch_num, catches))
                                
                                # Check if next line has directionality data
                                if i + 1 < len(lines):
                                    next_line = lines[i + 1]
                                    if "Directionality:" in next_line:
                                        # Parse: "Directionality:    64.18324     % towards,    31.37729     % away,"
                                        # and next line: "   4.439459     % perpendicular"
                                        parts_dir = next_line.split('%')
                                        if len(parts_dir) >= 3:
                                            towards_pct = float(parts_dir[0].split()[-1])
                                            away_pct = float(parts_dir[1].split()[-1])
                                            # Check if perpendicular is on same line or next
                                            if "perpendicular" in next_line:
                                                perp_pct = float(parts_dir[2].split()[0])
                                            elif i + 2 < len(lines) and "perpendicular" in lines[i + 2]:
                                                perp_pct = float(lines[i + 2].split('%')[0].strip())
                                            else:
                                                perp_pct = 0.0
                                            epoch_directionality.append((trial, epoch_num, towards_pct, away_pct, perp_pct))
                            except (ValueError, IndexError):
                                pass  # Skip malformed lines
                    i += 1

print("\n" + "="*60)
print("AGGREGATE RESULTS")
print("="*60)

if len(trial_catches) > 0:
    avg_catches = sum(trial_catches) / len(trial_catches)
    print(f"Average catches per trial: {avg_catches:.1f}")
    
    if len(trial_catches) > 1:
        variance = sum((c - avg_catches)**2 for c in trial_catches) / len(trial_catches)
        std_dev = math.sqrt(variance)
        print(f"Standard deviation: {std_dev:.1f}")
        print(f"Range: {min(trial_catches)} to {max(trial_catches)} catches")
        print(f"Median: {sorted(trial_catches)[len(trial_catches)//2]}")
    
    # Directionality statistics
    if len(trial_towards_pct) > 0:
        avg_towards = sum(trial_towards_pct) / len(trial_towards_pct)
        avg_away = sum(trial_away_pct) / len(trial_away_pct)
        print(f"\nDirectionality:")
        print(f"  Average TOWARDS: {avg_towards:.1f}%")
        print(f"  Average AWAY: {avg_away:.1f}%")
        print(f"  Towards/Away ratio: {avg_towards/avg_away:.2f}:1")
        
        if avg_towards > 60:
            print(f"  ✓✓ STRONG DIRECTIONAL LEARNING")
        elif avg_towards > 55:
            print(f"  ✓ MODERATE DIRECTIONAL LEARNING")
        elif avg_towards > 50:
            print(f"  ~ WEAK DIRECTIONAL LEARNING")
        else:
            print(f"  ✗ NO DIRECTIONAL LEARNING")
    
    if avg_catches >= 300:
        print("\n✓✓✓ EXCEPTIONAL PERFORMANCE - Cat is an expert hunter!")
    elif avg_catches >= 200:
        print("\n✓✓✓ EXCELLENT PERFORMANCE - Cat consistently catches many mice!")
    elif avg_catches >= 100:
        print("\n✓✓ STRONG PERFORMANCE - Cat catching mice reliably")
    elif avg_catches >= 50:
        print("\n✓ MODERATE PERFORMANCE - Cat learning to catch mice")
    elif avg_catches >= 20:
        print("\n~ WEAK PERFORMANCE - Cat occasionally catches mice")
    else:
        print("\n✗ NO LEARNING DETECTED - Cat not catching mice")
    
    # Analyze temporal learning trends
    if len(epoch_data) > 0:
        print("\n" + "="*60)
        print("TEMPORAL LEARNING ANALYSIS (ACROSS ALL TRIALS)")
        print("="*60)
        
        # Group by epoch number and average
        from collections import defaultdict
        epoch_avgs = defaultdict(list)
        for trial, epoch_num, catches in epoch_data:
            epoch_avgs[epoch_num].append(catches)
        
        max_epoch = max(epoch_avgs.keys())
        print(f"\nAverage catches per epoch (across {num_trials} trials):")
        for epoch in sorted(epoch_avgs.keys()):
            avg = sum(epoch_avgs[epoch]) / len(epoch_avgs[epoch])
            print(f"  Epoch {epoch:2d}: {avg:5.1f} catches")
        
        # Calculate trend (early vs late)
        early_epochs = [e for e in range(1, max_epoch//3 + 1) if e in epoch_avgs]
        late_epochs = [e for e in range(2*max_epoch//3 + 1, max_epoch + 1) if e in epoch_avgs]
        
        if early_epochs and late_epochs:
            early_avg = sum(sum(epoch_avgs[e]) for e in early_epochs) / sum(len(epoch_avgs[e]) for e in early_epochs)
            late_avg = sum(sum(epoch_avgs[e]) for e in late_epochs) / sum(len(epoch_avgs[e]) for e in late_epochs)
            improvement = ((late_avg - early_avg) / early_avg) * 100
            
            print(f"\nTemporal trend:")
            print(f"  Early epochs (1-{max_epoch//3}): {early_avg:.1f} avg catches")
            print(f"  Late epochs ({2*max_epoch//3+1}-{max_epoch}): {late_avg:.1f} avg catches")
            print(f"  Improvement: {improvement:+.1f}%")
            
            if improvement > 20:
                print("  ✓✓ STRONG TEMPORAL LEARNING")
            elif improvement > 5:
                print("  ✓ MODERATE TEMPORAL LEARNING")
            elif improvement > -5:
                print("  ~ STABLE PERFORMANCE")
            else:
                print("  ✗ PERFORMANCE DECLINE")
    
    # Analyze per-epoch directionality
    if len(epoch_directionality) > 0:
        print("\n" + "="*60)
        print("PER-EPOCH DIRECTIONALITY (ACROSS ALL TRIALS)")
        print("="*60)
        
        # Group by epoch number and average
        epoch_dir_avgs = defaultdict(lambda: {'towards': [], 'away': [], 'perp': []})
        for trial, epoch_num, towards_pct, away_pct, perp_pct in epoch_directionality:
            epoch_dir_avgs[epoch_num]['towards'].append(towards_pct)
            epoch_dir_avgs[epoch_num]['away'].append(away_pct)
            epoch_dir_avgs[epoch_num]['perp'].append(perp_pct)
        
        print(f"\nAverage directionality per epoch (across {num_trials} trials):")
        print(f"{'Epoch':>6} {'Towards':>10} {'Away':>10} {'Perp':>10} {'T:A Ratio':>11}")
        print("-" * 60)
        for epoch in sorted(epoch_dir_avgs.keys()):
            towards = sum(epoch_dir_avgs[epoch]['towards']) / len(epoch_dir_avgs[epoch]['towards'])
            away = sum(epoch_dir_avgs[epoch]['away']) / len(epoch_dir_avgs[epoch]['away'])
            perp = sum(epoch_dir_avgs[epoch]['perp']) / len(epoch_dir_avgs[epoch]['perp'])
            ratio = towards / away if away > 0 else 0
            print(f"{epoch:6d} {towards:9.1f}% {away:9.1f}% {perp:9.1f}% {ratio:10.2f}:1")
        
        # Calculate directionality trend
        if len(epoch_dir_avgs) >= 3:
            early_dir_epochs = [e for e in range(1, max_epoch//3 + 1) if e in epoch_dir_avgs]
            late_dir_epochs = [e for e in range(2*max_epoch//3 + 1, max_epoch + 1) if e in epoch_dir_avgs]
            
            if early_dir_epochs and late_dir_epochs:
                early_towards = sum(sum(epoch_dir_avgs[e]['towards']) for e in early_dir_epochs) / sum(len(epoch_dir_avgs[e]['towards']) for e in early_dir_epochs)
                late_towards = sum(sum(epoch_dir_avgs[e]['towards']) for e in late_dir_epochs) / sum(len(epoch_dir_avgs[e]['towards']) for e in late_dir_epochs)
                dir_improvement = late_towards - early_towards
                
                print(f"\nDirectionality trend:")
                print(f"  Early epochs: {early_towards:.1f}% towards")
                print(f"  Late epochs: {late_towards:.1f}% towards")
                print(f"  Improvement: {dir_improvement:+.1f} percentage points")
                
                if dir_improvement > 5:
                    print("  ✓✓ STRONG DIRECTIONAL IMPROVEMENT")
                elif dir_improvement > 2:
                    print("  ✓ MODERATE DIRECTIONAL IMPROVEMENT")
                elif dir_improvement > -2:
                    print("  ~ STABLE DIRECTIONALITY")
                else:
                    print("  ✗ DIRECTIONAL DECLINE")
else:
    print("ERROR: No trial data")

PYTHON_SCRIPT

echo ""
echo "==================================="
echo "OSCILLATION ANALYSIS SUMMARY"
echo "==================================="
echo "Checking for stuck/oscillating patterns..."
echo ""

# Display oscillation warnings (only if GUI enabled, otherwise too much output)
if [ "$SHOW_GUI" = true ]; then
    for trial in $(seq 1 $NUM_TRIALS); do
        if grep -q "WARNING" "$RESULTS_DIR/trial_${trial}_oscillation.txt" 2>/dev/null; then
            echo "⚠️  Trial $trial showed oscillation patterns:"
            grep "WARNING" "$RESULTS_DIR/trial_${trial}_oscillation.txt" | head -3
            echo ""
        fi
    done
else
    # Just count warnings when not showing GUI
    warning_count=$(grep -l "WARNING" "$RESULTS_DIR"/trial_*_oscillation.txt 2>/dev/null | wc -l)
    echo "$warning_count out of $NUM_TRIALS trials showed oscillation patterns"
fi

echo "Full oscillation reports saved to: $RESULTS_DIR/trial_*_oscillation.txt"

if [ "$SHOW_GUI" = true ]; then
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
fi

echo ""
echo "==================================="
echo "EXPERIMENT COMPLETE"
echo "==================================="
echo "Results saved in: $RESULTS_DIR/"
if [ "$SHOW_GUI" = false ]; then
    echo "(Use without --no-gui flag to see visualizations)"
fi
