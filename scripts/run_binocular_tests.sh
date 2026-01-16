#!/bin/bash
# Run multiple binocular learning trials and analyze results

NUM_TRIALS=30
RESULTS_DIR="results/binocular"
SHOW_GUI=true

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-gui|-n)
            SHOW_GUI=false
            shift
            ;;
        --trials|-t)
            NUM_TRIALS="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --trials, -t NUM    Number of trials to run (default: 30)"
            echo "  --no-gui, -n        Disable GUI visualization"
            echo "  --help, -h          Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0                  # Run 30 trials with GUI"
            echo "  $0 -t 1            # Run single trial with GUI"
            echo "  $0 -t 5 --no-gui   # Run 5 trials without GUI"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Validate number of trials
if ! [[ "$NUM_TRIALS" =~ ^[0-9]+$ ]] || [ "$NUM_TRIALS" -lt 1 ]; then
    echo "Error: Number of trials must be a positive integer"
    exit 1
fi

echo "==================================="
if [ "$NUM_TRIALS" -eq 1 ]; then
    echo "SINGLE BINOCULAR TRIAL EXPERIMENT"
else
    echo "MULTI-TRIAL BINOCULAR EXPERIMENT"
fi
echo "==================================="
echo "Running $NUM_TRIALS trial(s)..."
if [ "$SHOW_GUI" = false ]; then
    echo "(GUI visualization disabled)"
fi
echo ""

# Detect available Fortran compiler
if command -v nvfortran >/dev/null 2>&1; then
    FC="nvfortran"
else
    FC="gfortran"
fi

BINOCULAR_EXEC="bin/binocular_cat_mouse"

echo "Using compiler: $FC"
echo "Binocular executable: $BINOCULAR_EXEC"
echo ""

# Check if executable exists
if [ ! -f "$BINOCULAR_EXEC" ]; then
    echo "Error: $BINOCULAR_EXEC not found. Building..."
    make binocular
    if [ $? -ne 0 ]; then
        echo "Build failed!"
        exit 1
    fi
fi

# Create results directory
mkdir -p "$RESULTS_DIR"

# Run multiple trials
for trial in $(seq 1 $NUM_TRIALS); do
    echo "--- Trial $trial/$NUM_TRIALS ---"
    RANDOM_SEED=$((1000 + trial * 12345))  # Different seed per trial
    ./$BINOCULAR_EXEC $RANDOM_SEED > "$RESULTS_DIR/trial_${trial}.log" 2>&1
    mv binocular_simulation_log.csv "$RESULTS_DIR/trial_${trial}.csv"
    
    echo "Trial $trial complete"
done

echo ""
echo "==================================="
echo "ANALYZING RESULTS"
echo "==================================="

# Analyze all trials
echo "$NUM_TRIALS" > "$RESULTS_DIR/num_trials.txt"
python3 << 'PYTHON_SCRIPT'
import os
import csv
import math

results_dir = "results/binocular"
# Read the number of trials from file
with open(f"{results_dir}/num_trials.txt", "r") as f:
    num_trials = int(f.read().strip())

print("\n" + "="*60)
print("INDIVIDUAL TRIAL RESULTS (BINOCULAR VISION)")
print("="*60)

trial_catches = []
trial_towards_pct = []
trial_away_pct = []

for trial in range(1, num_trials + 1):
    filepath = f"{results_dir}/trial_{trial}.csv"
    
    if not os.path.exists(filepath):
        break
        
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
            
            if num_trials == 1 or trial % 5 == 0 or trial <= 3:
                print(f"Trial {trial:2d}: {catches:3d} catches in {num_bars} Bars (rate: {catch_rate:.4f} per Bar)")
        else:
            print(f"Trial {trial}: ERROR - No data")
            trial_catches.append(0)

num_trials = len(trial_catches)

# Read log files for directionality
epoch_data = []
for trial in range(1, num_trials + 1):
    logfile = f"{results_dir}/trial_{trial}.log"
    if os.path.exists(logfile):
        with open(logfile, 'r') as f:
            content = f.read()
            # Extract movement stats
            if "Movement directionality:" in content:
                for line in content.split('\n'):
                    if "Towards closest:" in line:
                        pct = float(line.split('(')[1].split('%')[0].strip())
                        trial_towards_pct.append(pct)
                        break
                for line in content.split('\n'):
                    if "Away from closest:" in line:
                        pct = float(line.split('(')[1].split('%')[0].strip())
                        trial_away_pct.append(pct)
                        break
            # Extract epoch data
            if "Epoch breakdown:" in content:
                lines = content.split('\n')
                in_epoch_section = False
                for line in lines:
                    if "Epoch breakdown:" in line:
                        in_epoch_section = True
                        continue
                    if in_epoch_section and "Epoch" in line and ":" in line:
                        try:
                            parts = line.split(':')
                            epoch_num = int(parts[0].split()[-1])
                            catches = int(parts[1].split()[0])
                            epoch_data.append((trial, epoch_num, catches))
                        except (ValueError, IndexError):
                            pass
                    elif in_epoch_section and line.strip() == "":
                        break

print("\n" + "="*60)
print("AGGREGATE RESULTS (BINOCULAR)")
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
        elif avg_towards > 50:
            print(f"  ✓ MODERATE DIRECTIONAL LEARNING")
        elif avg_towards > 45:
            print(f"  ~ WEAK DIRECTIONAL LEARNING")
        else:
            print(f"  ✗ NO DIRECTIONAL LEARNING")
    
    if avg_catches >= 300:
        print("\n✓✓✓ EXCELLENT BINOCULAR PERFORMANCE!")
    elif avg_catches >= 200:
        print("\n✓✓ STRONG BINOCULAR PERFORMANCE")
    elif avg_catches >= 100:
        print("\n✓ MODERATE BINOCULAR PERFORMANCE")
    elif avg_catches >= 50:
        print("\n~ WEAK BINOCULAR PERFORMANCE")
    else:
        print("\n✗ LOW BINOCULAR PERFORMANCE")
    
    # Analyze temporal learning trends
    if len(epoch_data) > 0:
        print("\n" + "="*60)
        print("TEMPORAL LEARNING ANALYSIS")
        print("="*60)
        
        from collections import defaultdict
        epoch_avgs = defaultdict(list)
        for trial, epoch_num, catches in epoch_data:
            epoch_avgs[epoch_num].append(catches)
        
        max_epoch = max(epoch_avgs.keys())
        print(f"\nAverage catches per epoch (across {num_trials} trials):")
        for epoch in sorted(epoch_avgs.keys()):
            avg = sum(epoch_avgs[epoch]) / len(epoch_avgs[epoch])
            print(f"  Epoch {epoch:2d}: {avg:5.1f} catches")
        
        # Calculate trend
        early_epochs = [e for e in range(1, max_epoch//3 + 1) if e in epoch_avgs]
        late_epochs = [e for e in range(2*max_epoch//3 + 1, max_epoch + 1) if e in epoch_avgs]
        
        if early_epochs and late_epochs:
            early_avg = sum(sum(epoch_avgs[e]) for e in early_epochs) / sum(len(epoch_avgs[e]) for e in early_epochs)
            late_avg = sum(sum(epoch_avgs[e]) for e in late_epochs) / sum(len(epoch_avgs[e]) for e in late_epochs)
            improvement = ((late_avg - early_avg) / early_avg) * 100 if early_avg > 0 else 0
            
            print(f"\nTemporal trend:")
            print(f"  Early epochs: {early_avg:.1f} avg catches")
            print(f"  Late epochs: {late_avg:.1f} avg catches")
            print(f"  Improvement: {improvement:+.1f}%")
            
            if improvement > 20:
                print("  ✓✓ STRONG LEARNING")
            elif improvement > 5:
                print("  ✓ MODERATE LEARNING")
            elif improvement > -5:
                print("  ~ STABLE")
            else:
                print("  ✗ DECLINING")
else:
    print("ERROR: No trial data")

PYTHON_SCRIPT

if [ "$SHOW_GUI" = true ]; then
    echo ""
    echo "==================================="
    echo "RUNNING GUI VISUALIZATION"
    echo "==================================="
    if [ "$NUM_TRIALS" -eq 1 ]; then
        echo "Replaying the trial with binocular GUI visualization..."
    else
        echo "Replaying Trial 1 with binocular GUI visualization..."
    fi
    echo "Press ESC or Q to quit early, SPACE to pause"
    echo ""

    # Replay trial 1 CSV through GUI
    python3 visualization/binocular_gui.py < "$RESULTS_DIR/trial_1.csv"
fi

echo ""
echo "==================================="
echo "EXPERIMENT COMPLETE"
echo "==================================="
if [ "$NUM_TRIALS" -eq 1 ]; then
    echo "Single trial results saved in: $RESULTS_DIR/"
else
    echo "$NUM_TRIALS trial results saved in: $RESULTS_DIR/"
fi
if [ "$SHOW_GUI" = false ]; then
    echo "(Use without --no-gui flag to see visualizations)"
fi
