#!/bin/bash
# Single trial with immediate GUI visualization

echo "==================================="
echo "SINGLE TRIAL GUI TEST"
echo "==================================="
echo "Running 1 trial with immediate GUI visualization..."
echo ""

# Ensure results directory exists
mkdir -p learning_results

# Run single learning simulation
echo "Running learning simulation..."
./cat_mouse_learning 1234 > learning_results/trial_gui.log 2>&1

# Move the CSV file
if [ -f "simulation_log.csv" ]; then
    mv "simulation_log.csv" "learning_results/trial_gui.csv"
    echo "Trial complete. Launching GUI visualization..."
    echo ""
    echo "==================================="
    echo "GUI VISUALIZATION"
    echo "==================================="
    echo "Replaying cat behavior with GUI..."
    echo "Press ESC or Q to quit early"
    echo ""
    
    # Launch GUI visualization
    python3 cat_mouse_gui.py < learning_results/trial_gui.csv
else
    echo "ERROR: simulation_log.csv not found"
    exit 1
fi

echo ""
echo "==================================="
echo "TRIAL SUMMARY"
echo "==================================="

# Quick analysis
python3 << 'PYTHON_SCRIPT'
import csv

with open('learning_results/trial_gui.csv', 'r') as f:
    reader = csv.reader(f)
    next(reader)  # Skip header
    
    rows = list(reader)
    num_bars = len(rows)
    
    if num_bars > 0:
        last_row = rows[-1]
        catches = int(last_row[-1])
        
        print(f"Bars simulated: {num_bars}")
        print(f"Total catches: {catches}")
        print(f"Catch rate: {catches/num_bars:.4f} per Bar")
        
        # Count movement directions
        movements = {}
        for row in rows:
            if len(row) >= 9 and row[8] != '0':  # output_action column
                direction = row[8]
                movements[direction] = movements.get(direction, 0) + 1
        
        total_moves = sum(movements.values())
        if total_moves > 0:
            print(f"Total movements: {total_moves}")
            print("Movement distribution:")
            for direction in sorted(movements.keys()):
                count = movements[direction]
                pct = (count / total_moves) * 100
                print(f"  Direction {direction}: {count} ({pct:.1f}%)")
    else:
        print("ERROR: No simulation data")

PYTHON_SCRIPT