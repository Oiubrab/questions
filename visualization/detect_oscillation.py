#!/usr/bin/env python3
"""
Detect oscillation patterns in cat movement from simulation logs.
Identifies when cat gets stuck in repetitive back-and-forth movements.
"""

import csv
import sys
from collections import Counter

def analyze_oscillation(csv_file):
    """Analyze cat movement for oscillation patterns."""
    
    positions = []
    movements = []
    
    # Read simulation log
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            cat_x = int(row['cat_x'])
            cat_y = int(row['cat_y'])
            positions.append((cat_x, cat_y))
            
            # Track movement direction (if output_direction exists)
            if 'output_direction' in row:
                direction = int(row['output_direction'])
                if direction > 0:
                    movements.append(direction)
    
    total_bars = len(positions)
    print(f"\n{'='*60}")
    print(f"OSCILLATION ANALYSIS: {csv_file}")
    print(f"{'='*60}")
    print(f"Total Bars: {total_bars}")
    
    # Calculate net displacement vs total movement
    if len(positions) > 1:
        start_pos = positions[0]
        end_pos = positions[-1]
        net_displacement = ((end_pos[0] - start_pos[0])**2 + 
                          (end_pos[1] - start_pos[1])**2)**0.5
        
        # Calculate total path length
        total_distance = 0
        for i in range(1, len(positions)):
            dx = positions[i][0] - positions[i-1][0]
            dy = positions[i][1] - positions[i-1][1]
            total_distance += (dx*dx + dy*dy)**0.5
        
        efficiency = net_displacement / total_distance if total_distance > 0 else 0
        
        print(f"\nMovement Efficiency:")
        print(f"  Net displacement: {net_displacement:.2f} units")
        print(f"  Total path length: {total_distance:.2f} units")
        print(f"  Efficiency: {efficiency:.3f} (0=stuck, 1=straight line)")
        
        if efficiency < 0.3:
            print(f"  ⚠️  WARNING: Very inefficient movement - likely oscillating!")
    
    # Check for position cycling
    if len(positions) >= 100:
        recent_positions = positions[-100:]
        unique_positions = len(set(recent_positions))
        position_counts = Counter(recent_positions)
        most_common = position_counts.most_common(5)
        
        print(f"\nPosition Cycling (last 100 Bars):")
        print(f"  Unique positions visited: {unique_positions}/100")
        
        if unique_positions < 20:
            print(f"  ⚠️  WARNING: Cat visiting very few positions - likely stuck!")
        
        print(f"  Most visited positions:")
        for pos, count in most_common:
            print(f"    {pos}: {count} times ({count}%)")
    
    # Detect back-and-forth oscillation in movements
    if len(movements) >= 20:
        # Check for direction reversals (opposite directions)
        opposites = {1: 8, 2: 7, 3: 6, 4: 5, 5: 4, 6: 3, 7: 2, 8: 1}
        
        reversals = 0
        for i in range(1, len(movements)):
            if movements[i] == opposites.get(movements[i-1], -1):
                reversals += 1
        
        reversal_rate = reversals / len(movements)
        
        print(f"\nDirection Reversals:")
        print(f"  Total reversals: {reversals}/{len(movements)}")
        print(f"  Reversal rate: {reversal_rate:.3f}")
        
        if reversal_rate > 0.3:
            print(f"  ⚠️  WARNING: High reversal rate - oscillating back and forth!")
        
        # Check for repeated short sequences
        print(f"\nMovement Pattern Analysis:")
        direction_counts = Counter(movements[-100:] if len(movements) > 100 else movements)
        print(f"  Direction usage (most recent):")
        for direction, count in direction_counts.most_common(8):
            dir_names = {1: "Up-Left", 2: "Up", 3: "Up-Right", 4: "Left", 
                        5: "Right", 6: "Down-Left", 7: "Down", 8: "Down-Right"}
            pct = (count / len(movements[-100:])) * 100 if len(movements) > 100 else (count / len(movements)) * 100
            print(f"    {direction} ({dir_names.get(direction, '?')}): {count} times ({pct:.1f}%)")
            
            if pct > 40:
                print(f"      ⚠️  One direction dominates - may be stuck in loop!")
    
    # Detect if cat is stuck in a small area
    if len(positions) >= 50:
        recent_pos = positions[-50:]
        xs = [p[0] for p in recent_pos]
        ys = [p[1] for p in recent_pos]
        
        x_range = max(xs) - min(xs)
        y_range = max(ys) - min(ys)
        
        print(f"\nSpatial Confinement (last 50 Bars):")
        print(f"  X range: {x_range} units")
        print(f"  Y range: {y_range} units")
        print(f"  Area covered: {x_range * y_range} square units")
        
        if x_range < 5 and y_range < 5:
            print(f"  ⚠️  WARNING: Cat confined to small area - likely stuck!")
    
    print(f"{'='*60}\n")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python detect_oscillation.py <simulation_log.csv>")
        sys.exit(1)
    
    analyze_oscillation(sys.argv[1])
