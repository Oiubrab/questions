#!/usr/bin/env python3
"""
Analyze the learned brain state from the cat-mouse learning system.
Focus on pathway strengths, input routing, and directional preferences.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict

def analyze_brain_pathways():
    print("=" * 60)
    print("BRAIN PATHWAY ANALYSIS")
    print("=" * 60)
    
    # Load the data
    synapses = pd.read_csv('learning_results/final_synapse_state.csv')
    inputter = pd.read_csv('learning_results/inputter_state.csv')
    outputter = pd.read_csv('learning_results/outputter_state.csv')
    
    print(f"Total synapses: {len(synapses)}")
    print(f"Synapse strength range: {synapses['strength'].min()} to {synapses['strength'].max()}")
    print(f"Mean synapse strength: {synapses['strength'].mean():.1f}")
    print(f"Median synapse strength: {synapses['strength'].median():.1f}")
    print()
    
    # Analyze input layer connections (row 1 - where vision inputs feed)
    print("=" * 60)
    print("INPUT LAYER ANALYSIS (Row 1 - Vision Input)")
    print("=" * 60)
    
    # Map input columns to vision slices
    # Input columns 3-10 correspond to vision slices 1-8 (45° each)
    vision_slices = {
        3: "Slice 1 (0°-45°)",
        4: "Slice 2 (45°-90°)", 
        5: "Slice 3 (90°-135°)",
        6: "Slice 4 (135°-180°)",
        7: "Slice 5 (180°-225°)",
        8: "Slice 6 (225°-270°)",
        9: "Slice 7 (270°-315°)",
        10: "Slice 8 (315°-360°)"
    }
    
    # Analyze pathways FROM each input column
    input_pathway_strengths = {}
    for col in range(3, 11):  # Columns 3-10
        # Find all synapses originating from row 1, this column
        from_input = synapses[(synapses['from_row'] == 1) & (synapses['from_col'] == col)]
        if len(from_input) > 0:
            total_strength = from_input['strength'].sum()
            max_strength = from_input['strength'].max()
            mean_strength = from_input['strength'].mean()
            strong_connections = len(from_input[from_input['strength'] > 1000])
            
            input_pathway_strengths[col] = {
                'total': total_strength,
                'max': max_strength,
                'mean': mean_strength,
                'strong_count': strong_connections,
                'slice_name': vision_slices[col]
            }
            
            print(f"{vision_slices[col]}:")
            print(f"  Total pathway strength: {total_strength:,}")
            print(f"  Strongest connection: {max_strength:,}")
            print(f"  Mean connection: {mean_strength:.1f}")
            print(f"  Strong connections (>1000): {strong_connections}")
            print()
    
    # Find which input slices have the strongest learned pathways
    sorted_inputs = sorted(input_pathway_strengths.items(), 
                          key=lambda x: x[1]['total'], reverse=True)
    
    print("=" * 60)
    print("INPUT PATHWAY RANKING (by total strength)")
    print("=" * 60)
    for i, (col, data) in enumerate(sorted_inputs):
        print(f"{i+1:2d}. {data['slice_name']:<20} Total: {data['total']:,}")
    print()
    
    # Analyze output layer connections (row 6 - where movement decisions come from)
    print("=" * 60)
    print("OUTPUT LAYER ANALYSIS (Row 6 - Movement Output)")
    print("=" * 60)
    
    # Movement directions mapping
    movement_dirs = {
        3: "Dir 1 (Up-Left)",
        4: "Dir 2 (Up)", 
        5: "Dir 3 (Up-Right)",
        6: "Dir 4 (Left)",
        7: "Dir 5 (Right)",
        8: "Dir 6 (Down-Left)",
        9: "Dir 7 (Down)",
        10: "Dir 8 (Down-Right)"
    }
    
    # Analyze pathways TO each output column
    output_pathway_strengths = {}
    for col in range(3, 11):  # Columns 3-10
        # Find all synapses terminating at row 6, this column
        to_output = synapses[(synapses['to_row'] == 6) & (synapses['to_col'] == col)]
        if len(to_output) > 0:
            total_strength = to_output['strength'].sum()
            max_strength = to_output['strength'].max()
            mean_strength = to_output['strength'].mean()
            strong_connections = len(to_output[to_output['strength'] > 1000])
            
            output_pathway_strengths[col] = {
                'total': total_strength,
                'max': max_strength,
                'mean': mean_strength,
                'strong_count': strong_connections,
                'dir_name': movement_dirs[col]
            }
            
            print(f"{movement_dirs[col]}:")
            print(f"  Total input strength: {total_strength:,}")
            print(f"  Strongest input: {max_strength:,}")
            print(f"  Mean input: {mean_strength:.1f}")
            print(f"  Strong inputs (>1000): {strong_connections}")
            print()
    
    # Find which output directions have the strongest learned pathways
    sorted_outputs = sorted(output_pathway_strengths.items(), 
                           key=lambda x: x[1]['total'], reverse=True)
    
    print("=" * 60)
    print("OUTPUT PATHWAY RANKING (by total input strength)")
    print("=" * 60)
    for i, (col, data) in enumerate(sorted_outputs):
        print(f"{i+1:2d}. {data['dir_name']:<20} Total: {data['total']:,}")
    print()
    
    # Analyze strongest individual synapses
    print("=" * 60)
    print("STRONGEST INDIVIDUAL SYNAPSES")
    print("=" * 60)
    strongest_synapses = synapses.nlargest(20, 'strength')
    
    for i, (idx, synapse) in enumerate(strongest_synapses.iterrows()):
        from_layer = "INPUT" if synapse['from_row'] == 1 else f"Layer {synapse['from_row']}"
        to_layer = "OUTPUT" if synapse['to_row'] == 6 else f"Layer {synapse['to_row']}"
        
        print(f"{i+1:2d}. {from_layer} ({synapse['from_row']},{synapse['from_col']}) → "
              f"{to_layer} ({synapse['to_row']},{synapse['to_col']}) "
              f"Strength: {synapse['strength']:,}")
    print()
    
    # Analyze vision-to-movement mappings (direct or indirect)
    print("=" * 60) 
    print("VISION-TO-MOVEMENT PATHWAY ANALYSIS")
    print("=" * 60)
    
    # For each vision slice, trace its strongest pathways to movement outputs
    vision_to_movement = {}
    
    for vision_col in range(3, 11):
        vision_slice = vision_col - 2  # Convert to 1-8
        vision_to_movement[vision_slice] = {}
        
        # Find all paths from this vision input that eventually reach outputs
        # Start with connections from input layer
        from_input = synapses[(synapses['from_row'] == 1) & (synapses['from_col'] == vision_col)]
        
        # Track total influence on each output direction
        for output_col in range(3, 11):
            movement_dir = output_col - 2  # Convert to 1-8
            
            # Find paths that go from this vision input to this movement output
            # This is a simplified analysis - just look for strong direct paths through the layers
            total_influence = 0
            
            # Check multi-hop paths (this is simplified - real analysis would need graph traversal)
            for _, conn1 in from_input.iterrows():
                if conn1['strength'] > 100:  # Only consider reasonably strong first hops
                    # Find connections from this intermediate point toward the output
                    intermediate_row = conn1['to_row']
                    intermediate_col = conn1['to_col']
                    
                    # Look for paths from intermediate to output
                    from_intermediate = synapses[
                        (synapses['from_row'] == intermediate_row) & 
                        (synapses['from_col'] == intermediate_col)
                    ]
                    
                    for _, conn2 in from_intermediate.iterrows():
                        if conn2['to_row'] == 6 and conn2['to_col'] == output_col:
                            # Direct path found: input -> intermediate -> output
                            path_strength = min(conn1['strength'], conn2['strength'])
                            total_influence += path_strength
            
            vision_to_movement[vision_slice][movement_dir] = total_influence
    
    # Display vision-to-movement mappings
    print("Vision Slice → Movement Direction Influence:")
    print("(Showing approximate pathway strengths)")
    print()
    
    for vision_slice in range(1, 9):
        vision_angle = f"{(vision_slice-1)*45}°-{vision_slice*45}°"
        print(f"Vision Slice {vision_slice} ({vision_angle}):")
        
        # Sort movement directions by influence
        movements = [(dir, strength) for dir, strength in vision_to_movement[vision_slice].items()]
        movements.sort(key=lambda x: x[1], reverse=True)
        
        for movement_dir, strength in movements[:3]:  # Show top 3
            if strength > 0:
                movement_angle = f"{(movement_dir-1)*45}°-{movement_dir*45}°" 
                print(f"  → Movement {movement_dir} ({movement_angle}): {strength:,}")
        print()
    
    print("=" * 60)
    print("ANALYSIS COMPLETE")
    print("=" * 60)

if __name__ == "__main__":
    analyze_brain_pathways()