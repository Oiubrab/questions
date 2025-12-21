#!/usr/bin/env python3
"""
Visualize the final brain state showing neurons and synapse connections.
"""

import csv
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyArrowPatch
import numpy as np

def load_brain_state(filename='brain_state.csv'):
    """Load neuron states from CSV."""
    brain = {}
    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            r, c = int(row['row']), int(row['col'])
            state = int(row['state'])
            brain[(r, c)] = state
    return brain

def load_synapse_state(filename='synapse_state.csv'):
    """Load synapse strengths from CSV."""
    synapses = []
    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            synapses.append({
                'from': (int(row['from_row']), int(row['from_col'])),
                'to': (int(row['to_row']), int(row['to_col'])),
                'strength': int(row['strength'])
            })
    return synapses

def load_inputter_state(filename='inputter_state.csv'):
    """Load inputter array from CSV."""
    inputter = {}
    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            col = int(row['col'])
            state = int(row['state'])
            inputter[col] = state
    return inputter

def load_outputter_state(filename='outputter_state.csv'):
    """Load outputter array from CSV."""
    outputter = {}
    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            col = int(row['col'])
            state = int(row['state'])
            outputter[col] = state
    return outputter

def visualize_brain(brain, synapses, inputter, outputter, output_file='brain_visualization.png'):
    """Create visualization of brain state and synapse connections."""
    
    # Determine grid dimensions
    rows = max(pos[0] for pos in brain.keys())
    cols = max(pos[1] for pos in brain.keys())
    
    # Calculate synapse strength range for normalization
    strengths = [s['strength'] for s in synapses]
    min_strength = min(strengths)
    max_strength = max(strengths)
    
    # Create figure with extra space for input/output rows
    fig, ax = plt.subplots(figsize=(18, 12))
    ax.set_xlim(-0.5, cols + 0.5)
    ax.set_ylim(-1.5, rows + 1.5)
    ax.set_aspect('equal')
    ax.invert_yaxis()  # Row 1 at top
    
    # State colors and labels
    state_colors = {0: '#cccccc', 1: '#4CAF50', 2: '#FF5722'}
    state_labels = {0: 'Low', 1: 'Medium', 2: 'High'}
    
    # Draw synapses first (so they appear behind neurons)
    print(f"Drawing {len(synapses)} synapses...")
    
    # Neuron radius for shrinking arrows
    neuron_radius = 0.3
    
    for syn in synapses:
        from_pos = syn['from']
        to_pos = syn['to']
        strength = syn['strength']
        
        # Normalize strength for visualization (0-1)
        norm_strength = (strength - min_strength) / (max_strength - min_strength) if max_strength > min_strength else 0.5
        
        # Calculate positions (neurons at integer coordinates)
        x1, y1 = from_pos[1] - 1, from_pos[0] - 1  # Convert to 0-indexed
        x2, y2 = to_pos[1] - 1, to_pos[0] - 1
        
        # Calculate offset perpendicular to arrow direction to avoid overlaps
        # This makes bidirectional arrows visible side-by-side
        dx, dy = x2 - x1, y2 - y1
        length = (dx**2 + dy**2)**0.5
        
        # For connections within the brain (not to output row)
        if to_pos[0] <= rows:
            if length > 0.01:  # Normal arrows between different neurons
                # Perpendicular offset (scaled by 0.15 for visibility)
                offset_x = -dy / length * 0.15
                offset_y = dx / length * 0.15
                
                # Apply offset to both start and end points
                x1_offset = x1 + offset_x
                y1_offset = y1 + offset_y
                x2_offset = x2 + offset_x
                y2_offset = y2 + offset_y
                
                # Shrink arrows to start/end at neuron edge
                shrinkA = neuron_radius * 50  # Scale for mutation_scale units
                shrinkB = neuron_radius * 50
            else:
                # Self-loop - no offset needed, won't be visible anyway
                continue
        else:
            # Connection to output row - no offset
            x1_offset, y1_offset = x1, y1
            x2_offset, y2_offset = x2, y2
            shrinkA = neuron_radius * 50
            shrinkB = 0
        
        # Line width based on strength (increased minimum for visibility)
        linewidth = 1.0 + 2.5 * norm_strength
        
        # Alpha (transparency) based on strength (increased minimum for visibility)
        alpha = 0.3 + 0.6 * norm_strength
        
        # Color gradient from yellow (weak) to red (strong)
        color = plt.cm.YlOrRd(norm_strength)
        
        # Draw arrow with visible arrowhead
        arrow = FancyArrowPatch((x1_offset, y1_offset), (x2_offset, y2_offset),
                               arrowstyle='->', 
                               linewidth=linewidth,
                               color=color,
                               alpha=alpha,
                               mutation_scale=20,
                               shrinkA=shrinkA,
                               shrinkB=shrinkB,
                               zorder=1)
        ax.add_patch(arrow)
    
    # Draw inputter array (above brain)
    print(f"Drawing {len(inputter)} input cells...")
    for col, state in inputter.items():
        x, y = col - 1, -1  # Row -1 for inputter
        color = state_colors.get(state, '#cccccc')
        circle = plt.Circle((x, y), 0.3, color=color, ec='black', linewidth=2, zorder=2)
        ax.add_patch(circle)
        ax.text(x, y, f'I{col}', ha='center', va='center', fontsize=8, zorder=3)
    
    # Draw empty placeholders for unused input positions
    for c in range(1, cols + 1):
        if c not in inputter:
            x, y = c - 1, -1
            circle = plt.Circle((x, y), 0.15, color='white', ec='lightgray', 
                              linewidth=1, linestyle='dashed', zorder=2, fill=False)
            ax.add_patch(circle)
    
    # Draw outputter array (below brain)
    print(f"Drawing {len(outputter)} output cells...")
    for col, state in outputter.items():
        x, y = col - 1, rows  # Row after last brain row
        color = state_colors.get(state, '#cccccc')
        circle = plt.Circle((x, y), 0.3, color=color, ec='black', linewidth=2, zorder=2)
        ax.add_patch(circle)
        ax.text(x, y, f'O{col}', ha='center', va='center', fontsize=8, zorder=3)
    
    # Draw empty placeholders for unused output positions
    for c in range(1, cols + 1):
        if c not in outputter:
            x, y = c - 1, rows
            circle = plt.Circle((x, y), 0.15, color='white', ec='lightgray', 
                              linewidth=1, linestyle='dashed', zorder=2, fill=False)
            ax.add_patch(circle)
    
    # Draw neurons
    print(f"Drawing {len(brain)} neurons...")
    for (row, col), state in brain.items():
        x, y = col - 1, row - 1  # Convert to 0-indexed
        
        # Neuron circle
        circle = plt.Circle((x, y), 0.3, 
                           facecolor=state_colors[state],
                           edgecolor='black',
                           linewidth=2,
                           zorder=2)
        ax.add_patch(circle)
        
        # Add position label
        ax.text(x, y, f'{row},{col}', 
               ha='center', va='center',
               fontsize=6, fontweight='bold',
               zorder=3)
    
    # Add title and labels
    ax.set_title('Brain State Visualization (Final Step)', 
                fontsize=16, fontweight='bold', pad=20)
    ax.set_xlabel('Column', fontsize=12)
    ax.set_ylabel('Row', fontsize=12)
    ax.grid(True, alpha=0.3, linestyle='--')
    
    # Create legend
    legend_elements = [
        patches.Patch(facecolor=state_colors[0], edgecolor='black', label='Low State'),
        patches.Patch(facecolor=state_colors[1], edgecolor='black', label='Medium State'),
        patches.Patch(facecolor=state_colors[2], edgecolor='black', label='High State'),
        plt.Line2D([0], [0], color='yellow', linewidth=1, alpha=0.3, label='Weak Synapse'),
        plt.Line2D([0], [0], color='orange', linewidth=2, alpha=0.6, label='Medium Synapse'),
        plt.Line2D([0], [0], color='red', linewidth=3, alpha=0.9, label='Strong Synapse')
    ]
    ax.legend(handles=legend_elements, loc='upper right', fontsize=10)
    
    # Add statistics text
    stats_text = f'Grid: {rows}×{cols} | Synapses: {len(synapses)} | '
    stats_text += f'Strength Range: {min_strength:,} - {max_strength:,}'
    ax.text(0.5, -0.1, stats_text,
           transform=ax.transAxes,
           ha='center', fontsize=10,
           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches='tight')
    print(f"Visualization saved to: {output_file}")
    plt.show()

if __name__ == '__main__':
    import sys
    import os
    
    brain_file = sys.argv[1] if len(sys.argv) > 1 else 'brain_state.csv'
    synapse_file = sys.argv[2] if len(sys.argv) > 2 else 'synapse_state.csv'
    output_file = sys.argv[3] if len(sys.argv) > 3 else 'brain_visualization.png'
    
    # Derive inputter/outputter filenames from brain_file path
    base_dir = os.path.dirname(brain_file) if brain_file != 'brain_state.csv' else '.'
    inputter_file = os.path.join(base_dir, 'inputter_state.csv')
    outputter_file = os.path.join(base_dir, 'outputter_state.csv')
    
    print("Loading brain state...")
    brain = load_brain_state(brain_file)
    print(f"Loaded {len(brain)} neurons")
    
    print("Loading synapse state...")
    synapses = load_synapse_state(synapse_file)
    print(f"Loaded {len(synapses)} synapses")
    
    print("Loading inputter state...")
    inputter = load_inputter_state(inputter_file)
    print(f"Loaded {len(inputter)} input cells")
    
    print("Loading outputter state...")
    outputter = load_outputter_state(outputter_file)
    print(f"Loaded {len(outputter)} output cells")
    
    print("Creating visualization...")
    visualize_brain(brain, synapses, inputter, outputter, output_file)
