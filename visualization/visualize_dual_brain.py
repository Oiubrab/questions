#!/usr/bin/env python3
"""
Visualize both primary brain and meta-brain states side-by-side.
Reads state files from a trial directory.
"""

import csv
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyArrowPatch
import numpy as np
import sys
import os

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
                'strength': int(row['strength']),
                'dominant_incoming_dir': int(row['dominant_incoming_dir'])
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

def visualize_single_brain(ax, brain, synapses, inputter, outputter, title, max_cols):
    """Visualize a single brain on given axes."""
    
    # Determine grid dimensions
    if brain:
        rows = max(pos[0] for pos in brain.keys())
        cols = max(pos[1] for pos in brain.keys())
    else:
        rows, cols = 1, 1
    
    # Calculate synapse strength range for normalization
    if synapses:
        strengths = [s['strength'] for s in synapses]
        min_strength = min(strengths)
        max_strength = max(strengths)
    else:
        min_strength, max_strength = 0, 1
    
    # Set axis limits
    ax.set_xlim(-0.5, max(cols, max_cols) + 0.5)
    ax.set_ylim(-1.5, rows + 1.5)
    ax.set_aspect('equal')
    ax.invert_yaxis()
    
    # State colors
    state_colors = {0: '#cccccc', 1: '#4CAF50', 2: '#FF5722'}
    
    # Direction colors
    direction_colors = plt.cm.hsv(np.linspace(0, 1, 9))[:8]
    
    # Neuron radius
    neuron_radius = 0.3
    
    # Draw synapses first
    for syn in synapses:
        from_pos = syn['from']
        to_pos = syn['to']
        strength = syn['strength']
        
        # Normalize strength
        norm_strength = (strength - min_strength) / (max_strength - min_strength) if max_strength > min_strength else 0.5
        
        # Calculate positions
        x1, y1 = from_pos[1] - 1, from_pos[0] - 1
        x2, y2 = to_pos[1] - 1, to_pos[0] - 1
        
        # Calculate offset perpendicular to arrow direction
        dx, dy = x2 - x1, y2 - y1
        length = (dx**2 + dy**2)**0.5
        
        if to_pos[0] <= rows:
            if length > 0.01:
                offset_x = -dy / length * 0.15
                offset_y = dx / length * 0.15
                x1_offset = x1 + offset_x
                y1_offset = y1 + offset_y
                x2_offset = x2 + offset_x
                y2_offset = y2 + offset_y
                shrinkA = neuron_radius * 50
                shrinkB = neuron_radius * 50
            else:
                continue
        else:
            x1_offset, y1_offset = x1, y1
            x2_offset, y2_offset = x2, y2
            shrinkA = neuron_radius * 50
            shrinkB = 0
        
        linewidth = 1.0 + 2.5 * norm_strength
        alpha = 0.3 + 0.6 * norm_strength
        
        # Color based on dominant incoming direction
        incoming_dir = syn.get('dominant_incoming_dir', 0)
        if incoming_dir > 0 and incoming_dir <= 8:
            color = direction_colors[incoming_dir - 1]
        else:
            color = plt.cm.YlOrRd(norm_strength)
        
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
    
    # Draw inputter array
    for col, state in inputter.items():
        x, y = col - 1, -1
        color = state_colors.get(state, '#cccccc')
        circle = plt.Circle((x, y), 0.3, color=color, ec='black', linewidth=2, zorder=2)
        ax.add_patch(circle)
        ax.text(x, y, f'I{col}', ha='center', va='center', fontsize=6, zorder=3)
    
    # Draw empty input placeholders
    for c in range(1, cols + 1):
        if c not in inputter:
            x, y = c - 1, -1
            circle = plt.Circle((x, y), 0.15, color='white', ec='lightgray', 
                              linewidth=1, linestyle='dashed', zorder=2, fill=False)
            ax.add_patch(circle)
    
    # Draw outputter array
    for col, state in outputter.items():
        x, y = col - 1, rows
        color = state_colors.get(state, '#cccccc')
        circle = plt.Circle((x, y), 0.3, color=color, ec='black', linewidth=2, zorder=2)
        ax.add_patch(circle)
        ax.text(x, y, f'O{col}', ha='center', va='center', fontsize=6, zorder=3)
    
    # Draw empty output placeholders
    for c in range(1, cols + 1):
        if c not in outputter:
            x, y = c - 1, rows
            circle = plt.Circle((x, y), 0.15, color='white', ec='lightgray', 
                              linewidth=1, linestyle='dashed', zorder=2, fill=False)
            ax.add_patch(circle)
    
    # Draw neurons
    for (row, col), state in brain.items():
        x, y = col - 1, row - 1
        circle = plt.Circle((x, y), 0.3, 
                           facecolor=state_colors[state],
                           edgecolor='black',
                           linewidth=2,
                           zorder=2)
        ax.add_patch(circle)
        ax.text(x, y, f'{row},{col}', 
               ha='center', va='center',
               fontsize=5, fontweight='bold',
               zorder=3)
    
    # Add title
    ax.set_title(title, fontsize=12, fontweight='bold', pad=10)
    ax.set_xlabel('Column', fontsize=10)
    ax.set_ylabel('Row', fontsize=10)
    ax.grid(True, alpha=0.3, linestyle='--')
    
    # Stats
    stats = f'{rows}×{cols} | {len(synapses)} syn | {min_strength:,}-{max_strength:,}'
    ax.text(0.5, -0.15, stats,
           transform=ax.transAxes,
           ha='center', fontsize=8,
           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

def visualize_dual_brain(trial_dir, output_file='dual_brain_visualization.png'):
    """Create side-by-side visualization of primary and meta brains."""
    
    print(f"Loading brain states from: {trial_dir}")
    
    # Load primary brain
    primary_brain = load_brain_state(os.path.join(trial_dir, 'brain_state.csv'))
    primary_synapses = load_synapse_state(os.path.join(trial_dir, 'synapse_state.csv'))
    primary_inputter = load_inputter_state(os.path.join(trial_dir, 'inputter_state.csv'))
    primary_outputter = load_outputter_state(os.path.join(trial_dir, 'outputter_state.csv'))
    
    print(f"Primary brain: {len(primary_brain)} neurons, {len(primary_synapses)} synapses")
    
    # Load meta brain (if files exist)
    meta_brain = {}
    meta_synapses = []
    meta_inputter = {}
    meta_outputter = {}
    
    meta_brain_file = os.path.join(trial_dir, 'meta_brain_state.csv')
    if os.path.exists(meta_brain_file):
        meta_brain = load_brain_state(meta_brain_file)
        meta_synapses = load_synapse_state(os.path.join(trial_dir, 'meta_synapse_state.csv'))
        meta_inputter = load_inputter_state(os.path.join(trial_dir, 'meta_inputter_state.csv'))
        meta_outputter = load_outputter_state(os.path.join(trial_dir, 'meta_outputter_state.csv'))
        print(f"Meta brain: {len(meta_brain)} neurons, {len(meta_synapses)} synapses")
    else:
        print("Note: Meta-brain state files not found, showing primary brain only")
    
    # Determine layout
    if meta_brain:
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(24, 12))
        max_cols = max(
            max((pos[1] for pos in primary_brain.keys()), default=1),
            max((pos[1] for pos in meta_brain.keys()), default=1)
        )
    else:
        fig, ax1 = plt.subplots(1, 1, figsize=(16, 12))
        max_cols = max((pos[1] for pos in primary_brain.keys()), default=1)
    
    # Visualize primary brain
    visualize_single_brain(ax1, primary_brain, primary_synapses, primary_inputter, 
                          primary_outputter, 'Primary Brain (Sensorimotor)', max_cols)
    
    # Visualize meta brain if available
    if meta_brain:
        visualize_single_brain(ax2, meta_brain, meta_synapses, meta_inputter, 
                              meta_outputter, 'Meta-Brain (Strategy Control)', max_cols)
    
    # Create shared legend
    state_colors = {0: '#cccccc', 1: '#4CAF50', 2: '#FF5722'}
    direction_colors = plt.cm.hsv(np.linspace(0, 1, 9))[:8]
    direction_labels = ['\u2196 Up-Left', '\u2191 Up', '\u2197 Up-Right', '\u2190 Left', 
                       '\u2192 Right', '\u2199 Down-Left', '\u2193 Down', '\u2198 Down-Right']
    
    state_legend = [
        patches.Patch(facecolor=state_colors[0], edgecolor='black', label='Low State'),
        patches.Patch(facecolor=state_colors[1], edgecolor='black', label='Medium State'),
        patches.Patch(facecolor=state_colors[2], edgecolor='black', label='High State')
    ]
    
    direction_legend = [plt.Line2D([0], [0], color=direction_colors[i], lw=4, 
                                   label=f'{i+1}: {direction_labels[i]}') 
                       for i in range(8)]
    
    all_legend = state_legend + direction_legend
    
    if meta_brain:
        fig.legend(handles=all_legend, loc='upper center', bbox_to_anchor=(0.5, 0.98),
                  ncol=6, title='States & Dominant Incoming Direction', fontsize=9)
    else:
        ax1.legend(handles=all_legend, loc='upper left', bbox_to_anchor=(1.02, 1), 
                  title='States & Direction', fontsize=9)
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches='tight')
    print(f"Visualization saved to: {output_file}")
    
    return output_file

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python visualize_dual_brain.py <trial_directory> [output_file]")
        print("Example: python visualize_dual_brain.py results/run_20260126_120000/phase_1_full_reward/trial_01/")
        sys.exit(1)
    
    trial_dir = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else os.path.join(trial_dir, 'dual_brain_visualization.png')
    
    visualize_dual_brain(trial_dir, output_file)
