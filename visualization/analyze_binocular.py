#!/usr/bin/env python3
"""
Analyze binocular cat-mouse simulation results
"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Read CSV
df = pd.read_csv('binocular_simulation_log.csv')

print("=" * 60)
print("BINOCULAR SIMULATION ANALYSIS")
print("=" * 60)

# Basic stats
total_bars = len(df)
catches_total = df['catches'].iloc[-1]
bars_with_output = (df['output_action'] > 0).sum()
bars_with_movement = bars_with_output

print(f"\nTotal bars: {total_bars}")
print(f"Total catches: {catches_total}")
print(f"Bars with output: {bars_with_output} ({100*bars_with_output/total_bars:.1f}%)")
print(f"Catches per bar: {catches_total/total_bars:.3f}")

# Calculate distances
df['cat_to_mouse_dist'] = np.sqrt(
    (df['cat_x'] - df['mouse_x'])**2 + 
    (df['cat_y'] - df['mouse_y'])**2
)

# Check for catch events (when catches increases)
df['catch_event'] = df['catches'].diff().fillna(0) > 0
catch_bars = df[df['catch_event']]
print(f"\nCatch events: {len(catch_bars)}")
print(f"Min distance at catch: {catch_bars['cat_to_mouse_dist'].min():.2f}")
print(f"Max distance at catch: {catch_bars['cat_to_mouse_dist'].max():.2f}")
print(f"Mean distance at catch: {catch_bars['cat_to_mouse_dist'].mean():.2f}")

# Brain energy over time
print(f"\nBrain energy:")
print(f"  Epoch 1 avg: {df.iloc[:2000]['brain_energy'].mean():.1f}")
print(f"  Epoch 5 avg: {df.iloc[8000:10000]['brain_energy'].mean():.1f}")
print(f"  Epoch 10 avg: {df.iloc[18000:]['brain_energy'].mean():.1f}")

# Create visualizations
fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# 1. Catches over time
ax = axes[0, 0]
ax.plot(df['bar'], df['catches'], linewidth=1)
ax.set_xlabel('Bar')
ax.set_ylabel('Total Catches')
ax.set_title('Cumulative Catches Over Time')
ax.grid(True, alpha=0.3)

# Add epoch lines
for epoch in range(1, 11):
    ax.axvline(epoch * 2000, color='red', alpha=0.3, linestyle='--', linewidth=0.5)

# 2. Distance to mouse over time (sample)
ax = axes[0, 1]
sample_df = df.iloc[::50]  # Sample every 50th bar
ax.plot(sample_df['bar'], sample_df['cat_to_mouse_dist'], linewidth=0.5, alpha=0.7)
ax.axhline(8.0, color='red', linestyle='--', label='Catch radius (8.0)', linewidth=2)
ax.set_xlabel('Bar')
ax.set_ylabel('Distance to Mouse')
ax.set_title('Cat-Mouse Distance (sampled)')
ax.legend()
ax.grid(True, alpha=0.3)

# 3. Catches per epoch
ax = axes[1, 0]
epoch_size = 2000
epochs = []
catches_per_epoch = []
for epoch in range(10):
    start = epoch * epoch_size
    end = (epoch + 1) * epoch_size
    if start < len(df):
        catches_start = df.iloc[start]['catches']
        catches_end = df.iloc[min(end-1, len(df)-1)]['catches']
        epochs.append(epoch + 1)
        catches_per_epoch.append(catches_end - catches_start)

ax.bar(epochs, catches_per_epoch, color='steelblue', alpha=0.7)
ax.set_xlabel('Epoch')
ax.set_ylabel('Catches')
ax.set_title('Catches Per Epoch')
ax.grid(True, alpha=0.3, axis='y')
ax.set_xticks(epochs)

# 4. Brain energy over time
ax = axes[1, 1]
sample_df = df.iloc[::50]
ax.plot(sample_df['bar'], sample_df['brain_energy'], linewidth=0.5, alpha=0.7, color='orange')
ax.set_xlabel('Bar')
ax.set_ylabel('Brain Energy')
ax.set_title('Brain Activation Over Time (sampled)')
ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('binocular_analysis.png', dpi=150)
print(f"\nPlot saved to: binocular_analysis.png")

# Check suspicious pattern - are catches happening every bar?
df['catches_diff'] = df['catches'].diff().fillna(0)
catch_frequency = df['catches_diff'].value_counts()
print(f"\nCatch frequency distribution:")
print(catch_frequency)

if (df['catches_diff'] > 0).sum() > total_bars * 0.5:
    print("\n⚠️  WARNING: Catches happening more than 50% of bars!")
    print("   This suggests catch radius may be too large or distance check is wrong")
