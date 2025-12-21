# Bar-Structured Brain Processing

## Temporal Organization

The simulation now uses **"Bars"** (like musical bars) as the fundamental time unit:

### One Bar = One Real-World Step

Each Bar consists of:

1. **World State Update** (once)
   - Mouse moves 5 units
   - Cat position checked
   - Vision angle calculated → input slice determined

2. **Input Application** (once at Bar start)
   - Active vision slice copied to brain top row
   - Only happens ONCE per Bar

3. **Brain Processing** (10 steps per Bar)
   - Brain propagates state through 10 internal steps
   - Each step: cells pick directions, move state, reinforce synapses, decay all synapses
   - Output accumulates throughout these 10 steps

4. **Action Execution** (once at Bar end)
   - Output vector read
   - Cat moves based on accumulated output
   - Next Bar begins

## Why This Matters

### Before (1 brain step per real-world step):
- Input → brain → output in single step
- With 6-row brain, signal needs 6 real-world steps to propagate
- Mouse moves 30 units during propagation delay
- Output arrives too late to be useful

### After (10 brain steps per Bar):
- Input → 10 brain steps → output = within one real-world step
- Signal has time to propagate through all 6 rows
- Cat acts immediately after processing
- **Now we can reinforce based on outcome!**

## Reinforcement Learning Implications

With the Bar structure, we can now implement meaningful reinforcement:

1. **Clear causality**: 
   - Bar N: Input (vision slice 3) → Brain processing → Output (move right)
   - Bar N+1: Measure result (did distance decrease?)

2. **Credit assignment**:
   - Track which synapses fired during Bar N's 10 brain steps
   - Reward/punish those synapses based on Bar N+1's outcome

3. **Delayed reward**:
   - Can implement eligibility traces
   - Recently-used synapses get more credit/blame

## Parameters

- **steps_per_bar**: 10 (adjustable)
- **max_bars**: Number of real-world time steps
- **Brain rows**: 6 (so 10 steps gives ~1.6× propagation depth)

## Files Modified

- `cat_mouse_gui_demo.f90`: GUI demo with Bar structure
- `cat_mouse_learning.f90`: Long-run simulation with Bar structure
- Both now output: `bar,mouse_x,mouse_y,cat_x,cat_y,vision_slice,brain_energy,output_energy`

## Next Steps

Ready to implement reward-based reinforcement:
- Track synapse usage during each Bar
- Calculate reward (distance change)
- Apply differential reinforcement based on reward
