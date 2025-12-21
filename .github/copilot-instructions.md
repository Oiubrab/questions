# ForWhoseAdvantage - AI Coding Instructions

This is a Fortran 90 simulation modeling brain-like behavior using trinary state cells, synaptic connections, and probabilistic state transitions. The system now includes a complete sensorimotor learning loop with vision-based reinforcement learning.

## Architecture Overview

The system uses a modular architecture with core modules supporting both the original simulation and a new cat-mouse learning system:

### Core Modules
- **`trinary_module.f90`**: Custom type with 3 states (low=0, medium=1, high=2). Encapsulates state via `set()`, `get()`, and `shift(up/down)` methods.
- **`brain_module.f90`**: 2D grid (brain matrix) representing neural cells. Contains complex probabilistic state propagation logic based on synaptic weights. Includes synapse usage tracking for selective reinforcement.
- **`inputter_module.f90`**: 1D array feeding stimuli into brain top row. Only non-low states are copied.
- **`outputter_module.f90`**: 1D array capturing states that propagate beyond brain bottom row. Uses `backup_outputter` module variable to preserve previous state.
- **`synapses_module.f90`**: 4D array (rows × cols × 8 incoming × 8 outgoing) storing connection strengths for context-dependent routing. Includes decay, reinforcement, and adaptive reinforcement/punishment mechanisms that scale with number of brain steps.
- **`vision_simulation_module.f90`**: Field simulation with angular vision system (6 slices × 60°), mouse/cat positioning, and movement logic with toroidal boundaries.

### Main Programs
- **`forWhoseAdvantage.f90`**: Original simulation with command-line parameter parsing.
- **`cat_mouse_learning.f90`**: Sensorimotor learning simulation - cat learns to chase mouse using vision input and motor output with direction-based reinforcement (reward for moving towards mouse).
- **`cat_mouse_gui_demo.f90`**: GUI-compatible version outputting CSV for real-time Pygame visualization.

### Supporting Files
- **`cat_mouse_gui.py`**: Pygame visualization showing cat (blue triangle), mouse (red circle), vision rays, and info panel.
- **`run_learning_tests.sh`**: Multi-trial testing framework with statistical analysis (mean ± std dev).
- **`test_4d_mechanics.f90`**: Comprehensive test suite validating signal propagation, directional routing, and reward system with 4D synapses.
- **`visualize_brain.py`**: Matplotlib brain visualization with color-coded arrows showing dominant incoming direction for each connection.
- **`BAR_STRUCTURE.md`**: Documentation of temporal organization (multiple brain steps per real-world step).

## Critical Patterns

### Module Interdependencies
All modules depend on `trinary_module`. The `brain_module` imports `synapses_module` and `outputter_module`. When modifying types, check reverse dependencies.

### Trinary State Management
Never directly access `trinary%value` - always use methods:
```fortran
call cell%set(medium)
state = cell%get()
call cell%shift(up)  ! Moves toward high, capped
```

### Direction System (Brain Module)
8-direction array with **biased probabilities** - downward movement (indices 6-8) has higher bias (1.5-1.8×) than upward (0.5×) or lateral (1.0×). The `directions` array maps indices to (row_delta, col_delta) pairs.

### 4D Directional Routing (MAJOR ARCHITECTURE)
**Context-dependent pathway selection**: Each neuron has 64 connection strengths (8 incoming × 8 outgoing directions). The system selects which outgoing synapses to use based on where the signal came from:
- **`synapses(row, col, incoming_dir, outgoing_dir)`**: 4D array stores routing preferences
- **`incoming_direction(row, col, 2)`**: Tracks up to 2 incoming directions per neuron (for HIGH state)
- **`direction_opposites`**: Maps each direction to its opposite (1↔8, 2↔7, 3↔6, 4↔5) for signal origin tracking
- **Signal propagation**: When neuron receives signal, records where it came from. When neuron fires, uses that incoming direction to select which outgoing synapses to activate.
- **HIGH neurons**: Average synapse values from both incoming directions when selecting output
- **Enables learning with moving targets**: Different input patterns can learn different routes through the brain

### Synapse Reinforcement and Decay Dynamics (CRITICAL DESIGN)
This system implements a **self-regulating competitive learning mechanism** with sophisticated equilibrium properties:

**Base Parameters:**
- `reinforcement_amount = 1000` (additive per firing in `brain_module.f90`)
- `decay_multiplier = 0.9 to 1.0` (random per step, applied to ALL synapses every brain step)
- `min_synapse_strength = 1` (hard floor)
- `max_synapse_strength = 2000000` (hard ceiling)

**Adaptive Reinforcement (Learning System):**
The learning system uses adaptive reinforcement that scales with `steps_per_bar`:
- **Reward formula**: `1.05 / (0.95^steps_per_bar)` - counteracts decay plus adds ~5% growth
- **Punishment formula**: `0.8 * (0.95^steps_per_bar)` - amplifies decay for failed pathways (NOTE: multiplies, not divides!)
- **Randomness**: ±10% variation in multiplier for exploration
- **Selective application**: Only affects synapses that fired during the Bar (tracked via `synapse_usage(row,col,incoming,outgoing)` 4D array)
- **Direction-based reward**: Cat rewarded for moving TOWARDS mouse (dot product of movement vector with desired direction > 0), independent of actual distance change

**Key Properties:**
1. **Global decay maintains equilibrium**: ALL synapses decay toward floor (25), including used ones - usage combats decay through reinforcement but pathways must be active regularly to maintain bias, otherwise they tend toward floor ensuring new patterns can always form
2. **Selective reinforcement for credit assignment**: Only synapses used in current Bar are rewarded/punished based on outcome
3. **Pathway tracking**: Boolean `synapse_usage(rows, cols, 8)` marks which synapses fire, reset each Bar
4. **Non-linear ratio-based selection**: Probabilities determined by synapse strength ratios, not absolute values
   - Synapse A=1, B=10 → B is 10× more likely
   - After decay: A=1, B=9 → B is still 9× more likely
   - Relative advantage changes slowly, creating stable pathway preferences

5. **Equilibrium zone** (~100-200): Heavily-used synapses reach saturation where reinforcement balances decay
   - At equilibrium: immediate +1000 boost + periodic adaptive reinforcement ≈ continuous decay
   - Random decay (0.9-1.0) creates "fuzzy equilibrium zone" rather than hard limit
   - Multiple active pathways can coexist at similar strengths (fair competition)

**DO NOT** change reinforcement/decay balance without understanding these emergent properties. The current values create intentional saturation behavior that prevents single pathways from monopolizing signal flow while allowing learning through selective reinforcement.

### State Propagation Logic
In `update_brain_state_based_on_synapses()`:
1. Neurons with `incoming_direction = 0` are skipped (must receive signal first to activate)
2. Valid moves check target isn't `high` (prevents overload)
3. For MEDIUM neurons: use `synapses(i, j, incoming_dir, k)` - single direction group
4. For HIGH neurons: average `synapses(i,j,incoming_dir,k)` and `synapses(i,j,incoming_dir2,k)` from both incoming directions
5. Synapse weights × direction bias determine move probabilities
6. Cumulative probability distribution used for weighted random selection
7. Successful moves shift source `down` and target `up`
8. Set target's `incoming_direction` using `direction_opposites` to track signal origin
9. Immediate reinforcement: Add `reinforcement_amount` (1000) to synapse that fired
10. Track usage: Mark `synapse_usage(i, j, incoming_dir, outgoing_dir) = .true.` for selective learning (4D indexing)
11. Special case: last row can propagate into `outputter` array

### Bar Structure (Temporal Organization)
A **Bar** represents one real-world time step, containing multiple brain processing steps:
- Current default: `steps_per_bar = 12` (12 brain updates per real-world step)
- Enables signal propagation from input (top row) to output (bottom row) within single timestep
- **Critical for learning**: Decay happens once per Bar, not per brain step - this allows vision→movement patterns to persist across brain processing cycles
- **Pattern persistence**: Multiple directional pathways can coexist as mouse moves between vision sectors
- See `BAR_STRUCTURE.md` for detailed explanation

### Learning Loop (cat_mouse_learning.f90)
Each Bar follows this sequence:
1. **Reset tracking**: `reset_synapse_usage()` for this Bar
2. **Move mouse**: Currently stationary at center (50, 50) for testing
3. **Update vision**: Calculate which slice (1-8) contains mouse, set inputter
4. **Apply input**: Copy vision to brain top row, set `incoming_direction = 7` (Down)
5. **Brain processing**: Run `steps_per_bar` (12) iterations
   - Each step: update brain state using 4D synapses based on incoming_direction
   - Track which synapses fire in `synapse_usage(row, col, incoming, outgoing)`
6. **Apply decay**: Once per Bar (after all brain steps)
7. **Extract output**: Find strongest output direction
8. **Move cat**: Apply movement if output > 0
9. **Direction-based reward**: Calculate normalized dot product of cat movement with desired direction (cat → mouse)
10. **Selective reinforcement**: If cat moved:
    - Dot product > 0 (moved towards): `apply_adaptive_reinforcement()` to used synapses
    - Dot product < 0 (moved away): `apply_adaptive_punishment()` to used synapses
    - Dot product = 0 (perpendicular): no selective learning

### Direction-Based Reward (Critical Design)
**Reward based on cat's decision, not outcome:**
- Calculate unit vector from cat → mouse (desired direction)
- Calculate cat's movement vector
- Dot product gives component of movement in desired direction
- **Eliminates mouse movement problem**: Reward independent of whether distance actually decreased
- **Enables learning with moving targets**: Cat learns to move towards where mouse IS, not where it WAS

### Module-Level State
`outputter_module` maintains `backup_outputter` as module-scoped variable for state persistence across time steps. This is unusual - most data flows through subroutine parameters.

## Build & Run

**Use the Makefile for all builds** - it auto-detects available compiler (nvfortran preferred, gfortran fallback):
```bash
make clean          # Clean build artifacts
make learning       # Build cat-mouse learning system
make                # Build main forWhoseAdvantage executable  
make all-programs   # Build all executables
```

**Manual compilation order** (if needed) due to module dependencies:
```bash
nvfortran trinary_module.f90 brain_module.f90 inputter_module.f90 outputter_module.f90 synapses_module.f90 forWhoseAdvantage.f90 -o forWhoseAdvantage
```

Alternative compiler: replace `nvfortran` with `gfortran`

**Cat-Mouse Learning System Testing:**
```bash
./run_learning_tests.sh    # Multi-trial learning experiment with statistics
./run_gui.sh              # Real-time GUI visualization  
make learning && ./cat_mouse_learning  # Single learning trial
```

**Original simulation execution** requires 7 command-line arguments:
```bash
./forWhoseAdvantage <rows> <cols> <input_offset> <input_length> <output_offset> <output_length> <print_synapses_flag>
```

Example: `./forWhoseAdvantage 6 12 6 6 1 6 false`

**Validation**: `input_offset + input_length - 1 ≤ cols` and `output_offset + output_length - 1 ≤ cols`

## Testing Tools

- **`run_learning_tests.sh`**: Comprehensive 5-trial learning experiment with statistical analysis (mean ± std dev, learning detection)
- **`run_gui.sh`**: Real-time pygame visualization of cat-mouse learning behavior 
- **`cat_mouse_learning`**: Single trial learning simulation with CSV logging
- **`cat_mouse_gui_demo`**: GUI-compatible learning demo outputting real-time state data

All testing tools use intelligent compiler detection and work with both nvfortran and gfortran.

## File Artifacts

`.mod` files and compiled binaries (`forWhoseAdvantage`, `modify_array`) are build artifacts. Source files are `.f90` only.

## Utility Files

- **`parallel_test.f90`**: OpenMP test demonstrating parallel region and parallel loop patterns with `omp_lib`
- **`modify_array.f90`**: Random array manipulation example using `random_utils` module
- **`hello_world.f90`**: Minimal Fortran program template

These are standalone examples, not part of the main simulation.

## Debugging Tips

- Synapse array dimensions are `(rows, cols, 8)` - third dimension is fixed
- `allocatable` arrays must be allocated before use; check allocation status if encountering segfaults
- Random number generator seeded with `call random_seed()` - results vary between runs
- Use `print_synapses=true` flag to inspect synapse states during debugging
