# ForWhoseAdvantage - AI Coding Instructions

This is a Fortran 90 simulation modeling brain-like behavior using trinary state cells, synaptic connections, and probabilistic state transitions. The system now includes a complete sensorimotor learning loop with vision-based reinforcement learning.

## Architecture Overview

The system uses a modular architecture with core modules supporting both the original simulation and a new cat-mouse learning system:

### Core Modules
- **`trinary_module.f90`**: Custom type with 3 states (low=0, medium=1, high=2). Encapsulates state via `set()`, `get()`, and `shift(up/down)` methods.
- **`brain_module.f90`**: 2D grid (brain matrix) representing neural cells. Contains complex probabilistic state propagation logic based on synaptic weights. Includes synapse usage tracking for selective reinforcement.
- **`inputter_module.f90`**: 1D array feeding stimuli into brain top row. Only non-low states are copied.
- **`outputter_module.f90`**: 1D array capturing states that propagate beyond brain bottom row. Uses `backup_outputter` module variable to preserve previous state.
- **`synapses_module.f90`**: 3D array (rows × cols × 8 directions) storing connection strengths. Includes decay, reinforcement, and adaptive reinforcement mechanisms that scale with number of brain steps.
- **`vision_simulation_module.f90`**: Field simulation with angular vision system (6 slices × 60°), mouse/cat positioning, and movement logic with toroidal boundaries.

### Main Programs
- **`forWhoseAdvantage.f90`**: Original simulation with command-line parameter parsing.
- **`cat_mouse_learning.f90`**: Sensorimotor learning simulation - cat learns to chase mouse using vision input and motor output with distance-based reinforcement.
- **`cat_mouse_gui_demo.f90`**: GUI-compatible version outputting CSV for real-time Pygame visualization.

### Supporting Files
- **`cat_mouse_gui.py`**: Pygame visualization showing cat (blue triangle), mouse (red circle), vision rays, and info panel.
- **`run_learning_tests.sh`**: Multi-trial testing framework with statistical analysis (mean ± std dev).
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
- **Punishment formula**: `0.95 / (0.95^steps_per_bar)` - amplifies decay for failed pathways
- **Randomness**: ±10% variation in multiplier for exploration
- **Selective application**: Only affects synapses that fired during the Bar (tracked via `synapse_usage` array)
- **Movement-conditional**: Only applies when cat actually moved (prevents rewarding random mouse movement)

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
1. Valid moves check target isn't `high` (prevents overload)
2. Synapse weights × direction bias determine move probabilities
3. Cumulative probability distribution used for weighted random selection
4. Successful moves shift source `down` and target `up`
5. Immediate reinforcement: Add `reinforcement_amount` (1000) to synapse that fired
6. Track usage: Mark `synapse_usage(i, j, index) = .true.` for selective learning
7. Special case: last row can propagate into `outputter` array

### Bar Structure (Temporal Organization)
A **Bar** represents one real-world time step, containing multiple brain processing steps:
- Current default: `steps_per_bar = 12` (12 brain updates per real-world step)
- Enables signal propagation from input (top row) to output (bottom row) within single timestep
- **Critical for learning**: Decay happens once per Bar, not per brain step - this allows vision→movement patterns to persist across brain processing cycles
- **Pattern persistence**: Multiple directional pathways can coexist as mouse moves between vision sectors
- See `BAR_STRUCTURE.md` for detailed explanation

### Learning Loop (cat_mouse_learning.f90)
Each Bar follows this sequence:
1. **Measure baseline**: `previous_distance` before any actions
2. **Move mouse**: Random walk (simulates dynamic environment)
3. **Update vision**: Calculate which slice (1-6) contains mouse, set inputter
4. **Reset tracking**: `reset_synapse_usage()` for this Bar
5. **Apply input**: Copy vision to brain top row
6. **Brain processing**: Run `steps_per_bar` iterations
   - Each step: update brain state, track synapse usage, apply global decay
7. **Extract output**: Find strongest output direction and distance
8. **Move cat**: Apply movement if output > 0
9. **Measure outcome**: `current_distance` after cat moved
10. **Selective reinforcement**: If cat moved:
    - Distance decreased: `apply_adaptive_reinforcement()` to used synapses
    - Distance increased: `apply_adaptive_punishment()` to used synapses
    - No change or no movement: no selective learning

### Movement-Conditional Learning
Critical design decision: reinforcement only applies when `move_distance > 0`
- Prevents learning from random mouse movement when cat is passive
- Ensures credit assignment targets cat's actual behavior
- Encourages exploration by only evaluating active decisions

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
