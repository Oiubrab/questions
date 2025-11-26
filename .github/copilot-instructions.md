# ForWhoseAdvantage - AI Coding Instructions

This is a Fortran 90 simulation modeling brain-like behavior using trinary state cells, synaptic connections, and probabilistic state transitions.

## Architecture Overview

The system uses a modular architecture with five core modules feeding into a main program:

- **`trinary_module.f90`**: Custom type with 3 states (low=0, medium=1, high=2). Encapsulates state via `set()`, `get()`, and `shift(up/down)` methods.
- **`brain_module.f90`**: 2D grid (brain matrix) representing neural cells. Contains complex probabilistic state propagation logic based on synaptic weights.
- **`inputter_module.f90`**: 1D array feeding stimuli into brain top row. Only non-low states are copied.
- **`outputter_module.f90`**: 1D array capturing states that propagate beyond brain bottom row. Uses `backup_outputter` module variable to preserve previous state.
- **`synapses_module.f90`**: 3D array (rows × cols × 8 directions) storing connection strengths. Each cell has 8 directional synapses with decay and reinforcement mechanisms.
- **`forWhoseAdvantage.f90`**: Main program orchestrating the simulation loop with command-line parameter parsing.

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

### Synapse Reinforcement
When a state successfully propagates through a synapse, it's reinforced by `reinforcement_amount=10` and capped at `max_synapse_strength=200000`. This implements Hebbian-like learning.

### State Propagation Logic
In `update_brain_state_based_on_synapses()`:
1. Valid moves check target isn't `high` (prevents overload)
2. Synapse weights × direction bias determine move probabilities
3. Cumulative probability distribution used for weighted random selection
4. Successful moves shift source `down` and target `up`
5. Special case: last row can propagate into `outputter` array

### Module-Level State
`outputter_module` maintains `backup_outputter` as module-scoped variable for state persistence across time steps. This is unusual - most data flows through subroutine parameters.

## Build & Run

**Compilation order matters** due to module dependencies:
```bash
nvfortran trinary_module.f90 brain_module.f90 inputter_module.f90 outputter_module.f90 synapses_module.f90 forWhoseAdvantage.f90 -o forWhoseAdvantage
```

Alternative compiler: replace `nvfortran` with `gfortran`

**Execution requires 7 command-line arguments:**
```bash
./forWhoseAdvantage <rows> <cols> <input_offset> <input_length> <output_offset> <output_length> <print_synapses_flag>
```

Example: `./forWhoseAdvantage 6 12 6 6 1 6 false`

**Validation**: `input_offset + input_length - 1 ≤ cols` and `output_offset + output_length - 1 ≤ cols`

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
