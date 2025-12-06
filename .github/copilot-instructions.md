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

### Synapse Reinforcement and Decay Dynamics (CRITICAL DESIGN)
This system implements a **self-regulating competitive learning mechanism** with sophisticated equilibrium properties:

**Parameters:**
- `reinforcement_amount = 10` (additive per firing in `brain_module.f90`)
- `decay_multiplier = 0.9 to 1.0` (random per step in `synapses_module.f90`)
- `min_synapse_strength = 1` (hard floor)
- `max_synapse_strength = 200000` (hard ceiling)

**Key Properties:**
1. **Non-linear ratio-based selection**: Probabilities determined by synapse strength ratios, not absolute values
   - Synapse A=1, B=10 → B is 10× more likely
   - After decay: A=1, B=9 → B is still 9× more likely
   - Relative advantage changes slowly, creating stable pathway preferences

2. **Equilibrium zone** (~100-200): Heavily-used synapses reach saturation where reinforcement balances decay
   - At equilibrium: +10 reinforcement ≈ ×(0.9-1.0) decay
   - Overuse doesn't dominate - hits ceiling effect
   - Random decay (0.9-1.0) creates "fuzzy equilibrium zone" rather than hard limit

3. **Usage-dependent competition**:
   - Frequently used pathways: oscillate near equilibrium strength
   - Rarely used pathways: decay to floor (1), becoming background noise
   - Multiple active pathways can coexist at similar strengths (fair competition)
   - This prevents runaway winners while maintaining learned patterns

4. **Biological analogy**: Resembles homeostatic plasticity - the system naturally forms stable pathway preferences without any single route completely dominating.

**DO NOT** change reinforcement/decay balance without understanding these emergent properties. The current values create intentional saturation behavior that prevents single pathways from monopolizing signal flow.

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

**Use the Makefile** - The project uses NVIDIA nvfortran compiler:

```bash
# Build main executable
make

# Build specific program
make test_trinary

# Build all programs
make all-programs

# Clean all build artifacts
make clean

# Clean and rebuild
make rebuild

# See all available targets
make help
```

**Compilation order matters** due to module dependencies:
- `trinary_module.f90` (base)
- `synapses_module.f90` 
- `outputter_module.f90`
- `inputter_module.f90`
- `brain_module.f90` (depends on synapses and outputter)
- Main program files

**Manual compilation** (if not using Makefile):
```bash
nvfortran trinary_module.f90 synapses_module.f90 outputter_module.f90 inputter_module.f90 brain_module.f90 forWhoseAdvantage.f90 -o forWhoseAdvantage
```

**Execution requires 8 command-line arguments:**
```bash
./forWhoseAdvantage <rows> <cols> <input_offset> <input_length> <output_offset> <output_length> <print_synapses_flag> <max_steps>
```

Example: `./forWhoseAdvantage 6 12 6 6 1 6 false 10`

**Validation**: `input_offset + input_length - 1 ≤ cols` and `output_offset + output_length - 1 ≤ cols`

## File Artifacts

`.mod` files and compiled binaries are **build artifacts** - treat them like `.o` object files:
- **Never commit** `.mod` files to version control
- **Always run** `make clean` when switching compilers or encountering module errors
- `.mod` files are compiler-specific (nvfortran vs gfortran) and version-specific
- Run `make clean` before building after git pull/checkout if encountering "not a GNU Fortran module file" errors

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
