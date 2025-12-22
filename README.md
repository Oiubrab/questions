# ForWhoseAdvantage

ForWhoseAdvantage is a Fortran-based simulation that models brain-like behavior using trinary states (`low`, `medium`, `high`) with **4D directional routing** - a context-dependent pathway selection system where each neuron learns different routes based on signal origin. The system has achieved a breakthrough **70.1% directional learning accuracy** with specialized neural pathways.

## Recent Breakthrough 🎯

**Major Achievement**: Removed counterproductive anti-oscillation punishment, resulting in:
- **70.1% directional learning** (up from 61.1%)  
- **456 average catches** per trial (up from 284)
- **2.62:1 towards/away ratio** in movement decisions
- **Hyper-specialized neural pathways** with extreme selectivity
- **Assessment**: ✓✓✓ EXCEPTIONAL PERFORMANCE - Cat is an expert hunter!

## Features

- **4D Directional Routing**: Each neuron has 64 connection strengths (8 incoming × 8 outgoing directions) enabling context-dependent signal routing
- **Brain Matrix Simulation**: A 2D grid representing the brain with probabilistic state propagation
- **Vision System**: 8-slice angular vision (45° each) detecting mouse position
- **Motor Output**: 8-direction movement system with learned behaviors
- **Adaptive Reinforcement**: Direction-based reward system independent of environmental changes
- **Energy Conservation**: Signals propagate through the brain preserving total energy
- **Comprehensive Testing**: Test suite validating signal propagation, routing, and learning mechanics
- **Visualization**: Color-coded brain state showing dominant incoming directions for each synapse

## Architecture

### Core Modules
- **`trinary_module.f90`**: Trinary state type with encapsulated operations
- **`brain_module.f90`**: Brain grid with 4D synaptic routing and incoming direction tracking
- **`synapses_module.f90`**: 4D synapse array with decay, reinforcement, and punishment
- **`inputter_module.f90`**: Vision input interface (8 angular slices)
- **`outputter_module.f90`**: Motor output interface (8 movement directions)
- **`vision_simulation_module.f90`**: Cat-mouse field simulation with angular vision

### Main Programs
- **`forWhoseAdvantage.f90`**: Original simulation with command-line parameters
- **`cat_mouse_learning.f90`**: Sensorimotor learning with direction-based reward
- **`cat_mouse_gui_demo.f90`**: Real-time GUI visualization version

### Testing & Analysis
- **`run_learning_tests.sh`**: 30-trial statistical analysis with oscillation detection
- **`quick_test.sh`**: Fast 5-trial evaluation for development  
- **`single_trial_gui.sh`**: Single trial with immediate GUI visualization
- **`analyze_brain_pathways.py`**: Comprehensive analysis of learned neural pathways
- **`brain_summary.py`**: High-level summary of brain specialization patterns
- **`visualize_brain.py`**: Matplotlib brain visualization with color-coded directional routing
- **`cat_mouse_gui.py`**: Pygame real-time visualization
- **`detect_oscillation.py`**: Movement efficiency analysis

## Prerequisites

- **Fortran Compiler**: `nvfortran` (preferred) or `gfortran`
- **Python 3**: For visualization tools
- **Required Python packages**: `matplotlib`, `numpy`, `pygame` (for GUI)

## Installation

1. **Clone the Repository**:

    ```bash
    git clone git@github.com:Oiubrab/questions.git
    cd questions
    ```

2. **Install Python Dependencies**:

    ```bash
    pip install matplotlib numpy pygame
    ```

3. **Compile the Project**:

    Use the Makefile for automatic compiler detection:

    ```bash
    make clean          # Clean build artifacts
    make learning       # Build cat-mouse learning system
    make all-programs   # Build all executables
    ```

    Or compile manually:

    ```bash
    nvfortran trinary_module.f90 synapses_module.f90 outputter_module.f90 \
              inputter_module.f90 brain_module.f90 vision_simulation_module.f90 \
              cat_mouse_learning.f90 -o cat_mouse_learning
    ```

## Usage

### Cat-Mouse Learning System (Recommended)

**Full 30-Trial Analysis** with comprehensive statistics:

```bash
./run_learning_tests.sh    # Full 30-trial analysis with GUI visualization
./run_learning_tests.sh --no-gui    # Analysis only, no GUI
```

**Quick Development Testing**:

```bash
./quick_test.sh           # Fast 5-trial evaluation  
./single_trial_gui.sh     # Single trial with immediate GUI
```

**Single Trial**:

```bash
./cat_mouse_learning <random_seed>
# Example: ./cat_mouse_learning 1000
```

**Current Performance (Latest Results):**
- **Directional Learning**: 70.1% towards mouse (✓✓ STRONG)
- **Average Catches**: 456 per 20,000 Bars  
- **Success Rate**: 100% (all trials show learning)
- **Temporal Learning**: +18-26% improvement from early to late epochs
- **Assessment**: ✓✓✓ EXCEPTIONAL PERFORMANCE

### Visualization Tools

### Brain Analysis Tools

**Analyze Learned Neural Pathways** (after learning trials):

```bash
python3 analyze_brain_pathways.py    # Detailed pathway strength analysis
python3 brain_summary.py            # High-level specialization summary
```

These tools reveal:
- **Input pathway strengths** by vision slice (some 5× stronger than others)
- **Output pathway preferences** by movement direction  
- **Ultra-strong individual synapses** (up to 1.8M strength vs 25 baseline)
- **Vision-to-movement specialization patterns**
- **Architectural insights** about learned hunting strategies

**Brain State Visualization** (after learning run):
```bash
python3 visualize_brain.py
```
Generates `brain_visualization.png` showing:
- Neuron states (gray=low, green=medium, red=high)
- Synapse connections colored by dominant incoming direction
- Input/output arrays

**Real-time GUI** (during learning):
```bash
./run_gui.sh              # Quick single trial with GUI
./run_learning_tests.sh   # Full 30-trial analysis ending with GUI replay
```
Shows cat (blue triangle), mouse (red circle), vision rays, and learned hunting behavior.

### Testing Core Mechanics

Validate 4D routing, energy conservation, and reward system:

```bash
nvfortran trinary_module.f90 synapses_module.f90 outputter_module.f90 \
          inputter_module.f90 brain_module.f90 test_4d_mechanics.f90 \
          -o test_4d_mechanics
./test_4d_mechanics
```

### Original Simulation

Run the basic brain simulation:

```bash
./forWhoseAdvantage <rows> <cols> <input_offset> <input_length> <output_offset> <output_length> <print_synapses>
# Example: ./forWhoseAdvantage 6 12 6 6 1 6 false
```

## Key Learning Breakthrough 🧠

The major breakthrough came from **removing anti-oscillation punishment** that was counterproductive:

- **Problem**: System was punishing "correct" directional moves if they became repetitive (>40% of recent moves)  
- **Solution**: Always reward movement toward mouse, regardless of repetition
- **Result**: System developed **hyper-specialized neural pathways** instead of fighting itself

**What the brain learned**:
- **Vision slice 7** (270°-315°) became 5× stronger than others - hyper-sensitivity to left-downward mouse positions
- **RIGHT movement** became dominant output (1.3M total pathway strength)  
- **Ultra-strong synapses** (1.8M vs 25 baseline) create learned "superhighways"
- **Specialized hunting patterns** rather than general intelligence

This represents **embodied cognitive learning** - the system developed muscle memory for specific hunting scenarios.

## Key Concepts
Each neuron maintains 64 synaptic strengths organized as an 8×8 matrix:
- **Rows**: 8 possible incoming signal directions
- **Columns**: 8 possible outgoing signal directions
- **Behavior**: Neuron selects which outgoing synapses to use based on where the signal came from
- **Benefit**: Different input patterns can learn different routes through the brain

### Direction-Based Reward
Learning uses the direction of cat movement, not the outcome:
- Calculate unit vector: cat → mouse (desired direction)
- Measure: dot product with cat's actual movement
- Reward: positive dot product (moved towards mouse)
- Punish: negative dot product (moved away)
- **Critical advantage**: Reward independent of mouse movement, enables learning with moving targets

### Temporal Organization (Bars)
One **Bar** = one real-world time step containing:
- 12 brain processing steps (signal propagation)
- 1 decay application (after all brain steps)
- Reinforcement/punishment only at Bar boundaries
- See `BAR_STRUCTURE.md` for detailed explanation

### Adaptive Learning
- **Immediate reinforcement**: +1000 per synapse firing (encourages active pathways)
- **Global decay**: ×0.92-0.98 per Bar on ALL synapses (maintains exploration)
- **Selective reward**: `1.05/(0.95^12)` ≈ 2.0× on synapses that fired when cat moved towards mouse
- **Selective punishment**: `0.8×(0.95^12)` ≈ 0.43× on synapses that fired when cat moved away

## Code Structure

- **`trinary_module.f90`**
  Trinary state type (low=0, medium=1, high=2) with encapsulated operations.

- **`brain_module.f90`**
  Brain grid with 4D synaptic routing, incoming direction tracking, and probabilistic propagation.

- **`synapses_module.f90`**
  4D synapse array (rows × cols × 8 incoming × 8 outgoing) with decay, reinforcement, and punishment.

- **`inputter_module.f90`**
  Vision input interface - 8 angular slices detecting mouse position.

- **`outputter_module.f90`**
  Motor output interface - 8 movement directions with learned activation.

- **`vision_simulation_module.f90`**
  Cat-mouse field simulation with angular vision system and movement logic.

- **`cat_mouse_learning.f90`**
  Main sensorimotor learning program with direction-based reward system.

- **`test_4d_mechanics.f90`**
  Comprehensive test suite validating core 4D mechanics and reward systems.

- **`visualize_brain.py`**
  Matplotlib visualization showing learned brain structure with color-coded directional routing.

- **`forWhoseAdvantage.f90`**
  Original simulation program with command-line parameter control.

## Learning System Overview

The cat-mouse learning system demonstrates sensorimotor learning through these key steps:

1. **Vision Input**:
   - 8 angular slices (45° each) detect mouse position
   - Active slice sets corresponding inputter cell to MEDIUM
   - Vision pattern copied to brain top row with `incoming_direction = 7` (Down)

2. **Brain Processing** (12 steps per Bar):
   - Signals propagate through brain using 4D directional routing
   - Each neuron selects outgoing synapses based on incoming signal direction
   - HIGH neurons average values from both incoming directions
   - Synapse usage tracked for credit assignment

3. **Motor Output**:
   - Signals reaching bottom row activate corresponding output cells
   - Strongest output determines movement direction (1-8)
   - Cat moves 5 units in selected direction

4. **Reinforcement Learning**:
   - Calculate desired direction: unit vector cat → mouse
   - Measure cat movement direction
   - Dot product > 0: reward synapses that fired
   - Dot product < 0: punish synapses that fired
   - Global decay applied to all synapses every Bar

5. **Emergent Behavior**:
   - Cat learns to move towards mouse over ~200-1200 Bars
   - Different vision inputs learn different motor outputs
   - Context-dependent routing enables complex behaviors
   - **Pause**: The simulation pauses briefly (1 second) between steps to allow observation.

3. **Termination**:
   - The simulation runs for a predefined number of steps (`max_steps`), after which it terminates.

## Interpretation of Output

- **Inputter Array**: Shows the current states of the inputter, aligned according to the offset.
- **Brain Matrix**: Displays the trinary states of each cell in the brain matrix.
  - `0`: Low state
  - `1`: Medium state
  - `2`: High state
- **Outputter Array**: Reflects the states captured from the brain matrix's bottom row.
- **Synapses** (Optional): Lists synaptic strengths for each cell when `print_synapses_flag` is `true`.

## Customization and Experimentation

- **Adjust Parameters**: Modify the command-line arguments to experiment with different brain sizes, input patterns, and synapse behaviors.
- **Modify Input Patterns**: Alter the `initialize_inputter` subroutine in `inputter_module.f90` to change how the inputter array is initialized.
- **Change Synapse Dynamics**: Tweak the synaptic reinforcement and decay functions in `synapses_module.f90` to observe different emergent behaviors.

## Contributing

Contributions are welcome! If you'd like to improve the project or add new features:

1. **Fork the Repository**: Create a personal copy of the project.
2. **Create a Branch**: Develop your feature or fix in a new branch.
3. **Submit a Pull Request**: Describe your changes and submit a pull request for review.

Please ensure your code follows the project's coding standards and includes appropriate documentation.

## License

This project is open-source and available under the [GNU General Public License v3](LICENSE).

## Contact

For questions or suggestions, feel free to open an issue or contact the project maintainers.
