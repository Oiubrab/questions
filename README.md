# ForWhoseAdvantage

ForWhoseAdvantage is a Fortran-based simulation that models a brain-like system. It features a grid of trinary states (`low`, `medium`, `high`) and simulates synaptic connections and their evolution over time. The system includes an `inputter` array to feed stimuli into the brain matrix and an `outputter` array to capture the system's responses.

## Features

- **Brain Matrix Simulation**: A 2D grid representing the brain, where each cell holds a trinary state.
- **Inputter and Outputter Arrays**: Interfaces for feeding input into the brain matrix and capturing output from it.
- **Synaptic Connections**: Simulates synapses with dynamic strengths that evolve based on activity and decay over time.
- **Customizable Parameters**: Configure the size of the brain matrix, input/output arrays, and synapse behavior.
- **Optional Synapse Visualization**: Toggle the display of synapse states at each simulation step.

## Prerequisites

- **Fortran Compiler**: An environment capable of compiling Fortran 90 code (e.g., `nvfortran`, `gfortran`).

## Installation

1. **Clone the Repository**:

    ```bash
    git clone git@github.com:Oiubrab/questions.git
    cd question
    ```

2. **Compile the Project**:

    ```bash
    nvfortran trinary_module.f90 brain_module.f90 inputter_module.f90 outputter_module.f90 synapses_module.f90 forWhoseAdvantage.f90 -o forWhoseAdvantage
    ```

    *Replace `nvfortran` with your Fortran compiler if different.*

## Usage

Run the compiled program with the following syntax:

    ./forWhoseAdvantage <rows> <cols> <offset> <input_length> <print_synapses_flag>

### Parameters

- `<rows>`: **Integer**
  Number of rows in the brain matrix.

- `<cols>`: **Integer**
  Number of columns in the brain matrix.

- `<offset>`: **Integer**
  Starting column in the brain matrix where the `inputter` array is aligned.

- `<input_length>`: **Integer**
  Length of the `inputter` and `outputter` arrays. Must be less than or equal to the number of columns.

- `<print_synapses_flag>`: **Boolean** (`true` or `false`)
  Enables or disables the printing of synapse states at each simulation step.

### Example

To run the program with 6 rows, 12 columns, an offset of 6, an input length of 6, and synapse printout disabled:

    ./forWhoseAdvantage 6 12 6 6 false

## Code Structure

- **`trinary_module.f90`**
  Defines the `trinary` type and its operations, representing the trinary states of each cell.

- **`brain_module.f90`**
  Manages the brain matrix initialization and updates based on synaptic interactions.

- **`inputter_module.f90`**
  Initializes the `inputter` array and handles the transfer of input values into the brain matrix.

- **`outputter_module.f90`**
  Initializes the `outputter` array and manages the capture of output values from the brain matrix.

- **`synapses_module.f90`**
  Handles synapse initialization, updates, and decay mechanisms to simulate synaptic plasticity.

- **`forWhoseAdvantage.f90`**
  The main program that orchestrates the simulation, integrating all modules and managing the simulation loop.

## Simulation Overview

The simulation progresses through a series of steps, modeling the evolution of the brain matrix over time:

1. **Initialization**:
   - The brain matrix, inputter, outputter, and synapses are initialized based on user-defined parameters.
   - Synapses are assigned random strengths to simulate initial variability.

2. **Main Simulation Loop**:
   - **Input Integration**: Non-`low` states from the `inputter` array are copied to the top row of the brain matrix.
   - **State Update**: The brain matrix updates its states based on synaptic strengths and probabilistic state transitions.
   - **Synapse Decay**: Synaptic strengths undergo decay to simulate the weakening of unused connections.
   - **Output Capture**: States propagating beyond the bottom of the brain matrix are captured in the `outputter` array.
   - **Display**: The current states of the `inputter`, brain matrix, and `outputter` are printed. Synapses are displayed if enabled.
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
