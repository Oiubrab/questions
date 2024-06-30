# questions
This project aims to create a simulated brain-like system using Fortran. It involves initializing and evolving a grid of trinary states (`low`, `medium`, `high`) and their associated synapses over time. The system incorporates an `inputter` array that feeds values into the brain matrix and an `outputter` array that reflects changes in the brain matrix.

## Features

- Initialize and update a 2D brain matrix with trinary states.
- Manage input and output arrays to interact with the brain matrix.
- Simulate synaptic connections and decay over time.
- Optionally print the state of synapses at each step.

## Prerequisites

- Fortran compiler (e.g., `nvfortran`)

## Installation

1. Clone the repository:

    ```bash
    git clone <repository-url>
    cd <repository-directory>
    ```

2. Compile the project:

    ```bash
    nvfortran trinary_module.f90 brain_module.f90 inputter_module.f90 outputter_module.f90 synapses_module.f90 forWhoseAdvantage.f90 -o forWhoseAdvantage
    ```

## Usage

Run the compiled program with the following arguments:

```bash
./forWhoseAdvantage <rows> <cols> <offset> <input_length> <print_synapses_flag>
```

- <rows>: Number of rows in the brain matrix.
- <cols>: Number of columns in the brain matrix.
- <offset>: Offset position for the inputter array in the brain matrix.
- <input_length>: Length of the inputter and outputter arrays (must be less than or equal to the number of columns).
- <print_synapses_flag>: Boolean flag to enable (true) or disable (false) synapse printout.

### Example

To run the program with 3 rows, 4 columns, an offset of 1, an input length of 2, and synapse printout enabled:

bash
```bash
./forWhoseAdvantage 3 4 1 2 true
```
## Code Structure

- trinary_module.f90: Defines the trinary type and its operations.
- brain_module.f90: Manages the brain matrix initialization and updates based on synapses.
- inputter_module.f90: Initializes the inputter array and manages copying values to the brain matrix.
- outputter_module.f90: Initializes the outputter array based on the inputter array.
- synapses_module.f90: Manages synapse initialization, decay, and updates based on brain states.
- forWhoseAdvantage.f90: Main program that initializes all components and runs the main simulation loop.

## Project Evolution

The main simulation loop includes the following steps:

1. Copy non-low states from the inputter array to the top row of the brain matrix.
2. Update synapses based on the current state of the brain matrix.
3. Apply decay to the synapses.
4. Print the state of the inputter, brain, outputter, and optionally the synapses.
5. Pause for 1 second between each step to observe the evolution.

## Contributing

Feel free to submit issues or pull requests to improve the project. Contributions are welcome!
