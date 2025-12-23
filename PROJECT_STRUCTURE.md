# Project Structure

This document describes the reorganized directory structure of the ForWhoseAdvantage brain simulation project.

## Directory Layout

```
.
├── bin/                          # Compiled executables and build artifacts
│   ├── cat_mouse_learning       # Main learning simulation executable
│   ├── forWhoseAdvantage        # Original simulation executable
│   └── *.mod                    # Fortran module files
│
├── src/                         # Source code
│   ├── modules/                 # Reusable Fortran modules
│   │   ├── trinary_module.f90
│   │   ├── brain_module.f90
│   │   ├── synapses_module.f90
│   │   ├── inputter_module.f90
│   │   ├── outputter_module.f90
│   │   ├── vision_simulation_module.f90
│   │   └── statistics_module.f90
│   │
│   ├── programs/                # Main program entry points
│   │   ├── forWhoseAdvantage.f90
│   │   ├── cat_mouse_learning.f90
│   │   └── cat_mouse_vision.f90
│   │
│   └── tests/                   # Test programs
│       ├── test_trinary.f90
│       ├── test_conservation.f90
│       ├── test_synapse_decay.f90
│       └── ...
│
├── scripts/                     # Shell scripts for running experiments
│   ├── run_learning_tests.sh   # Multi-trial testing framework
│   ├── single_trial_gui.sh     # Single trial with GUI
│   ├── run_gui.sh              # Real-time GUI visualization
│   └── quick_test.sh           # Quick validation tests
│
├── visualization/               # Python visualization and analysis tools
│   ├── cat_mouse_gui.py        # Pygame GUI for cat-mouse simulation
│   ├── visualize_brain.py      # Brain state visualization
│   ├── detect_oscillation.py   # Movement pattern analysis
│   ├── analyze_brain_pathways.py
│   └── brain_summary.py
│
├── results/                     # Test results, logs, and data files
│   ├── trial_*.csv             # Trial simulation data
│   ├── trial_*.log             # Trial output logs
│   ├── trial_*_oscillation.txt # Oscillation analysis
│   └── *.png                   # Generated visualizations
│
├── Makefile                     # Build system
├── README.md                    # Project documentation
├── LICENSE                      # Project license
└── BAR_STRUCTURE.md            # Temporal organization documentation
```

## Building the Project

All build commands remain the same:

```bash
make                  # Build main forWhoseAdvantage executable
make learning         # Build cat-mouse learning system
make clean           # Clean build artifacts
make all-programs    # Build all executables
```

Executables are created in the `bin/` directory.

## Running Experiments

Scripts are now in the `scripts/` directory:

```bash
./scripts/run_learning_tests.sh          # 30-trial learning experiment
./scripts/run_learning_tests.sh --no-gui # Fast mode without visualization
./scripts/single_trial_gui.sh            # Single trial with GUI replay
./scripts/run_gui.sh                     # Real-time GUI visualization
```

Results are saved to the `results/` directory.

## Visualization

Python visualization tools are in the `visualization/` directory:

```bash
python3 visualization/cat_mouse_gui.py < results/trial_1.csv
python3 visualization/visualize_brain.py
python3 visualization/detect_oscillation.py results/trial_1.csv
```

## Benefits of New Structure

1. **Clear Separation**: Source code, scripts, results, and binaries are organized
2. **Easier Navigation**: Related files are grouped together
3. **Cleaner Root**: Documentation and build files remain at top level
4. **Version Control**: Easier to .gitignore results/ and bin/ directories
5. **Scalability**: Easy to add new modules, programs, or scripts
