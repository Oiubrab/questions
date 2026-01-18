# Makefile for ForWhoseAdvantage Fortran simulation
# Auto-detect best available Fortran compiler
# Supports both CPU (OpenMP) and GPU (OpenACC) builds

# Detect available compiler
ifeq ($(shell command -v nvfortran 2>/dev/null),)
    FC = gfortran
    FC_NAME = gfortran
else
    FC = nvfortran
    FC_NAME = nvfortran
endif

FFLAGS =
LDFLAGS =

# GPU compute capability (RTX 4060 = cc89, adjust for your GPU)
GPU_CC ?= cc89

# Build mode: cpu (default) or gpu
# Usage: make learning MODE=gpu
MODE ?= cpu

# Enable optimization based on compiler and mode
ifeq ($(FC_NAME),nvfortran)
    ifeq ($(MODE),gpu)
        # GPU mode: OpenACC for GPU offloading
        FFLAGS += -O3 -acc -gpu=$(GPU_CC),mem:managed -Minfo=accel,opt -fast
        LDFLAGS += -acc -gpu=$(GPU_CC),mem:managed
        $(info Building with GPU acceleration (OpenACC) for $(GPU_CC))
    else
        # CPU mode: OpenMP for multicore CPU
        FFLAGS += -O3 -mp -Minfo=mp,vect -fast
        LDFLAGS += -mp
        $(info Building with CPU parallelism (OpenMP))
    endif
else ifeq ($(FC_NAME),gfortran)
    FFLAGS += -O3 -fopenmp -ftree-vectorize -ffast-math -march=native
    LDFLAGS += -fopenmp
    $(info Building with gfortran (OpenMP only))
endif

# Directory structure
SRC_DIR = src
MODULE_DIR = $(SRC_DIR)/modules
PROGRAM_DIR = $(SRC_DIR)/programs
TEST_DIR = $(SRC_DIR)/tests
BIN_DIR = bin

# Module dependencies
MODULES = $(MODULE_DIR)/trinary_module.f90 \
          $(MODULE_DIR)/synapses_module.f90 \
          $(MODULE_DIR)/outputter_module.f90 \
          $(MODULE_DIR)/inputter_module.f90 \
          $(MODULE_DIR)/brain_module.f90 \
          $(MODULE_DIR)/brain_engine_module.f90 \
          $(MODULE_DIR)/vision_simulation_module.f90

# Main programs
MAIN_PROGRAMS = forWhoseAdvantage \
                cat_mouse_learning \
                modify_array \
                analyze_decay_math \
                test_conservation \
                test_energy_detailed \
                test_single_state \
                test_synapse_decay \
                test_transitions \
                test_trinary \
                test_visual_diff

# Default target
all: forWhoseAdvantage

# Main simulation executable
forWhoseAdvantage: $(MODULES) $(PROGRAM_DIR)/forWhoseAdvantage.f90 | $(BIN_DIR)
	@echo "Building forWhoseAdvantage with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) $(PROGRAM_DIR)/forWhoseAdvantage.f90 -o $(BIN_DIR)/forWhoseAdvantage $(LDFLAGS)
	@echo "Executable created in $(BIN_DIR)/"

# Cat-mouse learning programs
cat_mouse_learning: $(MODULES) $(PROGRAM_DIR)/cat_mouse_learning.f90 | $(BIN_DIR)
	@echo "Building cat_mouse_learning with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) $(PROGRAM_DIR)/cat_mouse_learning.f90 -o $(BIN_DIR)/cat_mouse_learning $(LDFLAGS)
	@echo "Cat-mouse learning system built successfully with $(FC_NAME)"

cat_mouse_gui_demo: $(MODULES) $(PROGRAM_DIR)/cat_mouse_gui_demo.f90
	@echo "Building cat_mouse_gui_demo with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) $(PROGRAM_DIR)/cat_mouse_gui_demo.f90 -o $(BIN_DIR)/cat_mouse_gui_demo $(LDFLAGS)

evolutionary_learning: $(MODULES) $(PROGRAM_DIR)/evolutionary_learning.f90
	@echo "Building evolutionary_learning with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) $(PROGRAM_DIR)/evolutionary_learning.f90 -o $(BIN_DIR)/evolutionary_learning $(LDFLAGS)

evolved_brain_gui_demo: $(MODULES) $(PROGRAM_DIR)/evolved_brain_gui_demo.f90
	@echo "Building evolved_brain_gui_demo with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) $(PROGRAM_DIR)/evolved_brain_gui_demo.f90 -o $(BIN_DIR)/evolved_brain_gui_demo $(LDFLAGS)

targeted_evolution: $(MODULES) $(PROGRAM_DIR)/targeted_evolution.f90
	@echo "Building targeted_evolution with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) $(PROGRAM_DIR)/targeted_evolution.f90 -o $(BIN_DIR)/targeted_evolution $(LDFLAGS)

# Utility programs
modify_array: $(MODULE_DIR)/trinary_module.f90 $(TEST_DIR)/modify_array.f90
	$(FC) $(FFLAGS) $(MODULE_DIR)/trinary_module.f90 $(TEST_DIR)/modify_array.f90 -o $(BIN_DIR)/modify_array $(LDFLAGS)

analyze_decay_math: $(TEST_DIR)/analyze_decay_math.f90
	$(FC) $(FFLAGS) $(TEST_DIR)/analyze_decay_math.f90 -o $(BIN_DIR)/analyze_decay_math $(LDFLAGS)

# Test programs
test_conservation: $(MODULES) $(TEST_DIR)/test_conservation.f90
	$(FC) $(FFLAGS) $(MODULES) $(TEST_DIR)/test_conservation.f90 -o $(BIN_DIR)/test_conservation $(LDFLAGS)

test_energy_detailed: $(MODULES) $(TEST_DIR)/test_energy_detailed.f90
	$(FC) $(FFLAGS) $(MODULES) $(TEST_DIR)/test_energy_detailed.f90 -o $(BIN_DIR)/test_energy_detailed $(LDFLAGS)

test_single_state: $(MODULES) $(TEST_DIR)/test_single_state.f90
	$(FC) $(FFLAGS) $(MODULES) $(TEST_DIR)/test_single_state.f90 -o $(BIN_DIR)/test_single_state $(LDFLAGS)

test_synapse_decay: $(MODULE_DIR)/synapses_module.f90 $(TEST_DIR)/test_synapse_decay.f90
	$(FC) $(FFLAGS) $(MODULE_DIR)/synapses_module.f90 $(TEST_DIR)/test_synapse_decay.f90 -o $(BIN_DIR)/test_synapse_decay $(LDFLAGS)

test_transitions: $(MODULES) $(TEST_DIR)/test_transitions.f90
	$(FC) $(FFLAGS) $(MODULES) $(TEST_DIR)/test_transitions.f90 -o $(BIN_DIR)/test_transitions $(LDFLAGS)

test_trinary: $(MODULE_DIR)/trinary_module.f90 $(TEST_DIR)/test_trinary.f90
	$(FC) $(FFLAGS) $(MODULE_DIR)/trinary_module.f90 $(TEST_DIR)/test_trinary.f90 -o $(BIN_DIR)/test_trinary $(LDFLAGS)

test_visual_diff: $(MODULES) $(TEST_DIR)/test_visual_diff.f90
	$(FC) $(FFLAGS) $(MODULES) $(TEST_DIR)/test_visual_diff.f90 -o $(BIN_DIR)/test_visual_diff $(LDFLAGS)

# Build all executables
all-programs: $(MAIN_PROGRAMS)

# Directory creation rule  
$(BIN_DIR):
	@echo "Creating $(BIN_DIR) directory..."
	@mkdir -p $(BIN_DIR)

# Clean build artifacts
clean:
	rm -f *.mod *.o $(BIN_DIR)/* 

# Clean and rebuild
rebuild: clean forWhoseAdvantage

# Build learning system
learning: cat_mouse_learning

# Help target
help:
	@echo "Available targets:"
	@echo "  make                  - Build main forWhoseAdvantage executable (CPU)"
	@echo "  make all-programs     - Build all executables"
	@echo "  make clean            - Remove all build artifacts"
	@echo "  make rebuild          - Clean and rebuild main executable"
	@echo "  make <program>        - Build specific program (e.g., make test_trinary)"
	@echo ""
	@echo "GPU Acceleration (requires nvfortran):"
	@echo "  make learning MODE=gpu       - Build cat_mouse_learning for GPU"
	@echo "  make forWhoseAdvantage MODE=gpu"
	@echo "  GPU_CC=cc89                  - Set compute capability (RTX 4060=cc89, default)"
	@echo ""
	@echo "To verify GPU usage, run with: NVCOMPILER_ACC_TIME=1 ./bin/cat_mouse_learning"
	@echo ""
	@echo "Programs: $(MAIN_PROGRAMS)"

.PHONY: all all-programs clean rebuild help
