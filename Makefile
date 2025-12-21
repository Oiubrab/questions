# Makefile for ForWhoseAdvantage Fortran simulation
# Auto-detect best available Fortran compiler

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

# Module dependencies
MODULES = trinary_module.f90 \
          synapses_module.f90 \
          outputter_module.f90 \
          inputter_module.f90 \
          brain_module.f90 \
          vision_simulation_module.f90

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
forWhoseAdvantage: $(MODULES) forWhoseAdvantage.f90
	@echo "Building forWhoseAdvantage with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) forWhoseAdvantage.f90 -o forWhoseAdvantage $(LDFLAGS)

# Cat-mouse learning programs
cat_mouse_learning: $(MODULES) cat_mouse_learning.f90
	@echo "Building cat_mouse_learning with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) cat_mouse_learning.f90 -o cat_mouse_learning $(LDFLAGS)

cat_mouse_gui_demo: $(MODULES) cat_mouse_gui_demo.f90
	@echo "Building cat_mouse_gui_demo with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) cat_mouse_gui_demo.f90 -o cat_mouse_gui_demo $(LDFLAGS)

evolutionary_learning: $(MODULES) evolutionary_learning.f90
	@echo "Building evolutionary_learning with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) evolutionary_learning.f90 -o evolutionary_learning $(LDFLAGS)

evolved_brain_gui_demo: $(MODULES) evolved_brain_gui_demo.f90
	@echo "Building evolved_brain_gui_demo with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) evolved_brain_gui_demo.f90 -o evolved_brain_gui_demo $(LDFLAGS)

targeted_evolution: $(MODULES) targeted_evolution.f90
	@echo "Building targeted_evolution with $(FC_NAME)"
	$(FC) $(FFLAGS) $(MODULES) targeted_evolution.f90 -o targeted_evolution $(LDFLAGS)

# Utility programs
modify_array: trinary_module.f90 modify_array.f90
	$(FC) $(FFLAGS) trinary_module.f90 modify_array.f90 -o modify_array $(LDFLAGS)

analyze_decay_math: analyze_decay_math.f90
	$(FC) $(FFLAGS) analyze_decay_math.f90 -o analyze_decay_math $(LDFLAGS)

# Test programs
test_conservation: $(MODULES) test_conservation.f90
	$(FC) $(FFLAGS) $(MODULES) test_conservation.f90 -o test_conservation $(LDFLAGS)

test_energy_detailed: $(MODULES) test_energy_detailed.f90
	$(FC) $(FFLAGS) $(MODULES) test_energy_detailed.f90 -o test_energy_detailed $(LDFLAGS)

test_single_state: $(MODULES) test_single_state.f90
	$(FC) $(FFLAGS) $(MODULES) test_single_state.f90 -o test_single_state $(LDFLAGS)

test_synapse_decay: synapses_module.f90 test_synapse_decay.f90
	$(FC) $(FFLAGS) synapses_module.f90 test_synapse_decay.f90 -o test_synapse_decay $(LDFLAGS)

test_transitions: $(MODULES) test_transitions.f90
	$(FC) $(FFLAGS) $(MODULES) test_transitions.f90 -o test_transitions $(LDFLAGS)

test_trinary: trinary_module.f90 test_trinary.f90
	$(FC) $(FFLAGS) trinary_module.f90 test_trinary.f90 -o test_trinary $(LDFLAGS)

test_visual_diff: $(MODULES) test_visual_diff.f90
	$(FC) $(FFLAGS) $(MODULES) test_visual_diff.f90 -o test_visual_diff $(LDFLAGS)

# Build all executables
all-programs: $(MAIN_PROGRAMS)

# Clean build artifacts
clean:
	rm -f *.mod *.o $(MAIN_PROGRAMS) cat_mouse_learning_gfortran cat_mouse_gui_demo_gfortran

# Clean and rebuild
rebuild: clean forWhoseAdvantage

# Build learning system
learning: cat_mouse_learning
	@echo "Cat-mouse learning system built successfully with $(FC_NAME)"

# Help target
help:
	@echo "Available targets:"
	@echo "  make                  - Build main forWhoseAdvantage executable"
	@echo "  make all-programs     - Build all executables"
	@echo "  make clean            - Remove all build artifacts"
	@echo "  make rebuild          - Clean and rebuild main executable"
	@echo "  make <program>        - Build specific program (e.g., make test_trinary)"
	@echo ""
	@echo "Programs: $(MAIN_PROGRAMS)"

.PHONY: all all-programs clean rebuild help
