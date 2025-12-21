#!/bin/bash
# Run cat-mouse simulation with GUI visualization

# Detect available Fortran compiler and set executable name
if command -v nvfortran >/dev/null 2>&1; then
    GUI_EXEC="cat_mouse_gui_demo"
else
    GUI_EXEC="cat_mouse_gui_demo"
fi

echo "Starting Cat & Mouse Brain Simulation with GUI..."
echo "Press ESC or Q in the window to quit early"
echo ""

./$GUI_EXEC | python3 cat_mouse_gui.py
