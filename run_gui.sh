#!/bin/bash
# Run cat-mouse simulation with GUI visualization

echo "Starting Cat & Mouse Brain Simulation with GUI..."
echo "Press ESC or Q in the window to quit early"
echo ""

./cat_mouse_gui_demo | python3 cat_mouse_gui.py
