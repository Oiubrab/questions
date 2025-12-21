#!/bin/bash

# Run Evolved Brain GUI Demo
# This script builds and runs the evolved brain visualization

echo "=== Evolved Brain GUI Demo ==="

# Check if we're in the right directory
if [ ! -f "evolved_brain_gui_demo.f90" ]; then
    echo "Error: Must run from the questions directory"
    exit 1
fi

# Build the demo if needed
if [ ! -f "evolved_brain_gui_demo" ] || [ "evolved_brain_gui_demo.f90" -nt "evolved_brain_gui_demo" ]; then
    echo "Building evolved brain demo..."
    make evolved_brain_gui_demo
    if [ $? -ne 0 ]; then
        echo "Build failed!"
        exit 1
    fi
fi

# Check for Python and pygame
if ! command -v python3 &> /dev/null; then
    echo "Python3 not found. Running text-only version..."
    ./evolved_brain_gui_demo
    exit 0
fi

# Try to import pygame
if ! python3 -c "import pygame" 2>/dev/null; then
    echo "Pygame not available. Running text-only version..."
    echo "To install pygame: pip install pygame"
    echo ""
    ./evolved_brain_gui_demo
    exit 0
fi

# Check if best brain file exists
if [ ! -f "best_brain_gen_15.dat" ]; then
    echo "Warning: best_brain_gen_15.dat not found"
    echo "The demo will use a fallback brain configuration"
    echo ""
fi

# Make the GUI script executable
chmod +x evolved_brain_gui.py

echo "Starting evolved brain GUI visualization..."
echo "Press ESC to exit the visualization"
echo ""

# Run the GUI
python3 evolved_brain_gui.py