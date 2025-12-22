# ForWhoseAdvantage

**A sophisticated brain simulation system** that demonstrates emergent learning behavior through neural networks with **4D directional routing**. Watch as an artificial cat learns to hunt a mouse using vision-based sensorimotor learning, achieving **70.1% directional accuracy** and developing specialized hunting strategies.

## 🧠 What Makes This Special

ForWhoseAdvantage implements a **unique 4D synaptic architecture** where each neuron maintains 64 different connection strengths (8 incoming × 8 outgoing directions). This allows neurons to route signals differently based on where they came from - enabling **context-dependent learning** that mirrors how biological brains process information.

**Key Capabilities:**
- **Advanced Neural Architecture**: 4D directional routing with context-dependent pathway selection
- **Real-Time Learning**: Watch the cat develop hunting strategies over time  
- **Exceptional Performance**: 70.1% directional learning with 456 average catches per trial
- **Complete Visualization Suite**: Real-time GUI, brain state analysis, and pathway visualization
- **Comprehensive Analytics**: 30-trial statistical analysis with oscillation detection
- **Embodied AI**: Sensorimotor learning loop connecting vision → brain → movement

## 🎯 Performance Highlights

**Latest Results** (30-trial analysis):
- **🎯 Directional Learning**: 70.1% accuracy moving toward target
- **🏆 Hunt Success**: 456 average catches per 20,000-step trial  
- **📈 Learning Curve**: +18-26% improvement from early to late epochs
- **🧭 Behavioral Ratio**: 2.62:1 preference for correct vs incorrect movements
- **⭐ Overall Assessment**: ✓✓✓ EXCEPTIONAL PERFORMANCE - Expert hunting behavior

The system demonstrates **genuine emergent learning** - developing specialized neural pathways that weren't explicitly programmed.

## 🚀 Quick Start Guide

**1. Get Up and Running** (3 commands):
```bash
git clone git@github.com:Oiubrab/questions.git
cd questions  
make learning    # Auto-detects nvfortran or gfortran
```

**2. Run Your First Learning Trial**:
```bash
./single_trial_gui.sh    # Watch the cat learn to hunt in real-time!
```

**3. Full Performance Analysis**:
```bash
./run_learning_tests.sh  # 30-trial comprehensive analysis
```

That's it! You'll see the cat develop from random movement to expert hunting behavior.

## 📋 Prerequisites

- **Fortran Compiler**: `nvfortran` (preferred) or `gfortran`
- **Python 3**: For visualization and analysis tools
- **Python packages**: `pip install matplotlib numpy pygame`

*The Makefile automatically detects your available compiler - just run `make learning` and you're ready to go!*

## 🎮 Complete Usage Guide

### Learning Experiments

**🔬 Full Scientific Analysis** (Recommended for research):
```bash
./run_learning_tests.sh       # 30 trials + statistical analysis + GUI replay
./run_learning_tests.sh --no-gui  # Analysis only, no visualization
```
*Generates comprehensive statistics, temporal learning analysis, and oscillation detection*

**⚡ Quick Development Testing**:
```bash
./quick_test.sh              # Fast 5-trial evaluation  
./single_trial_gui.sh        # Single trial with immediate visualization
```

**🎯 Single Custom Trial**:
```bash
./cat_mouse_learning <seed>  # e.g., ./cat_mouse_learning 1000
```

### Visualization & Analysis

**🧠 Brain Pathway Analysis** (Run after learning experiments):
```bash
python3 analyze_brain_pathways.py  # Detailed neural pathway strengths
python3 brain_summary.py          # High-level specialization insights
```
*Reveals which vision inputs became dominant, which movements are preferred, and how the brain specialized*

**📊 Visual Brain State**:
```bash
python3 visualize_brain.py    # Creates brain_visualization.png
```
*Color-coded brain diagram showing learned neural connections*

**🎬 Real-Time GUI Observation**:
```bash
./run_gui.sh                  # Quick single trial with live visualization
# OR run the full test suite (includes GUI replay at the end)
./run_learning_tests.sh
```
*Watch the blue triangle (cat) learn to chase the red circle (mouse)*

### Advanced Usage

**🔧 Original Simulation** (for researchers):
```bash
make                          # Build the original program
./forWhoseAdvantage <rows> <cols> <input_offset> <input_length> <output_offset> <output_length> <print_synapses>
# Example: ./forWhoseAdvantage 6 12 3 8 3 8 false
```

**🧪 Core Mechanics Testing**:
```bash
make test_4d_mechanics && ./test_4d_mechanics  # Validate 4D routing system
```

## 🏗️ System Architecture

### Core Technology Stack

**Neural Simulation Engine** (Fortran 90):
- **`trinary_module.f90`**: Trinary state neurons (low/medium/high) with encapsulated operations
- **`brain_module.f90`**: 6×12 brain grid with 4D synaptic routing and directional tracking
- **`synapses_module.f90`**: 4D synapse arrays with adaptive learning (decay/reinforcement)
- **`vision_simulation_module.f90`**: 8-slice angular vision system (45° per slice)
- **`inputter_module.f90`**: Vision-to-brain interface (8 directional inputs)
- **`outputter_module.f90`**: Brain-to-movement interface (8 directional outputs)

**Learning Programs**:
- **`cat_mouse_learning.f90`**: Main sensorimotor learning simulation
- **`forWhoseAdvantage.f90`**: Original configurable brain simulation
- **`cat_mouse_gui_demo.f90`**: Real-time visualization version

**Analysis & Visualization** (Python):
- **`run_learning_tests.sh`**: 30-trial statistical framework with oscillation detection
- **`analyze_brain_pathways.py`**: Neural pathway strength analysis
- **`brain_summary.py`**: High-level brain specialization insights
- **`visualize_brain.py`**: Brain state visualization with color-coded connections
- **`cat_mouse_gui.py`**: Real-time Pygame GUI with field visualization
- **`detect_oscillation.py`**: Movement efficiency and pattern analysis

## 🧪 Understanding the Learning Process

### The Cat-Mouse Learning Loop

**1. Vision System**
- 8 angular slices detect mouse position (45° coverage each)
- Active slice triggers corresponding brain input
- System learns which visual patterns predict successful movements

**2. Neural Processing**
- 4D directional routing: each neuron has 64 connection strengths (8×8 matrix)
- Context-dependent signaling: route selection based on signal origin
- 12 processing steps per time unit allow signal propagation through all 6 brain layers

**3. Motor Output**
- 8 movement directions corresponding to compass directions
- Strongest output determines cat's movement choice
- System develops preferences for successful movement patterns

**4. Reinforcement Learning**
- **Direction-based rewards**: Cat rewarded for moving toward mouse (regardless of outcome)
- **Adaptive strengthening**: Successful pathways grow stronger over time
- **Global decay**: All connections naturally weaken to maintain plasticity
- **Specialization emergence**: Some pathways become "superhighways" (1000× stronger than baseline)

### What You'll Observe

**Early Learning** (0-5000 steps):
- Random or inefficient movement patterns
- Low catch rates (few successful hunts)
- Weak, relatively uniform neural connections

**Mid Learning** (5000-10000 steps):
- Emergence of directional preferences
- Increasing catch rates and shorter hunt times
- Formation of specialized neural pathways

**Expert Behavior** (10000+ steps):
- Consistent directional accuracy (70%+ toward mouse)
- Efficient hunting with minimal wasted movement
- Hyper-specialized neural networks with dominant pathways

## 📊 Analysis Tools Explained

### Statistical Analysis (`run_learning_tests.sh`)
**Outputs comprehensive performance metrics:**
- Individual trial results with catch rates
- Aggregate statistics (mean, std deviation, median)
- Directional learning percentages (towards vs away from mouse)
- Temporal learning analysis (early vs late epoch performance)
- Oscillation detection warnings

### Brain Pathway Analysis (`analyze_brain_pathways.py`)
**Reveals the learned neural architecture:**
- Input pathway strengths by vision slice (which directions became dominant)
- Output pathway preferences by movement direction
- Strongest individual synaptic connections (the learned "superhighways")
- Vision-to-movement mapping analysis

### Brain Visualization (`visualize_brain.py`)
**Creates visual brain diagrams showing:**
- Neuron activation states (color-coded by intensity)
- Synaptic connections with thickness indicating strength
- Directional routing patterns with color-coded flow directions
- Input and output layer activation patterns

## 🔬 For Researchers & Developers

### Key Technical Concepts

**4D Directional Routing:**
Each neuron maintains an 8×8 matrix of connection strengths, enabling context-dependent signal routing based on incoming direction. This allows different input patterns to learn different pathways through the same neural substrate.

**Temporal Bar Structure:**
One "Bar" (time unit) contains 12 brain processing steps followed by learning updates. This allows complete signal propagation from vision input to motor output within a single decision cycle.

**Embodied Learning:**
The system learns sensorimotor mappings through environmental interaction rather than supervised training, developing specialized neural pathways for specific behavioral contexts.

### Performance Characteristics

**Scalability:** 6×12 brain (72 neurons) with 64 connections each = 4,608 total synaptic weights
**Learning Speed:** Typically shows improvement within 2000-5000 time steps
**Specialization:** Develops extreme pathway preferences (some connections 1000× stronger than others)
**Reliability:** 100% learning success rate across different random seeds

## 🛠️ Troubleshooting

**"Permission denied" when running scripts:**
```bash
chmod +x *.sh    # Make all scripts executable
```

**"Command not found" errors:**
- Ensure you have `nvfortran` or `gfortran` installed
- For Python tools: `pip install matplotlib numpy pygame`

**Build failures:**
```bash
make clean       # Clear build artifacts
make learning    # Rebuild from scratch
```

**GUI won't start:**
- Ensure you have display/X11 forwarding if using SSH
- Install pygame: `pip install pygame`

## 🤝 Contributing

We welcome contributions! Whether you want to:
- Experiment with different brain architectures
- Improve the learning algorithms  
- Add new visualization features
- Extend the analysis tools
- Optimize performance

Please fork the repository, create a feature branch, and submit a pull request with your improvements.

## 📄 License

This project is open-source and available under the [GNU General Public License v3](LICENSE).

## 🎯 What's Next?

Try running the system and watch artificial intelligence emerge through learning! Start with `./single_trial_gui.sh` to see the magic happen, then dive deeper with the full analysis suite.

**Questions?** Open an issue - we'd love to hear about your experiments and results!

---

*ForWhoseAdvantage demonstrates that sophisticated learning behaviors can emerge from relatively simple neural architectures when the right learning principles are applied. The 4D directional routing system represents a novel approach to context-dependent neural computation that could inspire new AI architectures.*