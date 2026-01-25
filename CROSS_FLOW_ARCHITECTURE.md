okay,# Cross-Flow Pressure-Regulated Brain Architecture

## Overview

This document describes the planned architectural transformation from a simple input→output reactive brain to a **cross-flow pressure-regulated autonomous brain** with internal dynamics and multiple I/O ports.

## Current Architecture (Reactive)

```
        INPUT (top row)
             ↓
      ┌──────────────┐
      │              │
      │  MAIN BRAIN  │
      │   (6×12)     │
      │              │
      └──────────────┘
             ↓
        OUTPUT (bottom)
```

**Characteristics:**
- Sensory input → top row → signal propagates down → motor output from bottom row
- Purely reactive: no input = no activity
- Single vertical flow with downward bias

## Target Architecture (Autonomous Cross-Flow)

```
                  SENSORY INPUT
                       ↓
        ┌──────────────────────────────┐
        │     vision slices → top      │
        └──────────────────────────────┘
                       ↓
┌──────────┐                            ┌──────────┐
│ OVERFLOW │ ←  ┌───────────────┐   ← │META-INPUT│
│  (left)  │    │               │      │ (right)  │
│          │ ←  │  MAIN BRAIN   │   ← │          │
│  drain   │    │    (6×12)     │      │  from    │
│  excess  │ ←  │               │   ← │meta-brain│
└──────────┘    └───────────────┘      └──────────┘
                       ↓
        ┌──────────────────────────────┐
        │    bottom → motor commands   │
        └──────────────────────────────┘
                  MOTOR OUTPUT
```

**Four I/O Ports:**
1. **TOP (Sensory Input)**: Vision system → brain, always active
2. **RIGHT (Meta-Brain Input)**: Internal stimulus → brain, throttled by pressure
3. **BOTTOM (Motor Output)**: Brain → movement commands, always active
4. **LEFT (Overflow Drain)**: Brain → drain excess activity, activated by pressure

**Key Innovations:**
- **Internal dynamics**: Meta-brain continuously feeds activity into system
- **Pressure regulation**: System monitors internal activity and regulates flow
- **Multiple flow patterns**: Vertical (sensory) and horizontal (meta) signals coexist
- **Self-stabilizing**: Three "valves" prevent overload and maintain equilibrium

## Pressure Regulation System

### Brain Pressure Calculation

```fortran
! Activity = sum of all trinary cell values (0, 1, or 2)
! Max possible activity = rows × cols × 2 = 6 × 12 × 2 = 144
! Pressure = activity / max_activity  (range: 0.0 to 1.0)

function calculate_brain_pressure() result(pressure)
  real :: pressure
  integer :: activity, max_activity
  
  activity = 0
  do i = 1, rows
    do j = 1, cols
      activity = activity + brain(i, j)%get()
    end do
  end do
  
  max_activity = rows * cols * 2
  pressure = real(activity) / real(max_activity)
end function
```

### Three Valves

**Valve 1: Meta-Brain Input Throttle (Right Side)**
- **Purpose**: Reduce incoming stimulus when brain is saturated
- **Thresholds**:
  - Pressure < 0.3: Full flow (100% of meta-brain output applied)
  - Pressure 0.3-0.6: Linear throttle (100% → 0%)
  - Pressure > 0.6: Cut off (0% flow)
- **Effect**: Prevents runaway activity accumulation

**Valve 2: Overflow Drain (Left Side)**
- **Purpose**: Emergency pressure release
- **Thresholds**:
  - Pressure < 0.5: Closed (no drain)
  - Pressure 0.5-0.7: Open (passive capture from left column)
  - Pressure > 0.7: Critical drain (active vacuum effect)
- **Effect**: Creates alternative exit path when output saturated

**Valve 3: Dynamic Gradient Steepness**
- **Purpose**: Accelerate flow-through when pressure builds
- **Mechanism**: Scale directional biases proportionally to pressure
  - Vertical bias multiplier: `1.0 + pressure × 1.0` (range: 1.0 → 2.0)
  - Horizontal bias multiplier: `1.0 + pressure × 0.5` (range: 1.0 → 1.5)
- **Effect**: Higher pressure = steeper gradients = faster drain to outputs

## Implementation Phases

### Phase 1: Multi-Directional I/O Infrastructure

**File**: `src/modules/brain_module.f90`

**1.1 New Module Variables**
```fortran
! Side I/O arrays (rows tall, for left/right columns)
type(trinary), dimension(:), allocatable :: meta_inputter      ! Right side input
type(trinary), dimension(:), allocatable :: overflow_outputter ! Left side output
type(trinary), dimension(:), allocatable :: backup_overflow    ! Previous overflow state

! Side I/O position parameters (like input_offset/output_offset for top/bottom)
integer :: meta_input_offset   ! Starting row for meta-brain input on right column
integer :: meta_input_length   ! Number of rows for meta input
integer :: overflow_offset     ! Starting row for overflow on left column
integer :: overflow_length     ! Number of rows for overflow
```

**1.2 Allocation**
```fortran
! In initialize_brain() or similar:
allocate(meta_inputter(rows))
allocate(overflow_outputter(rows))
allocate(backup_overflow(rows))

! Initialize to low state
do i = 1, rows
  call meta_inputter(i)%set(low)
  call overflow_outputter(i)%set(low)
  call backup_overflow(i)%set(low)
end do
```

**1.3 Side I/O Subroutines**
```fortran
subroutine apply_meta_input_to_brain(meta_inputter, brain, incoming_direction, &
                                      meta_input_offset, rows, cols)
  ! Apply meta-brain output to rightmost column at variable position
  type(trinary), allocatable :: meta_inputter(:), brain(:,:)
  integer, allocatable :: incoming_direction(:,:,:)
  integer, intent(in) :: meta_input_offset, rows, cols
  integer :: i, brain_row
  
  do i = 1, size(meta_inputter)
    brain_row = meta_input_offset + i - 1
    if (brain_row >= 1 .and. brain_row <= rows) then
      if (meta_inputter(i)%value /= low) then
        brain(brain_row, cols)%value = meta_inputter(i)%value
        incoming_direction(brain_row, cols, 1) = 8  ! Direction 8 = West (going left)
      end if
    end if
  end do
end subroutine

subroutine copy_overflow_from_brain_left_column(brain, overflow_outputter, &
                                                  overflow_offset, rows)
  ! Capture overflow from leftmost column at variable position (like output vector)
  type(trinary), allocatable :: brain(:,:), overflow_outputter(:)
  integer, intent(in) :: overflow_offset, rows
  integer :: i, brain_row
  
  do i = 1, size(overflow_outputter)
    brain_row = overflow_offset + i - 1
    if (brain_row >= 1 .and. brain_row <= rows) then
      if (brain(brain_row, 1)%value > low) then
        overflow_outputter(i)%value = brain(brain_row, 1)%value
      end if
    end if
  end do
end subroutine
```

### Phase 2: Direction System Updates

**File**: `src/modules/brain_module.f90`

**2.1 Directional Bias Enhancements**

Current direction indices and biases:
```fortran
! directions(8, 2) = [
!   [-1, -1],  ! 1 = NW (up-left)
!   [-1,  0],  ! 2 = N  (up)
!   [-1,  1],  ! 3 = NE (up-right)
!   [ 0,  1],  ! 4 = E  (right)
!   [ 1,  1],  ! 5 = SE (down-right)
!   [ 1,  0],  ! 6 = S  (down)
!   [ 1, -1],  ! 7 = SW (down-left)
!   [ 0, -1]   ! 8 = W  (left)
! ]

! Current vertical bias (for sensory signals from top):
real, dimension(8) :: vertical_bias = [0.5, 0.5, 0.5, 1.0, 1.8, 1.5, 1.5, 1.0]
!                                       NW   N    NE   E    SE   S    SW   W
!                                       ↑ upward low ↑      ↑ downward high ↑

! New horizontal bias (for meta signals from right):
real, dimension(8) :: horizontal_bias = [1.5, 1.0, 0.5, 0.5, 0.5, 1.0, 1.5, 1.8]
!                                         NW   N    NE   E    SE   S    SW   W
!                                         ↑ leftward high ↑    ↑ rightward low ↑
```

**2.2 Context-Dependent Bias Selection**

Modify `update_brain_state_based_on_synapses()`:
```fortran
! Determine which bias array to use based on signal origin
real, dimension(8) :: active_bias

! Check incoming_direction to classify signal source
if (incoming_dir == 6 .or. incoming_dir == 2) then
  ! Coming from North/South = vertical flow signal
  active_bias = vertical_bias
else if (incoming_dir == 4 .or. incoming_dir == 8) then
  ! Coming from East/West = horizontal flow signal
  active_bias = horizontal_bias
else
  ! Diagonal incoming = blend biases or use vertical as default
  active_bias = vertical_bias
end if

! For HIGH cells with two incoming directions: blend if they differ
if (current_state == high .and. incoming_dir2 /= 0) then
  ! Could average biases, or use dominant direction, TBD
end if
```

### Phase 3: Pressure-Regulated Flow Control

**File**: `src/modules/brain_module.f90`

**3.1 Brain Pressure Calculation**
```fortran
function calculate_brain_activity() result(activity)
  integer :: activity, i, j
  activity = 0
  do i = 1, rows
    do j = 1, cols
      activity = activity + brain(i, j)%get()
    end do
  end do
end function

function calculate_brain_pressure() result(pressure)
  real :: pressure
  integer :: activity, max_activity
  activity = calculate_brain_activity()
  max_activity = rows * cols * 2  ! All cells at HIGH = 2
  pressure = real(activity) / real(max_activity)
end function
```

**3.2 Valve 1: Throttled Meta-Input**
```fortran
subroutine apply_throttled_meta_input(pressure)
  real, intent(in) :: pressure
  real :: flow_rate, rand_val
  integer :: i
  
  ! Calculate flow rate based on pressure
  if (pressure < 0.3) then
    flow_rate = 1.0  ! Full flow
  else if (pressure < 0.6) then
    flow_rate = 1.0 - (pressure - 0.3) / 0.3  ! Linear throttle
  else
    flow_rate = 0.0  ! Cut off
  end if
  
  ! Apply meta_inputter with probability = flow_rate
  do i = 1, rows
    if (meta_inputter(i)%get() > 0) then
      call random_number(rand_val)
      if (rand_val < flow_rate) then
        call brain(i, cols)%set(meta_inputter(i)%get())
        incoming_direction(i, cols, 1) = 4  ! From East
      end if
    end if
  end do
end subroutine
```

**3.3 Valve 2: Overflow Drain (Gradient-Based)**
```fortran
! This is now integrated into the bias calculation, not a separate subroutine
! The overflow "drain" works by modifying direction_bias to pull signals left

function calculate_leftward_pull(pressure) result(leftward_multiplier)
  real, intent(in) :: pressure
  real :: leftward_multiplier
  
  if (pressure < 0.5) then
    leftward_multiplier = 1.0  ! No pull - drain closed
  else if (pressure < 0.7) then
    ! Gradual increase: 1.0 → 2.0 as pressure goes 0.5 → 0.7
    leftward_multiplier = 1.0 + (pressure - 0.5) / 0.2 * 1.0
  else
    leftward_multiplier = 2.0  ! Strong pull - drain wide open
  end if
end function

! Then in update_brain_state_based_on_synapses:
! Apply leftward multiplier to directions 1 (NW), 7 (SW), 8 (W)
real :: leftward_pull
leftward_pull = calculate_leftward_pull(pressure)
scaled_bias(1) = direction_bias(1) * leftward_pull  ! NW
scaled_bias(7) = direction_bias(7) * leftward_pull  ! SW  
scaled_bias(8) = direction_bias(8) * leftward_pull  ! W

! After brain processing, copy overflow from left column
! (This is like copy_to_outputter for bottom row)
call copy_overflow_from_brain_left_column(brain, overflow_outputter, overflow_offset, rows)
```

**3.4 Valve 3: Pressure-Scaled Biases (Vertical Flow)**
```fortran
subroutine update_brain_state_based_on_synapses(pressure)
  real, intent(in) :: pressure
  real :: vertical_multiplier, horizontal_multiplier
  real, dimension(8) :: scaled_vertical_bias, scaled_horizontal_bias
  
  ! Scale gradients based on pressure
  vertical_multiplier = 1.0 + pressure * 1.0      ! 1.0 → 2.0
  horizontal_multiplier = 1.0 + pressure * 0.5    ! 1.0 → 1.5
  
  scaled_vertical_bias = vertical_bias * vertical_multiplier
  scaled_horizontal_bias = horizontal_bias * horizontal_multiplier
  
  ! Rest of propagation logic uses scaled biases...
end subroutine
```

### Phase 4: Integration into Main Loop

**File**: `src/programs/cat_mouse_learning.f90` (or new program)

**Updated Bar Processing Sequence:**
```fortran
do bar = 1, total_bars
  !============================================================
  ! 1. CALCULATE BRAIN PRESSURE
  !============================================================
  pressure = calculate_brain_pressure()
  
  !============================================================
  ! 2. APPLY SENSORY INPUT (TOP) - Always Active
  !============================================================
  ! Update vision based on cat/mouse positions
  call update_vision_input()
  call apply_input_to_brain()  ! Top row, incoming_direction = 7 (Down)
  
  !============================================================
  ! 3. APPLY META-BRAIN INPUT (RIGHT) - Throttled by Pressure
  !============================================================
  ! Set meta_inputter from meta-brain output (TBD: mapping)
  call populate_meta_inputter_from_meta_brain()
  call apply_throttled_meta_input(pressure)  ! Right column
  
  !============================================================
  ! 4. RUN BRAIN STEPS - Pressure-Adjusted Gradients
  !============================================================
  do step = 1, steps_per_bar
    call update_brain_state_based_on_synapses(pressure)
  end do
  
  !============================================================
  ! 5. EXTRACT MOTOR OUTPUT (BOTTOM) - Always Active
  !============================================================
  call extract_output_from_brain()  ! Bottom row
  call move_cat_based_on_output()
  
  !============================================================
  ! 6. CAPTURE OVERFLOW (LEFT) - After Gradient Pulls Left
  !============================================================
  ! Note: The "drain" happens naturally through pressure-adjusted leftward bias
  ! We just capture what reaches the left column (like outputter captures bottom)
  call copy_overflow_from_brain_left_column(brain, overflow_outputter, overflow_offset, rows)
  
  !============================================================
  ! 7. DECAY AND LEARNING
  !============================================================
  call apply_decay()
  if (cat_moved_towards_mouse) then
    call apply_adaptive_reinforcement()
  else if (cat_moved_away) then
    call apply_adaptive_punishment()
  end if
end do
```

## Design Decisions To Resolve

**STATUS UPDATE (2026-01-19)**: All four critical design questions have been resolved:
1. ✅ **Overflow logging**: Log all overflow data for future analysis
2. ✅ **Variable position mapping**: Side I/O uses offset/length parameters like top/bottom I/O
3. ✅ **Cross-flow conflicts**: Already solved by existing 4D synapse system with dual incoming direction tracking
4. ✅ **Overflow drain mechanism**: Gradient-based flow using pressure-adjusted leftward bias (matches output vector behavior)

See individual sections below for detailed resolutions.

---

### 1. Overflow Interpretation
**Question**: What do we do with overflow data?
**Options**:
- A) Discard (just a pressure release)
- B) Log for analysis (detect saturation patterns)
- C) Feed back to meta-brain (let it learn when overflow occurs)

**RESOLVED**: **B) Log overflow data**. We don't know what it will be used for yet, but preserve it for analysis. May implement C later for meta-brain feedback.

### 2. Meta-Brain Output Mapping
**Question**: Meta-brain currently outputs 1D array (7 elements). How map to right column (6 elements)?
**Options**:
- A) Drop last element (use first 6)
- B) Average elements (7 → 6 with weighted blending)
- C) Positional encoding (different ranges affect different rows)

**RESOLVED**: **Use variable position mapping like existing I/O**:
- **Top row input**: `inputter` array maps to columns `input_offset` to `input_offset + input_length - 1`
- **Bottom row output**: `outputter` array maps to columns `output_offset` to `output_offset + output_length - 1`
- **Right column meta-input**: `meta_inputter` array maps to rows `meta_input_offset` to `meta_input_offset + meta_input_length - 1`
- **Left column overflow**: `overflow_outputter` array maps to rows `overflow_offset` to `overflow_offset + overflow_length - 1`

This provides flexibility in where side I/O connects, just like top/bottom I/O.

### 3. Cross-Flow Interaction
**Question**: When neuron receives signals from BOTH top and right simultaneously, which bias wins?

**RESOLVED**: **This is already solved by the 4D synapse system!**

The existing architecture handles this elegantly:
- `incoming_direction(row, col, 2)` tracks **up to 2 incoming directions** per neuron
- When a neuron receives signals from multiple sources, it records both directions
- For **HIGH state neurons** (two active inputs), the system **averages synapse weights** from both incoming directions:
  ```fortran
  ! HIGH state with two incoming directions - average them
  do k = 1, 8
    synapse_values(k) = (synapses(i, j, incoming_dir, k) + &
                         synapses(i, j, incoming_dir2, k)) / 2.0
  end do
  ```
- This means the neuron considers **context from both signal sources** when selecting output direction
- The 4D synapse array `synapses(row, col, incoming_dir, outgoing_dir)` was designed precisely for this!

**No additional work needed** - the system naturally handles cross-flow interactions through context-dependent routing.

### 4. Drain Behavior
**Question**: Should overflow drain just capture, or actively pull?

**RESOLVED**: **Overflow should work exactly like the output vector**:
- **Output vector** (bottom row): Neurons propagate signals INTO outputter array as they travel (not just captured after)
- **Overflow vector** (left column): Should work the same way - neurons propagate INTO overflow as they move left
- **Gradient-based flow**: Add **leftward gradient** that strengthens with pressure:
  - Pressure < 0.5: No leftward bias (drain closed)
  - Pressure 0.5-0.7: Gradual leftward bias increase (drain opening)
  - Pressure > 0.7: Strong leftward bias (drain wide open)
- **Implementation**: Modify `direction_bias` array to add leftward component based on pressure
  - Leftward directions (1=NW, 7=SW, 8=W) get boosted when pressure high
  - This creates a "pull" toward the left edge naturally through existing propagation mechanics
  - No need for special "vacuum" logic - just adjust biases and let signals flow

**This maintains consistency** with how the system already handles output and leverages existing propagation mechanics.

### 5. Directional Encoding for Side I/O
**Question**: What `incoming_direction` should we set for side inputs?

**RESOLVED**: **Use direction 8 (West) for meta-input** - indicates intended flow direction (leftward):
- When meta-brain feeds into right column: `incoming_direction = 8` (West/left)
- This tells the neuron "you received signal from the right, you should route it left"
- Matches the conceptual flow: meta-input → rightmost column → flow left through brain → overflow on left
- The 4D synapse system `synapses(row, col, incoming_dir=8, outgoing_dir)` will select appropriate leftward routing

**For overflow drain**: No direction encoding needed - it's a capture mechanism like the output vector, not an input source.

## Success Metrics

### Short-Term (Prototype Working)
- [ ] Side I/O arrays allocated and functional
- [ ] Meta-brain output successfully feeds into right column
- [ ] Overflow drain captures activity from left column
- [ ] Pressure calculation returns sensible values (0.0-1.0)
- [ ] Three valves respond to pressure changes

### Medium-Term (System Stability)
- [ ] Brain doesn't saturate (pressure stays < 0.8 most of the time)
- [ ] Brain doesn't starve (pressure stays > 0.1 most of the time)
- [ ] Sensory signals still propagate to output (vertical flow maintained)
- [ ] Meta signals create detectable horizontal patterns
- [ ] Learning system still functions (reinforcement/punishment work)

### Long-Term (Performance Goals)
- [ ] Catch rate maintained or improved vs single-brain system
- [ ] Meta-brain learns effective input regulation strategies
- [ ] Cross-flow patterns emerge (distinct from pure vertical)
- [ ] Pressure regulation prevents instability
- [ ] System demonstrates autonomous activity even without sensory input

## Testing Strategy

### Unit Tests
1. **Side I/O Mechanics**: Test `apply_meta_input_to_brain()` and `drain_overflow_from_brain()` with known states
2. **Pressure Calculation**: Test `calculate_brain_pressure()` with various brain states (empty, half-full, saturated)
3. **Valve Functions**: Test each valve with different pressure levels

### Integration Tests
1. **Cross-Flow Patterns**: Input from top only, right only, both simultaneously
2. **Pressure Response**: Monitor pressure over time with different meta-brain input rates
3. **Stability**: Run 10,000+ bars and verify no saturation/starvation

### System Tests
1. **Cat-Mouse Learning**: Compare performance vs baseline (single-brain system)
2. **Meta-Learning**: Verify meta-brain learns to regulate input effectively
3. **Visualization**: Create tools to visualize cross-flow patterns and pressure dynamics

## Files To Modify

### Core Modules
- [x] `src/modules/brain_module.f90` - Add side I/O, pressure calculation, valves
- [ ] `src/modules/brain_engine_module.f90` - Update initialization for new arrays
- [ ] `src/modules/outputter_module.f90` - Consider if overflow needs similar backup mechanism

### Main Programs
- [ ] `src/programs/cat_mouse_learning.f90` - Integrate pressure-regulated cross-flow
- [ ] Create new `src/programs/cross_flow_demo.f90` - Isolated test program

### Testing
- [ ] `src/tests/test_cross_flow.f90` - Unit tests for new functionality
- [ ] `scripts/run_cross_flow_experiment.sh` - Multi-trial testing framework

### Visualization
- [ ] `visualization/visualize_cross_flow.py` - Show 4 I/O ports and pressure
- [ ] `visualization/analyze_pressure_dynamics.py` - Plot pressure over time

### Documentation
- [x] `CROSS_FLOW_ARCHITECTURE.md` - This file
- [ ] Update `README.md` with new architecture description
- [ ] Update `BAR_STRUCTURE.md` with cross-flow timing considerations

## Timeline Estimate

- **Phase 1** (Infrastructure): 2-3 hours - Add side I/O arrays and basic subroutines
- **Phase 2** (Direction System): 1-2 hours - Implement context-dependent biases
- **Phase 3** (Pressure Regulation): 2-3 hours - Add pressure calculation and three valves
- **Phase 4** (Integration): 1-2 hours - Update main loop and test
- **Testing & Tuning**: 3-5 hours - Debug, tune thresholds, verify stability
- **Visualization**: 2-3 hours - Create tools to understand cross-flow patterns

**Total**: ~15-20 hours for complete implementation and validation

## Next Steps

1. Resolve design decisions (see section above)
2. Implement Phase 1 (infrastructure) - this is foundational for everything else
3. Create unit tests for new functionality
4. Implement Phases 2-3 (direction system and pressure regulation)
5. Integrate into main loop (Phase 4)
6. Test stability and tune thresholds
7. Create visualization tools
8. Compare performance vs baseline

---

**Last Updated**: 2026-01-19  
**Status**: Design complete, implementation pending  
**Branch**: Will create `cross-flow-architecture` branch for development
