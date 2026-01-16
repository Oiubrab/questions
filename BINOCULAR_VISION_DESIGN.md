# Binocular Vision Architecture Design

## Overview
This document outlines the architecture for adding binocular (two-eye) vision to enable depth perception and strategic decision-making with two mice.

## New Parameters

### Brain Dimensions
- **Rows**: 10 (expanded from 6)
- **Columns**: 18 (expanded from 12)
- **Left eye input**: Columns 1-9 (input_offset_left = 1)
- **Right eye input**: Columns 10-18 (input_offset_right = 10)
- **Output**: 8 directions (columns 6-13, centered)

### Cat Body Model
```
                 Heading Direction (θ)
                        ↑
           Left Eye ●───┼───● Right Eye
                        │
                    [Center]
                        │
                   (Body Radius)
```

**Properties:**
- `cat_pos`: Center position (x, y)
- `cat_heading`: Direction cat is facing (0-360°, 0° = right/east)
- `eye_separation`: Distance between eyes (e.g., 5 units)
- `catch_radius`: Body zone for catching (e.g., 8 units)

### Eye Positions (calculated from cat center + heading)
```fortran
! Left eye is 90° counterclockwise from heading
left_eye_x = cat_x + (eye_separation/2) * cos(heading + 90°)
left_eye_y = cat_y + (eye_separation/2) * sin(heading + 90°)

! Right eye is 90° clockwise from heading
right_eye_x = cat_x + (eye_separation/2) * cos(heading - 90°)
right_eye_y = cat_y + (eye_separation/2) * sin(heading - 90°)
```

## Vision Coverage

### Each Eye: 9 Slices × 20° = 180° Coverage

**Left Eye** (relative to cat heading):
- Covers: heading - 135° to heading + 45° (left-biased, front-left arc)
- Slice 1: heading + 45° to heading + 25° (far right of left eye view)
- Slice 5: heading - 45° to heading - 65° (front of left eye view)  
- Slice 9: heading - 135° to heading - 155° (far left of left eye view)

**Right Eye** (relative to cat heading):
- Covers: heading - 45° to heading + 135° (right-biased, front-right arc)
- Slice 1: heading - 45° to heading - 25° (far left of right eye view)
- Slice 5: heading + 45° to heading + 65° (front of right eye view)
- Slice 9: heading + 135° to heading + 155° (far right of right eye view)

### Overlap & Parallax Zone
```
              Heading (0°)
                  ↑
         -45°    |    +45°
           ╲     |     ╱
      LEFT  ╲    |    ╱  RIGHT
       EYE   ╲===|===╱   EYE
        ↖     ╲  |  ╱     ↗
              ╲ | ╱
          OVERLAP ZONE
         (90° binocular)
               ═══
           PARALLAX
```

**In overlap zone**: Same mouse appears in DIFFERENT slices for each eye
- Left eye sees mouse at slice L
- Right eye sees mouse at slice R  
- **Parallax = |L - R|** → larger = closer

## Trinary Intensity Encoding

For each eye slice:
- **LOW (0)**: No mouse visible in this slice
- **MEDIUM (1)**: Mouse present, distance > close_threshold (e.g., 20 units)
- **HIGH (2)**: Mouse very close, distance ≤ close_threshold

This provides both:
1. **Angular position**: Which slice(s) contain mouse
2. **Proximity cue**: HIGH indicates close, requiring urgent attention

## Brain Input Mapping

```
Brain Top Row (18 columns):
 Col:  1  2  3  4  5  6  7  8  9 | 10 11 12 13 14 15 16 17 18
      ─────────────────────────────────────────────────────────
      [  LEFT EYE (9 slices)    ] | [  RIGHT EYE (9 slices)  ]
      
 Slice: L1 L2 L3 L4 L5 L6 L7 L8 L9  R1 R2 R3 R4 R5 R6 R7 R8 R9
 Angle: +45°→→→→→→→→→→→→→→→-135°  -45°→→→→→→→→→→→→→→→+135°
```

## Output Mapping

8-direction output remains centered:
```
Columns:        6  7  8  9 10 11 12 13
Direction:     UL  U UR  L  R DL  D DR
```

## Two Mice System

### Mouse Identities
- `mouse1_pos`: First mouse position
- `mouse2_pos`: Second mouse position

Both mice move independently (random walk every N bars).

### Vision Processing
Each eye detects **any/all** mice in its slices:
- If mouse1 OR mouse2 is in a slice → activate that slice
- Intensity based on **closest** mouse in that slice

### Catch Mechanics
- Any mouse within `catch_radius` of cat center → caught
- On catch: Respawn that mouse at random distant location
- Continue hunting other mouse

## Heading/Orientation Updates

Cat heading changes based on movement:
- When cat moves, new heading = direction of movement
- This rotates where the eyes "look"
- Creates natural turn-then-move behavior

## Module Changes Required

### 1. vision_simulation_module.f90
- Add `calculate_eye_positions(cat_pos, heading, separation)` → left_eye, right_eye
- Add `update_binocular_vision(left_input, right_input, left_eye, right_eye, mice, num_slices, heading, close_threshold)`
- Each eye gets separate slice calculation with distance-based intensity

### 2. inputter_module.f90  
- Support two input arrays (left_inputter, right_inputter)
- Or single 18-element inputter with offset logic

### 3. brain_module.f90
- Increase default dimensions or make configurable
- Two input regions (left cols 1-9, right cols 10-18)
- Single output region (centered)

### 4. cat_mouse_learning.f90
- Add cat heading tracking
- Add second mouse
- Update vision calls for binocular system
- Radius-based catch detection
- Update CSV output format for dual mice
- Update reward system for multi-target

### 5. cat_mouse_gui.py
- Draw cat with body (not point)
- Draw two mice
- Show eye positions and vision rays
- Display heading direction

## Implementation Order

1. **Phase 1**: Update vision_simulation_module with binocular vision
2. **Phase 2**: Expand brain/inputter for 18-column input
3. **Phase 3**: Add second mouse and catch radius
4. **Phase 4**: Add heading tracking and eye position calculation
5. **Phase 5**: Update cat_mouse_learning main loop
6. **Phase 6**: Update visualization
7. **Phase 7**: Test and tune parameters

## Constants Summary

```fortran
! Brain dimensions
integer, parameter :: rows = 10
integer, parameter :: cols = 18
integer, parameter :: num_slices_per_eye = 9
integer, parameter :: total_vision_inputs = 18

! Input offsets
integer, parameter :: input_offset_left = 1
integer, parameter :: input_offset_right = 10

! Output
integer, parameter :: output_length = 8
integer, parameter :: output_offset = 6  ! Centered

! Cat body
real, parameter :: eye_separation = 5.0
real, parameter :: catch_radius = 8.0
real, parameter :: close_threshold = 20.0  ! Distance for HIGH intensity

! Vision
real, parameter :: eye_fov = 180.0  ! Degrees per eye
real, parameter :: slice_width = 20.0  ! Degrees per slice
```
