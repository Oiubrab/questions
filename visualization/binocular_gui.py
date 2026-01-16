#!/usr/bin/env python3
"""
GUI visualization for binocular cat-mouse simulation.
Reads CSV data from stdin and displays animated cat with two eyes chasing mouse.
"""

import pygame
import sys
import math
import csv

# Initialize Pygame
pygame.init()

# Constants
WINDOW_SIZE = 800
FIELD_SIZE = 100.0
SCALE = WINDOW_SIZE / FIELD_SIZE
FPS = 30

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
GRAY = (50, 50, 50)
RED = (255, 100, 100)
BLUE = (100, 150, 255)
GREEN = (100, 255, 100)
YELLOW = (255, 255, 100)
CYAN = (100, 255, 255)

# Create window
screen = pygame.display.set_mode((WINDOW_SIZE, WINDOW_SIZE))
pygame.display.set_caption("Binocular Cat & Mouse Brain Simulation")
clock = pygame.time.Clock()

def get_eye_positions(cat_x, cat_y, heading, eye_sep=5.0):
    """Calculate left and right eye positions based on cat center and heading"""
    # Eyes are perpendicular to heading direction
    perp_angle = heading + math.pi / 2
    
    left_eye_x = cat_x + (eye_sep / 2) * math.cos(perp_angle)
    left_eye_y = cat_y + (eye_sep / 2) * math.sin(perp_angle)
    
    right_eye_x = cat_x - (eye_sep / 2) * math.cos(perp_angle)
    right_eye_y = cat_y - (eye_sep / 2) * math.sin(perp_angle)
    
    return (left_eye_x, left_eye_y), (right_eye_x, right_eye_y)

def draw_vision_rays(cat_x, cat_y, heading, mouse_x, mouse_y):
    """Draw vision rays from eyes to mouse"""
    left_eye, right_eye = get_eye_positions(cat_x, cat_y, heading)
    
    # Draw rays from each eye to mouse
    left_px = int(left_eye[0] * SCALE)
    left_py = int(left_eye[1] * SCALE)
    right_px = int(right_eye[0] * SCALE)
    right_py = int(right_eye[1] * SCALE)
    mouse_px = int(mouse_x * SCALE)
    mouse_py = int(mouse_y * SCALE)
    
    # Left eye ray (cyan)
    pygame.draw.line(screen, CYAN, (left_px, left_py), (mouse_px, mouse_py), 1)
    
    # Right eye ray (yellow)
    pygame.draw.line(screen, YELLOW, (right_px, right_py), (mouse_px, mouse_py), 1)

def draw_cat_with_heading(x, y, heading):
    """Draw cat with body oriented according to heading, showing both eyes"""
    px = int(x * SCALE)
    py = int(y * SCALE)
    
    # Cat colors
    cat_body = (100, 150, 255)
    cat_dark = (60, 100, 200)
    
    # Calculate front direction
    front_x = px + 15 * math.cos(heading)
    front_y = py + 15 * math.sin(heading)
    
    # Main body (ellipse oriented to heading)
    body_radius = 12
    pygame.draw.circle(screen, cat_body, (px, py), body_radius)
    pygame.draw.circle(screen, cat_dark, (px, py), body_radius, 2)
    
    # Head in front
    head_radius = 9
    head_x = int(px + 8 * math.cos(heading))
    head_y = int(py + 8 * math.sin(heading))
    pygame.draw.circle(screen, cat_body, (head_x, head_y), head_radius)
    pygame.draw.circle(screen, cat_dark, (head_x, head_y), head_radius, 2)
    
    # Calculate eye positions (perpendicular to heading)
    left_eye, right_eye = get_eye_positions(x, y, heading, eye_sep=5.0)
    
    # Draw eyes on head
    perp_angle = heading + math.pi / 2
    left_eye_x = int(head_x + 3 * math.cos(perp_angle))
    left_eye_y = int(head_y + 3 * math.sin(perp_angle))
    right_eye_x = int(head_x - 3 * math.cos(perp_angle))
    right_eye_y = int(head_y - 3 * math.sin(perp_angle))
    
    # Eyes (white with black pupils)
    pygame.draw.circle(screen, WHITE, (left_eye_x, left_eye_y), 3)
    pygame.draw.circle(screen, BLACK, (left_eye_x, left_eye_y), 1)
    pygame.draw.circle(screen, WHITE, (right_eye_x, right_eye_y), 3)
    pygame.draw.circle(screen, BLACK, (right_eye_x, right_eye_y), 1)
    
    # Nose at front
    nose_x = int(head_x + head_radius * 0.8 * math.cos(heading))
    nose_y = int(head_y + head_radius * 0.8 * math.sin(heading))
    pygame.draw.circle(screen, (255, 150, 150), (nose_x, nose_y), 2)
    
    # Ears (triangles perpendicular to heading)
    ear_offset = 5
    ear_height = 8
    ear_base_x = head_x - int(3 * math.cos(heading))
    ear_base_y = head_y - int(3 * math.sin(heading))
    
    # Left ear
    left_ear_base_x = int(ear_base_x + ear_offset * math.cos(perp_angle))
    left_ear_base_y = int(ear_base_y + ear_offset * math.sin(perp_angle))
    left_ear_tip_x = int(left_ear_base_x + ear_height * math.cos(heading - math.pi/2))
    left_ear_tip_y = int(left_ear_base_y + ear_height * math.sin(heading - math.pi/2))
    left_ear = [
        (left_ear_base_x - 2, left_ear_base_y - 2),
        (left_ear_tip_x, left_ear_tip_y),
        (left_ear_base_x + 2, left_ear_base_y + 2)
    ]
    pygame.draw.polygon(screen, cat_body, left_ear)
    pygame.draw.polygon(screen, cat_dark, left_ear, 1)
    
    # Right ear
    right_ear_base_x = int(ear_base_x - ear_offset * math.cos(perp_angle))
    right_ear_base_y = int(ear_base_y - ear_offset * math.sin(perp_angle))
    right_ear_tip_x = int(right_ear_base_x + ear_height * math.cos(heading - math.pi/2))
    right_ear_tip_y = int(right_ear_base_y + ear_height * math.sin(heading - math.pi/2))
    right_ear = [
        (right_ear_base_x - 2, right_ear_base_y - 2),
        (right_ear_tip_x, right_ear_tip_y),
        (right_ear_base_x + 2, right_ear_base_y + 2)
    ]
    pygame.draw.polygon(screen, cat_body, right_ear)
    pygame.draw.polygon(screen, cat_dark, right_ear, 1)
    
    # Tail (curved line from back)
    tail_base_x = int(px - body_radius * 0.8 * math.cos(heading))
    tail_base_y = int(py - body_radius * 0.8 * math.sin(heading))
    tail_curve_angle = heading + math.pi + math.pi/4
    tail_mid_x = int(tail_base_x + 8 * math.cos(tail_curve_angle))
    tail_mid_y = int(tail_base_y + 8 * math.sin(tail_curve_angle))
    tail_end_x = int(tail_mid_x + 6 * math.cos(tail_curve_angle + math.pi/6))
    tail_end_y = int(tail_mid_y + 6 * math.sin(tail_curve_angle + math.pi/6))
    
    pygame.draw.lines(screen, cat_dark, False, 
                     [(tail_base_x, tail_base_y), (tail_mid_x, tail_mid_y), (tail_end_x, tail_end_y)], 3)

def draw_mouse(x, y):
    """Draw a recognizable mouse"""
    px = int(x * SCALE)
    py = int(y * SCALE)
    
    # Mouse colors
    mouse_body = (200, 200, 200)
    mouse_dark = (100, 100, 100)
    
    # Body (small circle)
    body_radius = 6
    pygame.draw.circle(screen, mouse_body, (px, py), body_radius)
    pygame.draw.circle(screen, mouse_dark, (px, py), body_radius, 1)
    
    # Head (smaller circle in front)
    head_radius = 4
    head_x = px + 5
    pygame.draw.circle(screen, mouse_body, (head_x, py), head_radius)
    pygame.draw.circle(screen, mouse_dark, (head_x, py), head_radius, 1)
    
    # Big round ears
    ear_radius = 4
    pygame.draw.circle(screen, mouse_body, (head_x - 2, py - 5), ear_radius)
    pygame.draw.circle(screen, mouse_dark, (head_x - 2, py - 5), ear_radius, 1)
    pygame.draw.circle(screen, mouse_body, (head_x - 2, py + 5), ear_radius)
    pygame.draw.circle(screen, mouse_dark, (head_x - 2, py + 5), ear_radius, 1)
    
    # Eyes (tiny black dots)
    pygame.draw.circle(screen, BLACK, (head_x + 2, py - 1), 1)
    pygame.draw.circle(screen, BLACK, (head_x + 2, py + 1), 1)
    
    # Nose (pink dot)
    pygame.draw.circle(screen, (255, 150, 150), (head_x + head_radius, py), 2)
    
    # Tail (thin line)
    tail_start_x = px - body_radius
    tail_end_x = px - body_radius - 12
    pygame.draw.line(screen, mouse_dark, (tail_start_x, py), (tail_end_x, py + 3), 2)

def draw_info_panel(bar, catches, brain_energy, output_action):
    """Draw info panel on the side"""
    font = pygame.font.Font(None, 24)
    
    info_lines = [
        f"Bar: {bar}",
        f"Catches: {catches}",
        f"Brain Energy: {brain_energy}",
        f"Output: {output_action}"
    ]
    
    y_offset = 10
    for line in info_lines:
        text = font.render(line, True, WHITE)
        screen.blit(text, (10, y_offset))
        y_offset += 25

# Read CSV header
csv_reader = csv.reader(sys.stdin)
header = next(csv_reader)

# Parse header to find column indices
col_indices = {name: idx for idx, name in enumerate(header)}

print("Starting binocular visualization...", file=sys.stderr)
print(f"CSV columns: {header}", file=sys.stderr)

running = True
paused = False

for row in csv_reader:
    if not running:
        break
    
    # Handle events
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_ESCAPE or event.key == pygame.K_q:
                running = False
            elif event.key == pygame.K_SPACE:
                paused = not paused
    
    if paused:
        clock.tick(10)
        continue
    
    # Parse row data
    try:
        bar = int(row[col_indices['bar']])
        cat_x = float(row[col_indices['cat_x']])
        cat_y = float(row[col_indices['cat_y']])
        cat_heading = float(row[col_indices['cat_heading']])
        mouse_x = float(row[col_indices['mouse_x']])
        mouse_y = float(row[col_indices['mouse_y']])
        brain_energy = int(row[col_indices['brain_energy']])
        output_action = int(row[col_indices['output_action']])
        catches = int(row[col_indices['catches']])
    except (KeyError, ValueError, IndexError) as e:
        print(f"Error parsing row: {e}", file=sys.stderr)
        continue
    
    # Clear screen
    screen.fill(GRAY)
    
    # Draw grid
    for i in range(0, WINDOW_SIZE, int(10 * SCALE)):
        pygame.draw.line(screen, (70, 70, 70), (i, 0), (i, WINDOW_SIZE), 1)
        pygame.draw.line(screen, (70, 70, 70), (0, i), (WINDOW_SIZE, i), 1)
    
    # Draw vision rays
    draw_vision_rays(cat_x, cat_y, cat_heading, mouse_x, mouse_y)
    
    # Draw mouse
    draw_mouse(mouse_x, mouse_y)
    
    # Draw cat with heading
    draw_cat_with_heading(cat_x, cat_y, cat_heading)
    
    # Draw info panel
    draw_info_panel(bar, catches, brain_energy, output_action)
    
    # Update display
    pygame.display.flip()
    clock.tick(FPS)

pygame.quit()
print("Visualization complete", file=sys.stderr)
