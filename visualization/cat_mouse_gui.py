#!/usr/bin/env python3
"""
Simple GUI visualization for cat-mouse simulation.
Reads state from stdin and displays animated cat chasing mouse.
"""

import pygame
import sys
import math

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

# Create window
screen = pygame.display.set_mode((WINDOW_SIZE, WINDOW_SIZE))
pygame.display.set_caption("Cat & Mouse Brain Simulation")
clock = pygame.time.Clock()

def draw_cat(x, y):
    """Draw a recognizable cat - simple and iconic"""
    px = int(x * SCALE)
    py = int(y * SCALE)
    
    # Cat colors
    cat_body = (100, 150, 255)  # Blue
    cat_dark = (60, 100, 200)   # Dark blue
    
    # Main body (one large circle)
    body_radius = 12
    pygame.draw.circle(screen, cat_body, (px, py), body_radius)
    pygame.draw.circle(screen, cat_dark, (px, py), body_radius, 2)
    
    # Head overlapping body (circle in front)
    head_radius = 9
    head_x = px + 6
    pygame.draw.circle(screen, cat_body, (head_x, py - 2), head_radius)
    pygame.draw.circle(screen, cat_dark, (head_x, py - 2), head_radius, 2)
    
    # Pointy ears (clear triangles)
    ear_height = 8
    # Left ear
    left_ear = [
        (head_x - 5, py - 2 - head_radius + 3),
        (head_x - 2, py - 2 - head_radius - ear_height),
        (head_x + 1, py - 2 - head_radius + 3)
    ]
    pygame.draw.polygon(screen, cat_body, left_ear)
    pygame.draw.polygon(screen, cat_dark, left_ear, 2)
    
    # Right ear
    right_ear = [
        (head_x + 2, py - 2 - head_radius + 3),
        (head_x + 5, py - 2 - head_radius - ear_height),
        (head_x + 8, py - 2 - head_radius + 3)
    ]
    pygame.draw.polygon(screen, cat_body, right_ear)
    pygame.draw.polygon(screen, cat_dark, right_ear, 2)
    
    # Eyes (simple white circles with black pupils)
    eye_y = py - 4
    pygame.draw.circle(screen, WHITE, (head_x + 1, eye_y), 3)
    pygame.draw.circle(screen, BLACK, (head_x + 2, eye_y), 1)
    
    # Nose (small pink triangle)
    nose_tip = head_x + head_radius - 2
    nose = [
        (nose_tip - 2, py - 1),
        (nose_tip - 2, py + 1),
        (nose_tip + 1, py)
    ]
    pygame.draw.polygon(screen, (255, 150, 150), nose)
    
    # Whiskers (3 lines each side)
    whisker_color = WHITE
    for i, offset in enumerate([-3, 0, 3]):
        pygame.draw.line(screen, whisker_color, 
                        (head_x + 4, py + offset), 
                        (head_x + 13, py + offset - 2), 1)
    
    # Tail (upright and curved)
    tail_base_x = px - body_radius + 3
    tail_base_y = py + 3
    # Draw tail as thick curved line pointing up and back
    tail_points = [
        (tail_base_x, tail_base_y),
        (tail_base_x - 8, tail_base_y + 6),
        (tail_base_x - 10, tail_base_y + 12),
        (tail_base_x - 8, tail_base_y + 16)
    ]
    for i in range(len(tail_points) - 1):
        pygame.draw.line(screen, cat_dark, tail_points[i], tail_points[i+1], 4)
        pygame.draw.line(screen, cat_body, tail_points[i], tail_points[i+1], 2)

def draw_mouse(x, y):
    """Draw a recognizable mouse - round body with big ears and long tail"""
    px = int(x * SCALE)
    py = int(y * SCALE)
    
    # Mouse colors
    mouse_body = (220, 90, 90)     # Red/brown
    mouse_dark = (160, 50, 50)     # Dark red
    mouse_light = (255, 180, 180)  # Pink
    
    # Body (round circle - mice are round!)
    body_radius = 8
    pygame.draw.circle(screen, mouse_body, (px, py), body_radius)
    pygame.draw.circle(screen, mouse_dark, (px, py), body_radius, 2)
    
    # Head (smaller circle overlapping)
    head_radius = 6
    head_x = px + 5
    pygame.draw.circle(screen, mouse_body, (head_x, py), head_radius)
    pygame.draw.circle(screen, mouse_dark, (head_x, py), head_radius, 1)
    
    # BIG round ears (key mouse feature!)
    ear_radius = 5
    # Left ear
    ear_left_x = head_x - 3
    ear_left_y = py - head_radius - 2
    pygame.draw.circle(screen, mouse_body, (ear_left_x, ear_left_y), ear_radius)
    pygame.draw.circle(screen, mouse_dark, (ear_left_x, ear_left_y), ear_radius, 2)
    pygame.draw.circle(screen, mouse_light, (ear_left_x, ear_left_y), 3)  # Inner ear
    
    # Right ear
    ear_right_x = head_x + 3
    ear_right_y = py - head_radius - 2
    pygame.draw.circle(screen, mouse_body, (ear_right_x, ear_right_y), ear_radius)
    pygame.draw.circle(screen, mouse_dark, (ear_right_x, ear_right_y), ear_radius, 2)
    pygame.draw.circle(screen, mouse_light, (ear_right_x, ear_right_y), 3)  # Inner ear
    
    # Tiny eyes (beady mouse eyes)
    pygame.draw.circle(screen, BLACK, (head_x + 3, py - 1), 2)
    pygame.draw.circle(screen, BLACK, (head_x + 3, py + 1), 2)
    
    # Pink nose at tip
    pygame.draw.circle(screen, mouse_light, (head_x + head_radius, py), 2)
    
    # Long thin tail (classic mouse tail - key feature!)
    tail_start_x = px - body_radius
    tail_start_y = py + 2
    tail_length = 20
    
    # Draw as S-curve
    tail_segments = 15
    for i in range(tail_segments):
        t = i / tail_segments
        # S-curve formula
        tail_x = tail_start_x - t * tail_length
        tail_y = tail_start_y + math.sin(t * math.pi * 2) * 4
        
        next_t = (i + 1) / tail_segments
        next_x = tail_start_x - next_t * tail_length
        next_y = tail_start_y + math.sin(next_t * math.pi * 2) * 4
        
        # Tail gets thinner towards tip
        thickness = max(1, 3 - int(t * 2))
        pygame.draw.line(screen, mouse_dark, 
                        (int(tail_x), int(tail_y)), 
                        (int(next_x), int(next_y)), thickness)

def draw_vision_line(cat_x, cat_y, mouse_x, mouse_y, slice_num):
    """Draw a line from cat to mouse showing vision slice"""
    px1 = int(cat_x * SCALE)
    py1 = int(cat_y * SCALE)
    px2 = int(mouse_x * SCALE)
    py2 = int(mouse_y * SCALE)
    
    # Semi-transparent line
    color = (100, 255, 100, 100)
    pygame.draw.line(screen, GREEN, (px1, py1), (px2, py2), 1)

def draw_info(step, brain_energy, output_energy, slice_num, distance, catches=0):
    """Draw info text"""
    font = pygame.font.Font(None, 24)
    
    texts = [
        f"Step: {step}",
        f"Catches: {catches}",
        f"Vision Slice: {slice_num}",
        f"Brain Energy: {brain_energy}",
        f"Output Energy: {output_energy}",
        f"Distance: {distance:.1f}"
    ]
    
    y_offset = 10
    for text in texts:
        surface = font.render(text, True, WHITE)
        screen.blit(surface, (10, y_offset))
        y_offset += 25

def main():
    running = True
    
    # Default state
    cat_x, cat_y = 50, 50
    mouse_x, mouse_y = 25, 25
    step = 0
    brain_energy = 0
    output_energy = 0
    slice_num = 0
    catches = 0
    
    print("GUI started. Waiting for input...", file=sys.stderr)
    
    try:
        for line in sys.stdin:
            # Handle pygame events
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                    break
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE or event.key == pygame.K_q:
                        running = False
                        break
            
            if not running:
                break
            
            # Parse input line: step,mouse_x,mouse_y,cat_x,cat_y,slice,brain_energy,output_energy,output_action,move_dist,catches
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            
            try:
                parts = line.split(',')
                step = int(parts[0])
                mouse_x = float(parts[1])
                mouse_y = float(parts[2])
                cat_x = float(parts[3])
                cat_y = float(parts[4])
                slice_num = int(parts[5])
                brain_energy = int(parts[6])
                output_energy = int(parts[7])
                # New: catches counter (last column)
                if len(parts) > 10:
                    catches = int(parts[10])
            except (ValueError, IndexError):
                continue
            
            # Calculate distance
            dx = mouse_x - cat_x
            dy = mouse_y - cat_y
            # Handle toroidal wrapping
            if abs(dx) > FIELD_SIZE / 2:
                dx = dx - FIELD_SIZE if dx > 0 else dx + FIELD_SIZE
            if abs(dy) > FIELD_SIZE / 2:
                dy = dy - FIELD_SIZE if dy > 0 else dy + FIELD_SIZE
            distance = math.sqrt(dx*dx + dy*dy)
            
            # Draw everything
            screen.fill(BLACK)
            
            # Draw grid
            for i in range(0, WINDOW_SIZE, int(10 * SCALE)):
                pygame.draw.line(screen, GRAY, (i, 0), (i, WINDOW_SIZE), 1)
                pygame.draw.line(screen, GRAY, (0, i), (WINDOW_SIZE, i), 1)
            
            # Draw vision line
            if brain_energy > 0:
                draw_vision_line(cat_x, cat_y, mouse_x, mouse_y, slice_num)
            
            # Draw characters
            draw_mouse(mouse_x, mouse_y)
            draw_cat(cat_x, cat_y)
            
            # Draw info
            draw_info(step, brain_energy, output_energy, slice_num, distance, catches)
            
            # Update display
            pygame.display.flip()
            clock.tick(FPS)
    
    except KeyboardInterrupt:
        pass
    
    pygame.quit()
    print("GUI closed.", file=sys.stderr)

if __name__ == "__main__":
    main()
