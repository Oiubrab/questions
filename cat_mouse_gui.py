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
    """Draw a simple cat (triangle with ears)"""
    px = int(x * SCALE)
    py = int(y * SCALE)
    size = 12
    
    # Body (triangle pointing right)
    points = [
        (px - size, py - size),
        (px - size, py + size),
        (px + size, py)
    ]
    pygame.draw.polygon(screen, BLUE, points)
    pygame.draw.polygon(screen, WHITE, points, 2)
    
    # Eyes (two small circles)
    pygame.draw.circle(screen, WHITE, (px - size//2, py - size//3), 2)
    pygame.draw.circle(screen, WHITE, (px - size//2, py + size//3), 2)

def draw_mouse(x, y):
    """Draw a simple mouse (small circle with tail)"""
    px = int(x * SCALE)
    py = int(y * SCALE)
    size = 8
    
    # Body
    pygame.draw.circle(screen, RED, (px, py), size)
    pygame.draw.circle(screen, WHITE, (px, py), size, 2)
    
    # Tail (curved line)
    tail_end_x = px + size + 6
    tail_end_y = py - 4
    pygame.draw.line(screen, RED, (px + size, py), (tail_end_x, tail_end_y), 2)
    
    # Ear
    pygame.draw.circle(screen, RED, (px - 4, py - 6), 3)

def draw_vision_line(cat_x, cat_y, mouse_x, mouse_y, slice_num):
    """Draw a line from cat to mouse showing vision slice"""
    px1 = int(cat_x * SCALE)
    py1 = int(cat_y * SCALE)
    px2 = int(mouse_x * SCALE)
    py2 = int(mouse_y * SCALE)
    
    # Semi-transparent line
    color = (100, 255, 100, 100)
    pygame.draw.line(screen, GREEN, (px1, py1), (px2, py2), 1)

def draw_info(step, brain_energy, output_energy, slice_num, distance):
    """Draw info text"""
    font = pygame.font.Font(None, 24)
    
    texts = [
        f"Step: {step}",
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
            
            # Parse input line: step,mouse_x,mouse_y,cat_x,cat_y,slice,brain_energy,output_energy
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
            draw_info(step, brain_energy, output_energy, slice_num, distance)
            
            # Update display
            pygame.display.flip()
            clock.tick(FPS)
    
    except KeyboardInterrupt:
        pass
    
    pygame.quit()
    print("GUI closed.", file=sys.stderr)

if __name__ == "__main__":
    main()
