#!/usr/bin/env python3
"""
Evolved Brain GUI Visualization
Displays the behavior of the best evolved brain from the evolutionary algorithm
"""

import pygame
import sys
import subprocess
import math
import csv
import io
from threading import Thread
from queue import Queue, Empty

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 1000
WINDOW_HEIGHT = 700
FIELD_SIZE = 100.0
SCALE = 5.0  # Pixels per unit
FIELD_OFFSET_X = 50
FIELD_OFFSET_Y = 50

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
BLUE = (0, 0, 255)
GREEN = (0, 255, 0)
YELLOW = (255, 255, 0)
GRAY = (128, 128, 128)
LIGHT_GRAY = (200, 200, 200)
DARK_BLUE = (0, 0, 128)

class EvolvedBrainViewer:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Evolved Brain - Cat Chasing Mouse")
        self.clock = pygame.time.Clock()
        
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        
        # Simulation state
        self.cat_pos = [50.0, 50.0]
        self.mouse_pos = [50.0, 50.0]
        self.distance = 0.0
        self.vision_slice = 1
        self.output_action = 0
        self.step = 0
        
        # History for tracking
        self.distance_history = []
        self.cat_trail = []
        self.mouse_trail = []
        
        # Data queue from subprocess
        self.data_queue = Queue()
        self.process = None
        self.reader_thread = None
        
    def world_to_screen(self, x, y):
        """Convert world coordinates to screen coordinates"""
        screen_x = FIELD_OFFSET_X + x * SCALE
        screen_y = FIELD_OFFSET_Y + y * SCALE
        return int(screen_x), int(screen_y)
    
    def draw_field(self):
        """Draw the simulation field"""
        # Field boundary
        field_rect = pygame.Rect(
            FIELD_OFFSET_X, FIELD_OFFSET_Y,
            FIELD_SIZE * SCALE, FIELD_SIZE * SCALE
        )
        pygame.draw.rect(self.screen, WHITE, field_rect)
        pygame.draw.rect(self.screen, GRAY, field_rect, 2)
        
        # Grid lines
        for i in range(0, int(FIELD_SIZE), 10):
            start_x, start_y = self.world_to_screen(i, 0)
            end_x, end_y = self.world_to_screen(i, FIELD_SIZE)
            pygame.draw.line(self.screen, LIGHT_GRAY, (start_x, start_y), (end_x, end_y), 1)
            
            start_x, start_y = self.world_to_screen(0, i)
            end_x, end_y = self.world_to_screen(FIELD_SIZE, i)
            pygame.draw.line(self.screen, LIGHT_GRAY, (start_x, start_y), (end_x, end_y), 1)
    
    def draw_vision_rays(self):
        """Draw cat's vision slices"""
        cat_screen_x, cat_screen_y = self.world_to_screen(self.cat_pos[0], self.cat_pos[1])
        
        # Draw all 6 vision slices
        for slice_idx in range(6):
            angle_start = slice_idx * 60 - 30  # Each slice is 60 degrees
            angle_end = angle_start + 60
            
            color = GREEN if slice_idx + 1 == self.vision_slice else LIGHT_GRAY
            
            # Draw vision sector
            for angle in range(int(angle_start), int(angle_end), 10):
                rad = math.radians(angle)
                end_x = self.cat_pos[0] + 15 * math.cos(rad)
                end_y = self.cat_pos[1] + 15 * math.sin(rad)
                
                # Handle toroidal wrapping for display
                if end_x > FIELD_SIZE: end_x -= FIELD_SIZE
                if end_x < 0: end_x += FIELD_SIZE
                if end_y > FIELD_SIZE: end_y -= FIELD_SIZE
                if end_y < 0: end_y += FIELD_SIZE
                
                end_screen_x, end_screen_y = self.world_to_screen(end_x, end_y)
                pygame.draw.line(self.screen, color, 
                               (cat_screen_x, cat_screen_y), 
                               (end_screen_x, end_screen_y), 1)
    
    def draw_entities(self):
        """Draw cat and mouse"""
        # Mouse (red circle)
        mouse_screen_x, mouse_screen_y = self.world_to_screen(self.mouse_pos[0], self.mouse_pos[1])
        pygame.draw.circle(self.screen, RED, (mouse_screen_x, mouse_screen_y), 6)
        
        # Cat (blue triangle pointing in movement direction)
        cat_screen_x, cat_screen_y = self.world_to_screen(self.cat_pos[0], self.cat_pos[1])
        
        # Draw cat as triangle
        triangle_size = 8
        if self.output_action > 0:
            # Direction mapping (same as in Fortran)
            directions = {
                1: (-1, -1),  # Up-Left
                2: (0, -1),   # Up
                3: (1, -1),   # Up-Right
                4: (-1, 0),   # Left
                5: (1, 0),    # Right
                6: (-1, 1),   # Down-Left
                7: (0, 1),    # Down
                8: (1, 1)     # Down-Right
            }
            
            if self.output_action in directions:
                dx, dy = directions[self.output_action]
                angle = math.atan2(dy, dx)
            else:
                angle = 0
        else:
            angle = 0
            
        # Triangle points
        points = []
        for i in range(3):
            point_angle = angle + i * 2 * math.pi / 3
            px = cat_screen_x + triangle_size * math.cos(point_angle)
            py = cat_screen_y + triangle_size * math.sin(point_angle)
            points.append((px, py))
        
        pygame.draw.polygon(self.screen, BLUE, points)
    
    def draw_trails(self):
        """Draw movement trails"""
        # Mouse trail (red)
        if len(self.mouse_trail) > 1:
            for i in range(1, len(self.mouse_trail)):
                start_pos = self.world_to_screen(self.mouse_trail[i-1][0], self.mouse_trail[i-1][1])
                end_pos = self.world_to_screen(self.mouse_trail[i][0], self.mouse_trail[i][1])
                pygame.draw.line(self.screen, (255, 100, 100), start_pos, end_pos, 1)
        
        # Cat trail (blue)
        if len(self.cat_trail) > 1:
            for i in range(1, len(self.cat_trail)):
                start_pos = self.world_to_screen(self.cat_trail[i-1][0], self.cat_trail[i-1][1])
                end_pos = self.world_to_screen(self.cat_trail[i][0], self.cat_trail[i][1])
                pygame.draw.line(self.screen, (100, 100, 255), start_pos, end_pos, 1)
    
    def draw_info_panel(self):
        """Draw information panel"""
        info_x = FIELD_OFFSET_X + FIELD_SIZE * SCALE + 20
        info_y = FIELD_OFFSET_Y
        
        # Background
        info_rect = pygame.Rect(info_x - 10, info_y - 10, 200, 300)
        pygame.draw.rect(self.screen, WHITE, info_rect)
        pygame.draw.rect(self.screen, GRAY, info_rect, 2)
        
        # Information text
        texts = [
            f"Evolved Brain Demo",
            f"",
            f"Step: {self.step}",
            f"Distance: {self.distance:.2f}",
            f"",
            f"Cat: ({self.cat_pos[0]:.1f}, {self.cat_pos[1]:.1f})",
            f"Mouse: ({self.mouse_pos[0]:.1f}, {self.mouse_pos[1]:.1f})",
            f"",
            f"Vision Slice: {self.vision_slice}/6",
            f"Output Action: {self.output_action}",
            f"",
            f"Performance:",
            f"Avg Distance: {sum(self.distance_history[-50:]) / len(self.distance_history[-50:]):.1f}" if self.distance_history else "N/A",
            f"",
            f"Controls:",
            f"ESC - Exit",
            f"",
            f"Legend:",
            f"🔴 Mouse",
            f"🔵 Cat (evolved brain)",
            f"Green lines - Active vision",
            f"Gray lines - Inactive vision"
        ]
        
        for i, text in enumerate(texts):
            color = BLACK
            if "Evolved Brain" in text:
                color = DARK_BLUE
            elif "Performance:" in text or "Controls:" in text or "Legend:" in text:
                color = DARK_BLUE
                
            text_surface = self.small_font.render(text, True, color)
            self.screen.blit(text_surface, (info_x, info_y + i * 20))
    
    def draw_distance_graph(self):
        """Draw distance over time graph"""
        if len(self.distance_history) < 2:
            return
            
        graph_x = FIELD_OFFSET_X
        graph_y = FIELD_OFFSET_Y + FIELD_SIZE * SCALE + 30
        graph_width = FIELD_SIZE * SCALE
        graph_height = 100
        
        # Background
        graph_rect = pygame.Rect(graph_x, graph_y, graph_width, graph_height)
        pygame.draw.rect(self.screen, WHITE, graph_rect)
        pygame.draw.rect(self.screen, GRAY, graph_rect, 2)
        
        # Title
        title = self.font.render("Distance Over Time", True, BLACK)
        self.screen.blit(title, (graph_x, graph_y - 25))
        
        # Plot distance
        if len(self.distance_history) > 1:
            max_dist = max(self.distance_history[-200:]) if self.distance_history else 50
            min_dist = min(self.distance_history[-200:]) if self.distance_history else 0
            dist_range = max_dist - min_dist if max_dist > min_dist else 1
            
            recent_history = self.distance_history[-200:]
            for i in range(1, len(recent_history)):
                x1 = graph_x + (i - 1) * graph_width / len(recent_history)
                y1 = graph_y + graph_height - ((recent_history[i - 1] - min_dist) / dist_range * graph_height)
                x2 = graph_x + i * graph_width / len(recent_history)
                y2 = graph_y + graph_height - ((recent_history[i] - min_dist) / dist_range * graph_height)
                pygame.draw.line(self.screen, RED, (x1, y1), (x2, y2), 2)
    
    def start_simulation(self):
        """Start the Fortran simulation subprocess"""
        try:
            self.process = subprocess.Popen(
                ['./evolved_brain_gui_demo'],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                universal_newlines=True,
                bufsize=1
            )
            
            # Start reader thread
            self.reader_thread = Thread(target=self.read_simulation_output, daemon=True)
            self.reader_thread.start()
            
        except Exception as e:
            print(f"Error starting simulation: {e}")
            return False
        
        return True
    
    def read_simulation_output(self):
        """Read simulation output in separate thread"""
        try:
            for line in self.process.stdout:
                line = line.strip()
                if line and not line.startswith('#'):
                    self.data_queue.put(line)
        except Exception as e:
            print(f"Error reading simulation output: {e}")
    
    def update_from_simulation(self):
        """Update state from simulation data"""
        try:
            while True:
                line = self.data_queue.get_nowait()
                if line.startswith("step,"):
                    continue  # Skip header
                
                parts = line.split(',')
                if len(parts) >= 8:
                    self.step = int(parts[0])
                    self.cat_pos = [float(parts[1]), float(parts[2])]
                    self.mouse_pos = [float(parts[3]), float(parts[4])]
                    self.distance = float(parts[5])
                    self.vision_slice = int(parts[6])
                    self.output_action = int(parts[7])
                    
                    # Update history
                    self.distance_history.append(self.distance)
                    if len(self.distance_history) > 500:
                        self.distance_history.pop(0)
                    
                    # Update trails
                    self.cat_trail.append((self.cat_pos[0], self.cat_pos[1]))
                    self.mouse_trail.append((self.mouse_pos[0], self.mouse_pos[1]))
                    
                    # Limit trail length
                    if len(self.cat_trail) > 100:
                        self.cat_trail.pop(0)
                    if len(self.mouse_trail) > 100:
                        self.mouse_trail.pop(0)
                    
        except Empty:
            pass
    
    def run(self):
        """Main loop"""
        if not self.start_simulation():
            print("Failed to start simulation")
            return
        
        running = True
        while running:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        running = False
            
            # Update from simulation
            self.update_from_simulation()
            
            # Draw everything
            self.screen.fill(BLACK)
            self.draw_field()
            self.draw_vision_rays()
            self.draw_trails()
            self.draw_entities()
            self.draw_info_panel()
            self.draw_distance_graph()
            
            pygame.display.flip()
            self.clock.tick(30)  # 30 FPS
        
        # Cleanup
        if self.process:
            self.process.terminate()
        
        pygame.quit()

if __name__ == "__main__":
    viewer = EvolvedBrainViewer()
    viewer.run()