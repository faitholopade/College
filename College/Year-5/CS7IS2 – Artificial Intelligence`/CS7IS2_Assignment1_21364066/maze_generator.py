"""
Maze Generator Module
=====================
Generates perfect mazes (exactly one path between any two cells) using
Recursive Backtracking (randomised DFS) algorithm.

A "perfect" maze has no loops and no inaccessible areas — every cell is
reachable from every other cell via exactly one unique path.

Algorithm reference:
    Recursive Backtracking is a well-known randomised depth-first search
    approach for maze generation. See:
    - Jamis Buck, "Mazes for Programmers" (2015)
    - Wikipedia: Maze generation algorithm – Randomized DFS

Maze representation:
    The maze is stored as a 2-D NumPy grid of integers.
        0 = open passage
        1 = wall
    Dimensions are (2*rows+1) x (2*cols+1) to account for walls between cells.
    Cell (r, c) maps to grid position (2*r+1, 2*c+1).
    Start is top-left cell (0, 0) and goal is bottom-right cell (rows-1, cols-1).
"""

import numpy as np
import random
from typing import Tuple, Optional


class Maze:
    """
    Represents a rectangular maze of configurable size.

    Attributes
    ----------
    rows : int
        Number of cell rows in the maze.
    cols : int
        Number of cell columns in the maze.
    grid : np.ndarray
        The full wall/passage grid of shape (2*rows+1, 2*cols+1).
    start : tuple
        (row, col) grid coordinates of the start cell.
    goal : tuple
        (row, col) grid coordinates of the goal cell.
    """

    def __init__(self, rows: int, cols: int, seed: Optional[int] = None):
        """
        Generate a new random maze.

        Parameters
        ----------
        rows : int
            Number of cell rows (must be >= 2).
        cols : int
            Number of cell columns (must be >= 2).
        seed : int, optional
            Random seed for reproducibility.
        """
        if rows < 2 or cols < 2:
            raise ValueError("Maze dimensions must be at least 2x2 cells.")

        self.rows = rows
        self.cols = cols
        self.seed = seed

        if seed is not None:
            random.seed(seed)

        # grid: everything is a wall (1)
        self.grid = np.ones((2 * rows + 1, 2 * cols + 1), dtype=np.int8)

        # Generate maze using recursive backtracking
        self._generate()

        # start and goal positions (grid coordinates)
        self.start = (1, 1)                         # top left cell
        self.goal = (2 * rows - 1, 2 * cols - 1)    # bottom right cell

    # Maze generation using iterative randomised DFS (recursive backtracking)
    def _generate(self):
        """Carve passages using iterative randomised DFS."""
        visited = np.zeros((self.rows, self.cols), dtype=bool)

        # Directions: (delta_row, delta_col)
        directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

        # Start carving from cell (0, 0)
        stack = [(0, 0)]
        visited[0, 0] = True
        # Open the starting cell on the grid
        self.grid[1, 1] = 0

        while stack:
            cr, cc = stack[-1]

            # Find unvisited neighbours
            neighbours = []
            for dr, dc in directions:
                nr, nc = cr + dr, cc + dc
                if 0 <= nr < self.rows and 0 <= nc < self.cols and not visited[nr, nc]:
                    neighbours.append((nr, nc, dr, dc))

            if neighbours:
                # Choose a random unvisited neighbour
                nr, nc, dr, dc = random.choice(neighbours)
                visited[nr, nc] = True

                # Remove the wall between current cell and chosen neighbour
                wall_r = 2 * cr + 1 + dr
                wall_c = 2 * cc + 1 + dc
                self.grid[wall_r, wall_c] = 0

                # Open the neighbour cell itself
                self.grid[2 * nr + 1, 2 * nc + 1] = 0

                stack.append((nr, nc))
            else:
                stack.pop()  # backtrack


    def get_neighbours(self, r: int, c: int):
        """
        Return walkable neighbours of grid position (r, c).

        Parameters
        ----------
        r, c : int
            Row and column in the full grid.

        Returns
        -------
        list of (int, int)
            Grid coordinates of adjacent open cells.
        """
        neighbours = []
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < self.grid.shape[0] and 0 <= nc < self.grid.shape[1]:
                if self.grid[nr, nc] == 0:
                    neighbours.append((nr, nc))
        return neighbours

    def get_all_open_cells(self):
        """Return list of all open (passable) grid positions."""
        return list(zip(*np.where(self.grid == 0)))

    def cell_to_grid(self, cell_r: int, cell_c: int) -> Tuple[int, int]:
        """Convert logical cell coordinates to grid coordinates."""
        return (2 * cell_r + 1, 2 * cell_c + 1)

    def grid_to_cell(self, grid_r: int, grid_c: int) -> Tuple[float, float]:
        """Convert grid coordinates back to (approximate) cell coordinates."""
        return ((grid_r - 1) / 2, (grid_c - 1) / 2)

    def display(self, path=None):
        """
        Print the maze to the console.

        Parameters
        ----------
        path : list of (int, int), optional
            A path (grid coordinates) to highlight with '·'.
        """
        path_set = set(path) if path else set()
        symbols = {1: "██", 0: "  "}

        for r in range(self.grid.shape[0]):
            row_str = ""
            for c in range(self.grid.shape[1]):
                if (r, c) == self.start:
                    row_str += " S"
                elif (r, c) == self.goal:
                    row_str += " G"
                elif (r, c) in path_set:
                    row_str += " ·"
                else:
                    row_str += symbols[self.grid[r, c]]
            print(row_str)

    def to_image(self, path=None, cell_size: int = 10):
        """
        Render the maze as a PIL Image (for saving / embedding in reports).

        Parameters
        ----------
        path : list of (int, int), optional
            Path to draw (grid coordinates).
        cell_size : int
            Pixel size per grid cell.

        Returns
        -------
        PIL.Image.Image
        """
        from PIL import Image, ImageDraw

        h, w = self.grid.shape
        img = Image.new("RGB", (w * cell_size, h * cell_size), "white")
        draw = ImageDraw.Draw(img)

        path_set = set(path) if path else set()

        for r in range(h):
            for c in range(w):
                x0, y0 = c * cell_size, r * cell_size
                x1, y1 = x0 + cell_size, y0 + cell_size

                if self.grid[r, c] == 1:
                    draw.rectangle([x0, y0, x1, y1], fill="black")
                elif (r, c) == self.start:
                    draw.rectangle([x0, y0, x1, y1], fill="green")
                elif (r, c) == self.goal:
                    draw.rectangle([x0, y0, x1, y1], fill="red")
                elif (r, c) in path_set:
                    draw.rectangle([x0, y0, x1, y1], fill="dodgerblue")

        return img

    def __repr__(self):
        return f"Maze(rows={self.rows}, cols={self.cols}, grid_shape={self.grid.shape})"

#test
if __name__ == "__main__":
    print("=== Maze Generator Demo ===\n")

    for size in [5, 10, 15]:
        m = Maze(size, size, seed=42)
        print(f"\n--- {size}x{size} maze (grid {m.grid.shape}) ---")
        m.display()
        print(f"Start: {m.start}  Goal: {m.goal}")
        print(f"Open cells: {len(m.get_all_open_cells())}")
