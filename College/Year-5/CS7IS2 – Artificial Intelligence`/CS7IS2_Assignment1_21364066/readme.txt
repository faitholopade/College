CS7IS2 Assignment 1 — Maze Search and MDP Comparison 
================================================================

REQUIREMENTS
------------
Python 3.8+
numpy
matplotlib
Pillow (PIL) — optional, for maze images

Install:
    pip install numpy matplotlib Pillow


COMMAND LINES TO RUN
---------------------

1. Run ALL experiments (core comparison + sensitivity analysis):
    python main.py

2. Run with custom maze sizes:
    python main.py --sizes 5 10 20 30 50

3. Run with multiple repetitions for statistical analysis:
    python main.py --sizes 5 10 20 30 50 --runs 5

4. Run ONLY core comparison (skip sensitivity — faster):
    python main.py --skip-sensitivity

5. Run individual modules for testing:

   Maze Generator only:
       python maze_generator.py

   Search Algorithms only (DFS, BFS, A* Manhattan, A* Euclidean):
       python search_algorithms.py

   MDP Algorithms only (Value Iteration, Policy Iteration):
       python mdp_algorithms.py


OUTPUT FILES
------------
All results are saved to the results/ directory:

Core experiment outputs:
    - results_raw.csv                   Raw data for every experiment run
    - results_summary.csv               Aggregated means, std devs, 95% CIs
    - comparison_time.png               Execution time (all algorithms)
    - comparison_time_search_only.png   Execution time (search only, ms scale)
    - comparison_explored.png           Nodes explored (all algorithms)
    - comparison_explored_search_only.png  Nodes explored (search only)
    - comparison_path_length.png        Path length comparison
    - comparison_frontier.png           Max frontier size (search only)
    - comparison_mdp_iterations.png     Iterations to convergence (MDP only)
    - maze_*_solved_*.png               Example solved maze visualisations

Sensitivity analysis outputs:
    - sensitivity_gamma.csv                     Gamma sensitivity raw data
    - sensitivity_gamma_iterations.png          Gamma vs iterations chart
    - sensitivity_gamma_time.png                Gamma vs execution time chart
    - sensitivity_p_intended.csv                Stochastic transition raw data
    - sensitivity_p_intended_iterations.png     p_intended vs iterations chart
    - sensitivity_p_intended_time.png           p_intended vs execution time chart
    - sensitivity_p_intended_path.png           p_intended vs path length chart


PROJECT STRUCTURE
-----------------
    maze_generator.py       Maze generation (Recursive Backtracking)
    search_algorithms.py    DFS, BFS, A* Manhattan, A* Euclidean
    mdp_algorithms.py       Value Iteration, Policy Iteration
    main.py                 Experiment runner (core + sensitivity + charts)
    readme.txt              This file
