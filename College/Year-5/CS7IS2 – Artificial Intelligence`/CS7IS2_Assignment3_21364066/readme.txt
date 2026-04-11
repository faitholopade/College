CS7IS2 Assignment 3 - Minimax and Reinforcement Learning for Game Playing
=========================================================================
Student ID: 21364066

Project Structure
-----------------
main.py                 - Main entry point, runs all 5 experiments
games.py                - Tic Tac Toe and Connect 4 game implementations
minimax_algorithms.py   - Minimax (with and without alpha-beta pruning)
rl_algorithms.py        - Tabular Q-Learning and Deep Q-Network (DQN)
opponents.py            - Default (semi-intelligent) and Random opponents
results/                - Output directory for graphs and data
readme.txt              - This file

Dependencies
------------
Python 3.8+
numpy
matplotlib

Install:
    pip install numpy matplotlib

Running the Code
----------------

Full run (all 5 experiments, ~35-45 min):
    python main.py

Quick mode (fewer episodes/games, ~8-12 min):
    python main.py --quick

Scalability test only (~1 min):
    python main.py --scalability

Experiments Run
---------------
1. TTT: All 4 algorithms vs Default Opponent (100 games each)
2. Connect 4: All 4 algorithms vs Default Opponent (20 games each)
3. Head-to-Head: All algorithms vs each other on both games
4. Statistical Analysis: Multiple independent runs on BOTH games (3 runs each)
5. Hyperparameter Sensitivity:
   - Connect 4 Minimax depth variation (d=3, d=4, d=5)
   - Q-Learning training episode variation (10k, 30k, 80k)
   - Q-Learning learning rate variation (0.05, 0.1, 0.2)

Algorithms Implemented
----------------------
1. Minimax (no pruning)         - Transposition table, full depth TTT, depth-limited C4
2. Minimax with Alpha-Beta      - Full depth TTT, depth-limited C4
3. Tabular Q-Learning           - Epsilon-greedy, off-policy, 80k episodes TTT, 30k C4
4. Deep Q-Network (DQN)         - NumPy neural net, Huber loss, Adam, replay buffer,
                                  soft target updates, 10k episodes both games

Default Opponent
----------------
1. Takes winning move if available
2. Blocks opponent winning move if available
3. Otherwise plays random valid move

Output Graphs
-------------
results/ttt_vs_default.png           - TTT algorithm comparison
results/ttt_training_curves.png      - TTT RL training progress
results/ttt_time_comparison.png      - TTT decision time comparison
results/tictactoe_head_to_head.png   - TTT head-to-head heatmap
results/tictactoe_multirun_boxplot.png - TTT multi-run statistical analysis
results/c4_vs_default.png            - C4 algorithm comparison
results/c4_training_curves.png       - C4 RL training progress
results/c4_time_comparison.png       - C4 decision time comparison
results/c4_scalability.png           - C4 scalability test (nodes vs depth)
results/connectfour_head_to_head.png - C4 head-to-head heatmap
results/connectfour_multirun_boxplot.png - C4 multi-run statistical analysis
results/c4_depth_variation.png       - C4 Minimax depth sensitivity
results/ttt_ql_episode_variation.png - TTT Q-Learning episode sensitivity
results/ttt_ql_lr_variation.png      - TTT Q-Learning learning rate sensitivity
results/overall_summary.png          - Combined summary chart
