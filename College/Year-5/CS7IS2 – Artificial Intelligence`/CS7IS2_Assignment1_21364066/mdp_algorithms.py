"""
MDP Algorithms Module
=====================
Implements Value Iteration and Policy Iteration for maze solving,
treating the maze as a Markov Decision Process.

MDP Formulation for Maze Solving
---------------------------------
- **States (S):**  Every open (passable) cell in the grid.
- **Actions (A):** {UP, DOWN, LEFT, RIGHT} — move one cell in a cardinal direction.
- **Transition function T(s, a, s'):**
      With probability `p_intended` the agent moves in the intended direction.
      With probability `(1 - p_intended) / 2` the agent slips to each
      perpendicular direction.  If any resulting move hits a wall, the agent
      stays in place.  (Set p_intended=1.0 for fully deterministic behaviour.)
- **Rewards R(s):**
      +100  for reaching the goal state.
       -1   per step (living penalty to encourage shorter paths).
       -50  for wall collisions (attempting to move into a wall).
- **Discount factor (gamma):**  Configurable, default 0.99.

Both algorithms produce a policy (mapping state → action) from which the
optimal path from start to goal can be extracted.
"""

import numpy as np
import time
from typing import List, Tuple, Optional, Dict
from maze_generator import Maze


# Constants
ACTIONS = {
    "UP":    (-1, 0),
    "DOWN":  (1, 0),
    "LEFT":  (0, -1),
    "RIGHT": (0, 1),
}
ACTION_LIST = list(ACTIONS.keys())

# Perpendicular actions for stochastic transitions
PERPENDICULAR = {
    "UP":    ["LEFT", "RIGHT"],
    "DOWN":  ["LEFT", "RIGHT"],
    "LEFT":  ["UP", "DOWN"],
    "RIGHT": ["UP", "DOWN"],
}



class MDPResult:
    """Stores the outcome of an MDP solve."""

    def __init__(self, algorithm: str, path: Optional[List[Tuple[int, int]]],
                 nodes_explored: int, iterations: int, elapsed_time: float,
                 converged: bool):
        self.algorithm = algorithm
        self.path = path
        self.path_length = len(path) if path else 0
        self.nodes_explored = nodes_explored
        self.iterations = iterations
        self.elapsed_time = elapsed_time
        self.converged = converged
        self.solved = path is not None and len(path) > 0

    def __repr__(self):
        return (f"MDPResult(algo={self.algorithm}, solved={self.solved}, "
                f"path_len={self.path_length}, iterations={self.iterations}, "
                f"explored={self.nodes_explored}, time={self.elapsed_time:.6f}s)")


def _attempt_move(maze: Maze, state: Tuple[int, int],
                  action_name: str) -> Tuple[int, int]:
    """
    Try to move from `state` in direction `action_name`.
    Returns the new state, or the same state if blocked by a wall.
    """
    dr, dc = ACTIONS[action_name]
    nr, nc = state[0] + dr, state[1] + dc
    if (0 <= nr < maze.grid.shape[0] and 0 <= nc < maze.grid.shape[1]
            and maze.grid[nr, nc] == 0):
        return (nr, nc)
    return state  # blocked — stay in place


def _get_transitions(maze: Maze, state: Tuple[int, int],
                     action: str, p_intended: float = 1.0):
    """
    Return list of (probability, next_state) for taking `action` in `state`.

    With probability p_intended the agent goes in the intended direction.
    Remaining probability is split equally between the two perpendicular
    directions.
    """
    transitions = []
    p_side = (1.0 - p_intended) / 2.0

    # Intended direction
    next_state = _attempt_move(maze, state, action)
    transitions.append((p_intended, next_state))

    # Perpendicular slips
    for perp_action in PERPENDICULAR[action]:
        next_state_perp = _attempt_move(maze, state, perp_action)
        transitions.append((p_side, next_state_perp))

    return transitions


def _extract_path(maze: Maze, policy: Dict, max_steps: int = 50000
                  ) -> Optional[List[Tuple[int, int]]]:
    """
    Follow the greedy policy from start to goal.
    Returns the path as a list of grid positions, or None if the
    policy does not reach the goal within max_steps.
    """
    path = [maze.start]
    current = maze.start
    visited_count = {}

    for _ in range(max_steps):
        if current == maze.goal:
            return path
        if current not in policy:
            return None

        visited_count[current] = visited_count.get(current, 0) + 1
        if visited_count[current] > 4:  # stuck in a loop
            return None

        action = policy[current]
        next_state = _attempt_move(maze, current, action)
        path.append(next_state)
        current = next_state

    return None  # did not converge to goal


# Value Iteration
def value_iteration(maze: Maze, gamma: float = 0.99,
                    theta: float = 1e-6, max_iterations: int = 5000,
                    p_intended: float = 1.0,
                    step_reward: float = -1.0,
                    goal_reward: float = 100.0) -> MDPResult:
    """
    Solve the maze MDP using Value Iteration.

    Repeatedly applies the Bellman optimality update:
        V(s) = max_a  Σ_s' T(s,a,s') [ R(s,a,s') + γ V(s') ]
    until convergence (max change < theta).

    Parameters
    ----------
    maze : Maze
        The maze to solve.
    gamma : float
        Discount factor (0 < gamma <= 1).
    theta : float
        Convergence threshold.
    max_iterations : int
        Safety limit on iterations.
    p_intended : float
        Probability of moving in the intended direction.
    step_reward : float
        Reward (penalty) for each step taken.
    goal_reward : float
        Reward for reaching the goal.

    Returns
    -------
    MDPResult
    """
    start_time = time.perf_counter()

    # Identify all open (passable) states
    open_cells = maze.get_all_open_cells()
    states = set(open_cells)

    # Initialise value function
    V = {s: 0.0 for s in states}
    V[maze.goal] = goal_reward  # terminal state

    nodes_explored = 0  # total state-action evaluations
    converged = False

    for iteration in range(1, max_iterations + 1):
        delta = 0.0

        for s in states:
            if s == maze.goal:
                continue  # goal is terminal

            old_v = V[s]
            best_value = float('-inf')

            for action in ACTION_LIST:
                action_value = 0.0
                transitions = _get_transitions(maze, s, action, p_intended)

                for prob, next_state in transitions:
                    if next_state in states:
                        reward = goal_reward if next_state == maze.goal else step_reward
                        action_value += prob * (reward + gamma * V[next_state])
                    else:
                        # Hit a wall — stay in place with penalty
                        action_value += prob * (step_reward + gamma * V[s])

                nodes_explored += 1
                best_value = max(best_value, action_value)

            V[s] = best_value
            delta = max(delta, abs(old_v - V[s]))

        if delta < theta:
            converged = True
            break

    # Extract policy from converged value function
    policy = {}
    for s in states:
        if s == maze.goal:
            continue
        best_action = None
        best_value = float('-inf')
        for action in ACTION_LIST:
            action_value = 0.0
            transitions = _get_transitions(maze, s, action, p_intended)
            for prob, next_state in transitions:
                if next_state in states:
                    reward = goal_reward if next_state == maze.goal else step_reward
                    action_value += prob * (reward + gamma * V[next_state])
                else:
                    action_value += prob * (step_reward + gamma * V[s])
            if action_value > best_value:
                best_value = action_value
                best_action = action
        policy[s] = best_action

    path = _extract_path(maze, policy)
    elapsed = time.perf_counter() - start_time

    return MDPResult("Value Iteration", path, nodes_explored, iteration,
                     elapsed, converged)


# Policy Iteration
def policy_iteration(maze: Maze, gamma: float = 0.99,
                     theta: float = 1e-6, max_iterations: int = 1000,
                     p_intended: float = 1.0,
                     step_reward: float = -1.0,
                     goal_reward: float = 100.0) -> MDPResult:
    """
    Solve the maze MDP using Policy Iteration.

    Alternates between:
      1. Policy Evaluation — compute V^π for the current policy π.
      2. Policy Improvement — greedily update π w.r.t. V^π.
    Converges when the policy is stable (no changes in an improvement step).

    Parameters
    ----------
    (same as value_iteration)

    Returns
    -------
    MDPResult
    """
    start_time = time.perf_counter()

    open_cells = maze.get_all_open_cells()
    states = set(open_cells)
    states_list = list(states - {maze.goal})

    # Initialise random policy
    import random
    policy = {s: random.choice(ACTION_LIST) for s in states_list}

    V = {s: 0.0 for s in states}
    V[maze.goal] = goal_reward

    nodes_explored = 0
    converged = False

    for iteration in range(1, max_iterations + 1):
        # ---- Policy Evaluation ----
        while True:
            delta = 0.0
            for s in states_list:
                old_v = V[s]
                action = policy[s]
                action_value = 0.0
                transitions = _get_transitions(maze, s, action, p_intended)

                for prob, next_state in transitions:
                    if next_state in states:
                        reward = goal_reward if next_state == maze.goal else step_reward
                        action_value += prob * (reward + gamma * V[next_state])
                    else:
                        action_value += prob * (step_reward + gamma * V[s])

                nodes_explored += 1
                V[s] = action_value
                delta = max(delta, abs(old_v - V[s]))

            if delta < theta:
                break

        # ---- Policy Improvement ----
        policy_stable = True
        for s in states_list:
            old_action = policy[s]
            best_action = None
            best_value = float('-inf')

            for action in ACTION_LIST:
                action_value = 0.0
                transitions = _get_transitions(maze, s, action, p_intended)
                for prob, next_state in transitions:
                    if next_state in states:
                        reward = goal_reward if next_state == maze.goal else step_reward
                        action_value += prob * (reward + gamma * V[next_state])
                    else:
                        action_value += prob * (step_reward + gamma * V[s])
                nodes_explored += 1
                if action_value > best_value:
                    best_value = action_value
                    best_action = action

            policy[s] = best_action
            if old_action != best_action:
                policy_stable = False

        if policy_stable:
            converged = True
            break

    path = _extract_path(maze, policy)
    elapsed = time.perf_counter() - start_time

    return MDPResult("Policy Iteration", path, nodes_explored, iteration,
                     elapsed, converged)



def run_all_mdp(maze: Maze, **kwargs) -> list:
    """Run both MDP algorithms and return results."""
    return [value_iteration(maze, **kwargs), policy_iteration(maze, **kwargs)]



if __name__ == "__main__":
    m = Maze(10, 10, seed=42)
    print(m)
    print()

    for result in run_all_mdp(m):
        print(result)
        if result.path:
            m.display(result.path)
            print()
