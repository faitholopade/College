"""
Main Experiment Runner
=================================
Runs all 6 algorithms (DFS, BFS, A* Manhattan, A* Euclidean,
Value Iteration, Policy Iteration) across a range of maze sizes,
plus MDP parameter sensitivity analysis (gamma, p_intended).

Usage:
    python main.py                          # Run with default sizes
    python main.py --sizes 5 10 20 30 50    # Specify custom sizes
    python main.py --runs 5                 # Multiple runs per size
    python main.py --skip-sensitivity       # Skip MDP parameter sensitivity
    python main.py --help                   # See all options

Output files (saved to ./results/):
    - results_raw.csv               All raw data from core experiments
    - results_summary.csv           Aggregated means and std devs
    - sensitivity_gamma.csv         Gamma sensitivity raw data
    - sensitivity_p_intended.csv    Stochastic transition raw data
    - comparison_*.png              Visualisation charts (core experiments)
    - sensitivity_*.png             Sensitivity analysis charts
    - maze_*_solved_*.png           Example solved maze visualisations
"""

import argparse
import csv
import os
import sys
import time
import statistics
from typing import List
from collections import defaultdict

import numpy as np

from maze_generator import Maze
from search_algorithms import dfs, bfs, astar_manhattan, astar_euclidean, SearchResult
from mdp_algorithms import value_iteration, policy_iteration, MDPResult


# Config
DEFAULT_SIZES = [5, 10, 20, 30, 50]
DEFAULT_RUNS = 3
RESULTS_DIR = "results"

# MDP params
DEFAULT_MDP_PARAMS = {
    "gamma": 0.99,
    "theta": 1e-6,
    "p_intended": 1.0,
    "step_reward": -1.0,
    "goal_reward": 100.0,
}

# Sensitivity analysis parameter ranges
GAMMA_VALUES = [0.9, 0.95, 0.99]
P_INTENDED_VALUES = [0.7, 0.8, 0.9, 1.0]
SENSITIVITY_SIZES = [5, 10, 20]
SENSITIVITY_RUNS = 3


# Experiment 1: Core comparison of all algorithms across maze sizes
def run_core_experiments(sizes, num_runs, verbose=True):
    """Run all algorithms on mazes of each size, repeated num_runs times."""
    all_results = []
    algorithms = [
        ("DFS", dfs),
        ("BFS", bfs),
        ("A*-Manhattan", astar_manhattan),
        ("A*-Euclidean", astar_euclidean),
    ]

    total = len(sizes) * num_runs * (len(algorithms) + 2)
    exp_num = 0

    for size in sizes:
        for run in range(1, num_runs + 1):
            seed = size * 1000 + run
            maze = Maze(size, size, seed=seed)

            if verbose:
                print(f"\n{'='*60}")
                print(f"Maze: {size}x{size} (grid {maze.grid.shape})  |  Run {run}/{num_runs}")
                print(f"{'='*60}")

            for algo_name, algo_func in algorithms:
                exp_num += 1
                if verbose:
                    print(f"  [{exp_num}/{total}] Running {algo_name}...", end=" ", flush=True)

                result = algo_func(maze)
                row = {
                    "maze_size": size, "run": run, "seed": seed,
                    "algorithm": result.algorithm, "family": "Search",
                    "solved": result.solved, "path_length": result.path_length,
                    "nodes_explored": result.nodes_explored,
                    "max_frontier": result.max_frontier,
                    "iterations": "N/A", "converged": "N/A",
                    "elapsed_time_s": result.elapsed_time,
                }
                all_results.append(row)
                if verbose:
                    s = "\u2713" if result.solved else "\u2717"
                    print(f"{s} path={result.path_length}, explored={result.nodes_explored}, time={result.elapsed_time:.4f}s")
                if run == 1 and result.path:
                    _save_maze_image(maze, result.path, algo_name, size)

            for algo_name, algo_func in [("Value Iteration", value_iteration),
                                          ("Policy Iteration", policy_iteration)]:
                exp_num += 1
                if verbose:
                    print(f"  [{exp_num}/{total}] Running {algo_name}...", end=" ", flush=True)

                result = algo_func(maze, **DEFAULT_MDP_PARAMS)
                row = {
                    "maze_size": size, "run": run, "seed": seed,
                    "algorithm": result.algorithm, "family": "MDP",
                    "solved": result.solved, "path_length": result.path_length,
                    "nodes_explored": result.nodes_explored,
                    "max_frontier": "N/A",
                    "iterations": result.iterations, "converged": result.converged,
                    "elapsed_time_s": result.elapsed_time,
                }
                all_results.append(row)
                if verbose:
                    s = "\u2713" if result.solved else "\u2717"
                    print(f"{s} path={result.path_length}, iters={result.iterations}, time={result.elapsed_time:.4f}s")
                if run == 1 and result.path:
                    _save_maze_image(maze, result.path, algo_name, size)

    return all_results


# Experiment 2: Sensitivity analysis for gamma
def run_gamma_sensitivity(sizes, num_runs, verbose=True):
    """Vary gamma across GAMMA_VALUES for VI and PI."""
    print("\n" + "="*60)
    print("  EXPERIMENT 2: Gamma Sensitivity Analysis")
    print("="*60)

    all_results = []
    for size in sizes:
        for run in range(1, num_runs + 1):
            seed = size * 1000 + run
            maze = Maze(size, size, seed=seed)
            for gamma in GAMMA_VALUES:
                params = dict(DEFAULT_MDP_PARAMS)
                params["gamma"] = gamma
                for algo_name, algo_func in [("Value Iteration", value_iteration),
                                              ("Policy Iteration", policy_iteration)]:
                    if verbose:
                        print(f"  {size}x{size} run={run} gamma={gamma} {algo_name}...", end=" ", flush=True)
                    result = algo_func(maze, **params)
                    row = {
                        "maze_size": size, "run": run, "seed": seed,
                        "gamma": gamma, "algorithm": algo_name,
                        "solved": result.solved, "path_length": result.path_length,
                        "nodes_explored": result.nodes_explored,
                        "iterations": result.iterations,
                        "converged": result.converged,
                        "elapsed_time_s": result.elapsed_time,
                    }
                    all_results.append(row)
                    if verbose:
                        s = "\u2713" if result.solved else "\u2717"
                        print(f"{s} iters={result.iterations}, time={result.elapsed_time:.4f}s")
    return all_results


# Experiment 3: Sensitivity analysis for p_intended
def run_p_intended_sensitivity(sizes, num_runs, verbose=True):
    """Vary p_intended across P_INTENDED_VALUES for VI and PI."""
    print("\n" + "="*60)
    print("  EXPERIMENT 3: Stochastic Transition Sensitivity Analysis")
    print("="*60)

    all_results = []
    for size in sizes:
        for run in range(1, num_runs + 1):
            seed = size * 1000 + run
            maze = Maze(size, size, seed=seed)
            for p in P_INTENDED_VALUES:
                params = dict(DEFAULT_MDP_PARAMS)
                params["p_intended"] = p
                for algo_name, algo_func in [("Value Iteration", value_iteration),
                                              ("Policy Iteration", policy_iteration)]:
                    if verbose:
                        print(f"  {size}x{size} run={run} p={p} {algo_name}...", end=" ", flush=True)
                    result = algo_func(maze, **params)
                    row = {
                        "maze_size": size, "run": run, "seed": seed,
                        "p_intended": p, "algorithm": algo_name,
                        "solved": result.solved, "path_length": result.path_length,
                        "nodes_explored": result.nodes_explored,
                        "iterations": result.iterations,
                        "converged": result.converged,
                        "elapsed_time_s": result.elapsed_time,
                    }
                    all_results.append(row)
                    if verbose:
                        s = "\u2713" if result.solved else "\u2717"
                        print(f"{s} iters={result.iterations}, time={result.elapsed_time:.4f}s")
    return all_results



def _save_maze_image(maze, path, algo_name, size):
    try:
        from PIL import Image
        img = maze.to_image(path=path, cell_size=max(2, 20 // max(1, size // 10)))
        safe_name = algo_name.replace("*", "star").replace(" ", "_")
        filepath = os.path.join(RESULTS_DIR, f"maze_{size}x{size}_{safe_name}.png")
        img.save(filepath)
    except ImportError:
        pass



def export_csv(results, filepath):
    if not results:
        return
    with open(filepath, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=results[0].keys())
        writer.writeheader()
        writer.writerows(results)
    print(f"  Saved: {filepath}")


def export_summary_csv(results, filepath):
    grouped = defaultdict(list)
    for row in results:
        key = (row["maze_size"], row["algorithm"])
        grouped[key].append(row)

    summary_rows = []
    for (size, algo), rows in sorted(grouped.items()):
        times = [r["elapsed_time_s"] for r in rows]
        paths = [r["path_length"] for r in rows if r["solved"]]
        explored = [r["nodes_explored"] for r in rows]
        n = len(times)
        mean_t = statistics.mean(times)
        std_t = statistics.stdev(times) if n > 1 else 0
        ci95_t = 1.96 * std_t / (n ** 0.5) if n > 1 else 0
        mean_e = statistics.mean(explored)
        std_e = statistics.stdev(explored) if n > 1 else 0
        ci95_e = 1.96 * std_e / (n ** 0.5) if n > 1 else 0

        summary_rows.append({
            "maze_size": size, "algorithm": algo, "family": rows[0]["family"],
            "runs": n,
            "solved_count": sum(1 for r in rows if r["solved"]),
            "mean_path_length": statistics.mean(paths) if paths else "N/A",
            "std_path_length": statistics.stdev(paths) if len(paths) > 1 else 0,
            "mean_nodes_explored": mean_e, "std_nodes_explored": std_e,
            "ci95_nodes_explored": ci95_e,
            "mean_time_s": mean_t, "std_time_s": std_t, "ci95_time_s": ci95_t,
        })

    if not summary_rows:
        return
    with open(filepath, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=summary_rows[0].keys())
        writer.writeheader()
        writer.writerows(summary_rows)
    print(f"  Saved: {filepath}")



def generate_core_charts(results):
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        print("matplotlib not available.")
        return

    grouped = defaultdict(list)
    for row in results:
        grouped[(row["maze_size"], row["algorithm"])].append(row)

    sizes = sorted(set(r["maze_size"] for r in results))
    algos = ["DFS", "BFS", "A*-Manhattan", "A*-Euclidean", "Value Iteration", "Policy Iteration"]
    colors = ["#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c"]
    search_algos = algos[:4]
    search_colors = colors[:4]

    def _get_metric(algo, sizes, key, filter_solved=False):
        means, stds = [], []
        for size in sizes:
            rows = grouped.get((size, algo), [])
            if filter_solved:
                vals = [r[key] for r in rows if r["solved"]]
            else:
                vals = [r[key] for r in rows if r[key] != "N/A"]
            means.append(statistics.mean(vals) if vals else 0)
            stds.append(statistics.stdev(vals) if len(vals) > 1 else 0)
        return means, stds

    # Chart 1: Execution Time (all)
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(algos, colors):
        m, s = _get_metric(algo, sizes, "elapsed_time_s")
        ax.errorbar(sizes, m, yerr=s, marker="o", label=algo, color=color, capsize=3, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Execution Time (seconds)", fontsize=12)
    ax.set_title("Algorithm Execution Time vs Maze Size", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_time.png"), dpi=150); plt.close()

    # Chart 1b: Search-only time (ms)
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(search_algos, search_colors):
        m, _ = _get_metric(algo, sizes, "elapsed_time_s")
        ax.plot(sizes, [x * 1000 for x in m], marker="o", label=algo, color=color, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Execution Time (ms)", fontsize=12)
    ax.set_title("Search Algorithm Execution Time vs Maze Size", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_time_search_only.png"), dpi=150); plt.close()

    # Chart 2: Nodes explored (all)
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(algos, colors):
        m, _ = _get_metric(algo, sizes, "nodes_explored")
        ax.plot(sizes, m, marker="s", label=algo, color=color, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Nodes Explored / State-Action Evaluations", fontsize=12)
    ax.set_title("Nodes Explored vs Maze Size", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_explored.png"), dpi=150); plt.close()

    # Chart 2b: Nodes explored (search only)
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(search_algos, search_colors):
        m, _ = _get_metric(algo, sizes, "nodes_explored")
        ax.plot(sizes, m, marker="s", label=algo, color=color, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Nodes Explored", fontsize=12)
    ax.set_title("Nodes Explored vs Maze Size (Search Algorithms Only)", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_explored_search_only.png"), dpi=150); plt.close()

    # Chart 3: Path length
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(algos, colors):
        m, _ = _get_metric(algo, sizes, "path_length", filter_solved=True)
        ax.plot(sizes, m, marker="^", label=algo, color=color, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Path Length (grid steps)", fontsize=12)
    ax.set_title("Solution Path Length vs Maze Size", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_path_length.png"), dpi=150); plt.close()

    # Chart 4: Frontier (search)
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(search_algos, search_colors):
        m, _ = _get_metric(algo, sizes, "max_frontier")
        ax.plot(sizes, m, marker="D", label=algo, color=color, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Max Frontier Size", fontsize=12)
    ax.set_title("Maximum Frontier Size vs Maze Size (Search Algorithms)", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_frontier.png"), dpi=150); plt.close()

    # Chart 5: MDP iterations
    fig, ax = plt.subplots(figsize=(10, 6))
    for algo, color in zip(["Value Iteration", "Policy Iteration"], colors[4:]):
        m, _ = _get_metric(algo, sizes, "iterations")
        ax.plot(sizes, m, marker="o", label=algo, color=color, linewidth=2)
    ax.set_xlabel("Maze Size (NxN cells)", fontsize=12)
    ax.set_ylabel("Iterations to Convergence", fontsize=12)
    ax.set_title("MDP Iterations to Convergence vs Maze Size", fontsize=14)
    ax.legend(fontsize=10); ax.grid(True, alpha=0.3)
    plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, "comparison_mdp_iterations.png"), dpi=150); plt.close()

    print(f"  Core charts saved to: {RESULTS_DIR}/comparison_*.png")


def generate_sensitivity_charts(gamma_results, p_results):
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        return

    algo_styles = [("Value Iteration", "#9b59b6", "o"), ("Policy Iteration", "#1abc9c", "s")]

    def _panel_chart(data, param_name, param_values, metric_key, y_label, title_suffix, filename, sizes):
        fig, axes = plt.subplots(1, len(sizes), figsize=(5 * len(sizes), 5), sharey=False)
        if len(sizes) == 1:
            axes = [axes]
        for idx, size in enumerate(sizes):
            ax = axes[idx]
            for algo, color, marker in algo_styles:
                means, stds = [], []
                for pv in param_values:
                    rows = [r for r in data if r["maze_size"] == size and r[param_name] == pv and r["algorithm"] == algo]
                    vals = [r[metric_key] for r in rows if (r["solved"] if metric_key == "path_length" else True)]
                    means.append(statistics.mean(vals) if vals else 0)
                    stds.append(statistics.stdev(vals) if len(vals) > 1 else 0)
                ax.errorbar(param_values, means, yerr=stds, marker=marker, label=algo, color=color, capsize=3, linewidth=2)
            ax.set_xlabel(param_name, fontsize=11)
            ax.set_ylabel(y_label, fontsize=11)
            ax.set_title(f"{size}x{size} Maze", fontsize=12)
            ax.legend(fontsize=9); ax.grid(True, alpha=0.3)
        plt.suptitle(title_suffix, fontsize=14)
        plt.tight_layout(); plt.savefig(os.path.join(RESULTS_DIR, filename), dpi=150); plt.close()

    if gamma_results:
        _panel_chart(gamma_results, "gamma", GAMMA_VALUES, "iterations", "Iterations",
                     "Impact of Discount Factor (\u03b3) on Iterations to Convergence",
                     "sensitivity_gamma_iterations.png", SENSITIVITY_SIZES)
        _panel_chart(gamma_results, "gamma", GAMMA_VALUES, "elapsed_time_s", "Time (s)",
                     "Impact of Discount Factor (\u03b3) on Execution Time",
                     "sensitivity_gamma_time.png", SENSITIVITY_SIZES)
        print(f"  Gamma sensitivity charts saved.")

    if p_results:
        _panel_chart(p_results, "p_intended", P_INTENDED_VALUES, "iterations", "Iterations",
                     "Impact of Transition Stochasticity on Iterations",
                     "sensitivity_p_intended_iterations.png", SENSITIVITY_SIZES)
        _panel_chart(p_results, "p_intended", P_INTENDED_VALUES, "elapsed_time_s", "Time (s)",
                     "Impact of Transition Stochasticity on Execution Time",
                     "sensitivity_p_intended_time.png", SENSITIVITY_SIZES)
        _panel_chart(p_results, "p_intended", P_INTENDED_VALUES, "path_length", "Path Length",
                     "Impact of Transition Stochasticity on Path Length",
                     "sensitivity_p_intended_path.png", SENSITIVITY_SIZES)
        print(f"  Stochastic sensitivity charts saved.")



def main():
    parser = argparse.ArgumentParser(description="CS7IS2 Assignment 1 -  Maze Solver Comparison")
    parser.add_argument("--sizes", nargs="+", type=int, default=DEFAULT_SIZES,
                        help=f"Maze sizes (default: {DEFAULT_SIZES})")
    parser.add_argument("--runs", type=int, default=DEFAULT_RUNS,
                        help=f"Runs per size (default: {DEFAULT_RUNS})")
    parser.add_argument("--skip-sensitivity", action="store_true",
                        help="Skip MDP parameter sensitivity experiments")
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args()

    os.makedirs(RESULTS_DIR, exist_ok=True)

    print("=" * 62)
    print("  CS7IS2 Assignment 1 -  Maze Solver Comparison")
    print("=" * 62)
    print(f"  Maze sizes:       {args.sizes}")
    print(f"  Runs per size:    {args.runs}")
    print(f"  MDP defaults:     gamma={DEFAULT_MDP_PARAMS['gamma']}, theta={DEFAULT_MDP_PARAMS['theta']}")
    print(f"  Sensitivity:      {'SKIP' if args.skip_sensitivity else 'ON'}")
    if not args.skip_sensitivity:
        print(f"    Gamma values:   {GAMMA_VALUES}")
        print(f"    p_intended:     {P_INTENDED_VALUES}")
        print(f"    Sens. sizes:    {SENSITIVITY_SIZES}")
    print("=" * 62)

    # Experiment 1: Core comparison
    print("\n  EXPERIMENT 1: Core Algorithm Comparison")
    core_results = run_core_experiments(args.sizes, args.runs, verbose=not args.quiet)
    print("\n--- Exporting core results ---")
    export_csv(core_results, os.path.join(RESULTS_DIR, "results_raw.csv"))
    export_summary_csv(core_results, os.path.join(RESULTS_DIR, "results_summary.csv"))
    generate_core_charts(core_results)

    # Experiments 2 & 3: Sensitivity
    gamma_results = None
    p_results = None
    if not args.skip_sensitivity:
        gamma_results = run_gamma_sensitivity(SENSITIVITY_SIZES, SENSITIVITY_RUNS, verbose=not args.quiet)
        export_csv(gamma_results, os.path.join(RESULTS_DIR, "sensitivity_gamma.csv"))

        p_results = run_p_intended_sensitivity(SENSITIVITY_SIZES, SENSITIVITY_RUNS, verbose=not args.quiet)
        export_csv(p_results, os.path.join(RESULTS_DIR, "sensitivity_p_intended.csv"))

        print("\n--- Generating sensitivity charts ---")
        generate_sensitivity_charts(gamma_results, p_results)

    print("\n" + "=" * 62)
    print("  ALL EXPERIMENTS COMPLETE! Check 'results/' for outputs.")
    print("=" * 62)


if __name__ == "__main__":
    main()
