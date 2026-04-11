import argparse
import os
import time
import random
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

from games import TicTacToe, ConnectFour
from minimax_algorithms import MinimaxAgent, AlphaBetaAgent, scalability_test
from rl_algorithms import QLearningAgent, DQNAgent
from opponents import DefaultOpponent, RandomOpponent


RESULTS_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "results")
SEED = 42

FULL_CONFIG = {
    "ttt_eval_games": 100,
    "c4_eval_games": 20,
    "ttt_ql_episodes": 80000,
    "ttt_dqn_episodes": 10000,
    "c4_ql_episodes": 30000,
    "c4_dqn_episodes": 10000,
    "c4_minimax_depth": 4,
    "num_runs": 3,
    "scalability_time": 30,
    "h2h_games": 20,
}

QUICK_CONFIG = {
    "ttt_eval_games": 50,
    "c4_eval_games": 10,
    "ttt_ql_episodes": 10000,
    "ttt_dqn_episodes": 5000,
    "c4_ql_episodes": 10000,
    "c4_dqn_episodes": 5000,
    "c4_minimax_depth": 3,
    "num_runs": 2,
    "scalability_time": 20,
    "h2h_games": 10,
}


def set_seed(seed):
    random.seed(seed)
    np.random.seed(seed)



def play_game(game, agent1, agent2, agent1_player=1, verbose=False):
    state = game.reset()
    done = False
    move_count = 0
    time_a1, time_a2 = 0.0, 0.0
    current_player = 1

    while not done:
        if current_player == agent1_player:
            t0 = time.time()
            action = agent1.select_action(state, current_player)
            time_a1 += time.time() - t0
        else:
            t0 = time.time()
            action = agent2.select_action(state, current_player)
            time_a2 += time.time() - t0

        state = game.make_move(state, action, current_player)
        move_count += 1
        done, winner = game.check_winner(state)

        if verbose and done:
            game.display(state)

        current_player = -current_player

    return winner, move_count, time_a1, time_a2


def evaluate_matchup(game, agent1, agent2, num_games, agent1_name="Agent1",
                     agent2_name="Agent2", verbose=True):
    results = {
        "agent1_wins": 0, "agent2_wins": 0, "draws": 0,
        "agent1_total_time": 0.0, "agent2_total_time": 0.0,
        "total_moves": 0, "games_played": 0,
    }

    for i in range(num_games):
        a1_player = 1 if i % 2 == 0 else -1
        winner, moves, t1, t2 = play_game(game, agent1, agent2, a1_player)
        results["total_moves"] += moves
        results["agent1_total_time"] += t1
        results["agent2_total_time"] += t2
        results["games_played"] += 1

        if winner == a1_player:
            results["agent1_wins"] += 1
        elif winner == -a1_player:
            results["agent2_wins"] += 1
        else:
            results["draws"] += 1

    n = results["games_played"]
    results["agent1_win_rate"] = results["agent1_wins"] / n
    results["agent2_win_rate"] = results["agent2_wins"] / n
    results["draw_rate"] = results["draws"] / n
    results["avg_moves"] = results["total_moves"] / n
    results["agent1_avg_time"] = results["agent1_total_time"] / n
    results["agent2_avg_time"] = results["agent2_total_time"] / n

    if verbose:
        print(f"  {agent1_name} vs {agent2_name} ({n} games):")
        print(f"    {agent1_name} wins: {results['agent1_wins']} ({results['agent1_win_rate']:.1%})")
        print(f"    {agent2_name} wins: {results['agent2_wins']} ({results['agent2_win_rate']:.1%})")
        print(f"    Draws: {results['draws']} ({results['draw_rate']:.1%})")
        print(f"    Avg moves/game: {results['avg_moves']:.1f}")
        print(f"    Avg time/game: {agent1_name}={results['agent1_avg_time']*1000:.1f}ms, "
              f"{agent2_name}={results['agent2_avg_time']*1000:.1f}ms")

    return results



def plot_training_curves(rewards_list, wins_list, labels, title, filename, window=500):
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))

    for rewards, wins, label in zip(rewards_list, wins_list, labels):
        if len(rewards) >= window:
            smoothed = np.convolve(rewards, np.ones(window) / window, mode='valid')
            axes[0].plot(smoothed, label=label)
        else:
            axes[0].plot(rewards, label=label)

        if len(wins) >= window:
            win_rate = np.convolve(wins, np.ones(window) / window, mode='valid')
            axes[1].plot(win_rate, label=label)
        else:
            axes[1].plot(wins, label=label)

    axes[0].set_xlabel("Episode")
    axes[0].set_ylabel("Average Reward")
    axes[0].set_title(f"{title} - Training Reward")
    axes[0].legend()
    axes[0].grid(True, alpha=0.3)

    axes[1].set_xlabel("Episode")
    axes[1].set_ylabel(f"Win Rate (rolling {window})")
    axes[1].set_title(f"{title} - Training Win Rate")
    axes[1].legend()
    axes[1].grid(True, alpha=0.3)

    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, filename), dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {filename}")


def plot_comparison_bar(results_dict, title, filename):
    algorithms = list(results_dict.keys())
    wins = [results_dict[a]["win_rate"] for a in algorithms]
    draws = [results_dict[a]["draw_rate"] for a in algorithms]
    losses = [results_dict[a]["loss_rate"] for a in algorithms]

    x = np.arange(len(algorithms))
    width = 0.25
    fig, ax = plt.subplots(figsize=(10, 6))
    bars1 = ax.bar(x - width, wins, width, label='Win', color='#2ecc71')
    bars2 = ax.bar(x, draws, width, label='Draw', color='#f39c12')
    bars3 = ax.bar(x + width, losses, width, label='Loss', color='#e74c3c')

    ax.set_xlabel('Algorithm')
    ax.set_ylabel('Rate')
    ax.set_title(title)
    ax.set_xticks(x)
    ax.set_xticklabels(algorithms, rotation=15, ha='right')
    ax.legend()
    ax.set_ylim(0, 1.05)
    ax.grid(True, alpha=0.3, axis='y')

    for bars in [bars1, bars2, bars3]:
        for bar in bars:
            height = bar.get_height()
            if height > 0.01:
                ax.annotate(f'{height:.0%}', xy=(bar.get_x() + bar.get_width() / 2, height),
                           xytext=(0, 3), textcoords="offset points", ha='center', va='bottom',
                           fontsize=8)

    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, filename), dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {filename}")


def plot_time_comparison(time_dict, title, filename):
    algorithms = list(time_dict.keys())
    times_ms = [time_dict[a] * 1000 for a in algorithms]

    fig, ax = plt.subplots(figsize=(8, 5))
    bars = ax.bar(algorithms, times_ms, color=['#3498db', '#2ecc71', '#e74c3c', '#9b59b6'])

    ax.set_xlabel('Algorithm')
    ax.set_ylabel('Average Time per Game (ms)')
    ax.set_title(title)
    ax.grid(True, alpha=0.3, axis='y')

    for bar, t in zip(bars, times_ms):
        ax.annotate(f'{t:.1f}ms', xy=(bar.get_x() + bar.get_width() / 2, bar.get_height()),
                   xytext=(0, 3), textcoords="offset points", ha='center', va='bottom')

    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, filename), dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {filename}")


def plot_head_to_head_heatmap(matchup_matrix, agent_names, title, filename):
    n = len(agent_names)
    fig, ax = plt.subplots(figsize=(8, 6))

    im = ax.imshow(matchup_matrix, cmap='RdYlGn', vmin=0, vmax=1)

    ax.set_xticks(np.arange(n))
    ax.set_yticks(np.arange(n))
    ax.set_xticklabels(agent_names, rotation=45, ha='right')
    ax.set_yticklabels(agent_names)

    for i in range(n):
        for j in range(n):
            val = matchup_matrix[i][j]
            text = f"{val:.0%}" if not np.isnan(val) else "-"
            ax.text(j, i, text, ha="center", va="center",
                    color="black" if 0.3 < val < 0.7 else "white", fontsize=10)

    ax.set_xlabel("Opponent (columns)")
    ax.set_ylabel("Agent (rows)")
    ax.set_title(title)
    fig.colorbar(im, ax=ax, label="Win Rate")

    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, filename), dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {filename}")


def run_ttt_vs_default(config):
    print("\n" + "=" * 70)
    print("EXPERIMENT 1: Tic Tac Toe - Algorithms vs Default Opponent")
    print("=" * 70)

    game = TicTacToe()
    default_opp = DefaultOpponent(game)
    n_games = config["ttt_eval_games"]
    results_summary = {}

    # Minimax (full depth) vs Default
    print("\n[1a] Minimax (no pruning, full depth) vs Default Opponent")
    minimax = MinimaxAgent(game, max_depth=None, player=1)
    res = evaluate_matchup(game, minimax, default_opp, n_games, "Minimax", "Default")
    results_summary["Minimax"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    # Alpha-Beta (full depth) vs Default
    print("\n[1b] Alpha-Beta (full depth) vs Default Opponent")
    alphabeta = AlphaBetaAgent(game, max_depth=None, player=1)
    res = evaluate_matchup(game, alphabeta, default_opp, n_games, "AlphaBeta", "Default")
    results_summary["AlphaBeta"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    # Q-Learning vs Default
    print("\n[1c] Training Q-Learning agent...")
    ql_agent = QLearningAgent(game, player=1)
    ql_rewards, ql_wins = ql_agent.train(DefaultOpponent(game),
                                          num_episodes=config["ttt_ql_episodes"])
    print("  Evaluating Q-Learning vs Default Opponent...")
    res = evaluate_matchup(game, ql_agent, default_opp, n_games, "Q-Learning", "Default")
    results_summary["Q-Learning"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    # DQN vs Default
    print("\n[1d] Training DQN agent...")
    dqn_agent = DQNAgent(game, player=1)
    dqn_rewards, dqn_wins = dqn_agent.train(DefaultOpponent(game),
                                              num_episodes=config["ttt_dqn_episodes"])
    print("  Evaluating DQN vs Default Opponent...")
    res = evaluate_matchup(game, dqn_agent, default_opp, n_games, "DQN", "Default")
    results_summary["DQN"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    # Plots
    plot_training_curves([ql_rewards, dqn_rewards], [ql_wins, dqn_wins],
                        ["Q-Learning", "DQN"], "Tic Tac Toe", "ttt_training_curves.png")
    plot_comparison_bar(results_summary, "Tic Tac Toe: Algorithms vs Default Opponent",
                       "ttt_vs_default.png")
    plot_time_comparison({k: v["avg_time"] for k, v in results_summary.items()},
                        "Tic Tac Toe: Average Decision Time per Game", "ttt_time_comparison.png")

    return results_summary


def run_c4_vs_default(config):
    print("\n" + "=" * 70)
    print("EXPERIMENT 2: Connect 4 - Algorithms vs Default Opponent")
    print("=" * 70)

    game = ConnectFour()
    default_opp = DefaultOpponent(game)
    random_opp = RandomOpponent(game)
    n_games = config["c4_eval_games"]
    depth = config["c4_minimax_depth"]
    results_summary = {}

    print(f"\n[2a] Minimax (depth={depth}) vs Default Opponent")
    minimax = MinimaxAgent(game, max_depth=depth, player=1)
    res = evaluate_matchup(game, minimax, default_opp, n_games,
                          f"Minimax(d={depth})", "Default")
    results_summary[f"Minimax(d={depth})"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    print(f"\n[2b] Alpha-Beta (depth={depth}) vs Default Opponent")
    alphabeta = AlphaBetaAgent(game, max_depth=depth, player=1)
    res = evaluate_matchup(game, alphabeta, default_opp, n_games,
                          f"AlphaBeta(d={depth})", "Default")
    results_summary[f"AlphaBeta(d={depth})"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    print("\n[2c] Training Q-Learning agent (vs random opponent)...")
    ql_agent = QLearningAgent(game, player=1, epsilon_decay=0.99995)
    ql_rewards, ql_wins = ql_agent.train(random_opp,
                                          num_episodes=config["c4_ql_episodes"])
    print("  Evaluating Q-Learning vs Default Opponent...")
    res = evaluate_matchup(game, ql_agent, default_opp, n_games, "Q-Learning", "Default")
    results_summary["Q-Learning"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    print("\n[2d] Training DQN agent (vs random opponent)...")
    dqn_agent = DQNAgent(game, player=1, epsilon_decay=0.9997)
    dqn_rewards, dqn_wins = dqn_agent.train(random_opp,
                                              num_episodes=config["c4_dqn_episodes"])
    print("  Evaluating DQN vs Default Opponent...")
    res = evaluate_matchup(game, dqn_agent, default_opp, n_games, "DQN", "Default")
    results_summary["DQN"] = {
        "win_rate": res["agent1_win_rate"], "draw_rate": res["draw_rate"],
        "loss_rate": res["agent2_win_rate"], "avg_time": res["agent1_avg_time"],
    }

    # Plots
    plot_training_curves([ql_rewards, dqn_rewards], [ql_wins, dqn_wins],
                        ["Q-Learning", "DQN"], "Connect 4", "c4_training_curves.png")
    plot_comparison_bar(results_summary, "Connect 4: Algorithms vs Default Opponent",
                       "c4_vs_default.png")
    plot_time_comparison({k: v["avg_time"] for k, v in results_summary.items()},
                        "Connect 4: Average Decision Time per Game", "c4_time_comparison.png")

    return results_summary


def run_head_to_head(config):
    print("\n" + "=" * 70)
    print("EXPERIMENT 3: Head-to-Head Matchups")
    print("=" * 70)

    for game, game_name, ql_eps, dqn_eps, n_games, depth in [
        (TicTacToe(), "TicTacToe", config["ttt_ql_episodes"], config["ttt_dqn_episodes"],
         config["h2h_games"], None),
        (ConnectFour(), "ConnectFour", config["c4_ql_episodes"], config["c4_dqn_episodes"],
         config["h2h_games"], config["c4_minimax_depth"]),
    ]:
        print(f"\n{'='*50}")
        print(f"Head-to-Head: {game_name}")
        print(f"{'='*50}")

        if game_name == "TicTacToe":
            minimax = MinimaxAgent(game, max_depth=None, player=1)
            alphabeta = AlphaBetaAgent(game, max_depth=None, player=1)
            train_opp = DefaultOpponent(game)
        else:
            minimax = MinimaxAgent(game, max_depth=depth, player=1)
            alphabeta = AlphaBetaAgent(game, max_depth=depth, player=1)
            train_opp = RandomOpponent(game)

        print(f"  Training Q-Learning for {game_name}...")
        ql_agent = QLearningAgent(game, player=1,
                                   epsilon_decay=0.9995 if game_name == "TicTacToe" else 0.99995)
        ql_agent.train(train_opp, num_episodes=ql_eps, verbose=False)

        print(f"  Training DQN for {game_name}...")
        dqn_agent = DQNAgent(game, player=1,
                              epsilon_decay=0.9995 if game_name == "TicTacToe" else 0.9997)
        dqn_agent.train(train_opp, num_episodes=dqn_eps, verbose=False)

        if game_name == "TicTacToe":
            agents = {"Minimax": minimax, "AlphaBeta": alphabeta,
                      "Q-Learning": ql_agent, "DQN": dqn_agent}
        else:
            agents = {f"Minimax(d={depth})": minimax, f"AlphaBeta(d={depth})": alphabeta,
                      "Q-Learning": ql_agent, "DQN": dqn_agent}

        agent_names = list(agents.keys())
        n = len(agent_names)
        win_matrix = np.full((n, n), np.nan)

        for i in range(n):
            for j in range(n):
                if i == j:
                    continue
                print(f"\n  {agent_names[i]} vs {agent_names[j]}:")
                res = evaluate_matchup(game, agents[agent_names[i]], agents[agent_names[j]],
                                      n_games, agent_names[i], agent_names[j], verbose=True)
                win_matrix[i][j] = res["agent1_win_rate"]

        plot_head_to_head_heatmap(win_matrix, agent_names,
                                  f"{game_name}: Head-to-Head Win Rates",
                                  f"{game_name.lower()}_head_to_head.png")


def run_statistical_analysis(config):
    print("\n" + "=" * 70)
    print("EXPERIMENT 4: Statistical Analysis (Multiple Runs)")
    print("=" * 70)

    for game, game_name, ql_eps, dqn_eps, n_games, train_opp_cls in [
        (TicTacToe(), "TicTacToe", config["ttt_ql_episodes"],
         config["ttt_dqn_episodes"], config["ttt_eval_games"], DefaultOpponent),
        (ConnectFour(), "ConnectFour", config["c4_ql_episodes"],
         config["c4_dqn_episodes"], config["c4_eval_games"], RandomOpponent),
    ]:
        print(f"\n--- {game_name} ---")
        default_opp = DefaultOpponent(game)
        train_opp = train_opp_cls(game)

        # Q-Learning multi-run
        print(f"\n  Q-Learning multi-run evaluation ({game_name}):")
        ql_stats_list = []
        for run in range(config["num_runs"]):
            print(f"    Run {run + 1}/{config['num_runs']}")
            ql = QLearningAgent(game, player=1,
                                 epsilon_decay=0.9995 if game_name == "TicTacToe" else 0.99995)
            ql.train(train_opp, num_episodes=ql_eps, verbose=False)
            res = evaluate_matchup(game, ql, default_opp, n_games,
                                  "Q-Learning", "Default", verbose=False)
            ql_stats_list.append(res)

        ql_win_rates = [r["agent1_win_rate"] for r in ql_stats_list]
        print(f"  Q-Learning win rates: {[f'{x:.1%}' for x in ql_win_rates]}")
        print(f"  Mean: {np.mean(ql_win_rates):.1%}, Std: {np.std(ql_win_rates):.1%}")

        # DQN multi-run
        print(f"\n  DQN multi-run evaluation ({game_name}):")
        dqn_stats_list = []
        for run in range(config["num_runs"]):
            print(f"    Run {run + 1}/{config['num_runs']}")
            dqn = DQNAgent(game, player=1,
                            epsilon_decay=0.9995 if game_name == "TicTacToe" else 0.9997)
            dqn.train(train_opp, num_episodes=dqn_eps, verbose=False)
            res = evaluate_matchup(game, dqn, default_opp, n_games,
                                  "DQN", "Default", verbose=False)
            dqn_stats_list.append(res)

        dqn_win_rates = [r["agent1_win_rate"] for r in dqn_stats_list]
        print(f"  DQN win rates: {[f'{x:.1%}' for x in dqn_win_rates]}")
        print(f"  Mean: {np.mean(dqn_win_rates):.1%}, Std: {np.std(dqn_win_rates):.1%}")

        # Plot boxplot
        fig, ax = plt.subplots(figsize=(8, 5))
        data = [ql_win_rates, dqn_win_rates]
        bp = ax.boxplot(data, positions=[1, 2], widths=0.5, patch_artist=True)
        for patch, color in zip(bp['boxes'], ['#3498db', '#e74c3c']):
            patch.set_facecolor(color)
            patch.set_alpha(0.7)
        ax.set_xticklabels(['Q-Learning', 'DQN'])
        ax.set_ylabel('Win Rate vs Default Opponent')
        ax.set_title(f'{game_name}: RL Agent Win Rate Distribution ({config["num_runs"]} runs)')
        ax.grid(True, alpha=0.3, axis='y')
        ax.set_ylim(0, 1.05)
        plt.tight_layout()
        plt.savefig(os.path.join(RESULTS_DIR, f"{game_name.lower()}_multirun_boxplot.png"),
                   dpi=150, bbox_inches='tight')
        plt.close()
        print(f"  Saved: {game_name.lower()}_multirun_boxplot.png")


def run_hyperparameter_analysis(config):
    print("\n" + "=" * 70)
    print("EXPERIMENT 5: Hyperparameter Sensitivity Analysis")
    print("=" * 70)

    print("\n[5a] Connect 4: Minimax Depth Variation")
    c4 = ConnectFour()
    default_opp = DefaultOpponent(c4)
    depth_results = {}

    for depth in [3, 4, 5]:
        n_games = 10 if depth == 5 else 20  # depth 5 is slow
        print(f"\n  Testing depth={depth} ({n_games} games)...")

        mm = MinimaxAgent(c4, max_depth=depth, player=1)
        res = evaluate_matchup(c4, mm, default_opp, n_games,
                              f"Minimax(d={depth})", "Default")
        ab = AlphaBetaAgent(c4, max_depth=depth, player=1)
        res_ab = evaluate_matchup(c4, ab, default_opp, n_games,
                                  f"AlphaBeta(d={depth})", "Default")

        depth_results[depth] = {
            "mm_win_rate": res["agent1_win_rate"],
            "mm_time": res["agent1_avg_time"],
            "ab_win_rate": res_ab["agent1_win_rate"],
            "ab_time": res_ab["agent1_avg_time"],
            "mm_nodes": mm.nodes_visited,  # from last game
        }

    # Plot depth analysis
    depths = sorted(depth_results.keys())
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))

    # Win rates by depth
    mm_wins = [depth_results[d]["mm_win_rate"] for d in depths]
    ab_wins = [depth_results[d]["ab_win_rate"] for d in depths]
    x = np.arange(len(depths))
    width = 0.35
    axes[0].bar(x - width/2, mm_wins, width, label='Minimax', color='#3498db')
    axes[0].bar(x + width/2, ab_wins, width, label='Alpha-Beta', color='#2ecc71')
    axes[0].set_xlabel('Search Depth')
    axes[0].set_ylabel('Win Rate vs Default')
    axes[0].set_title('Connect 4: Win Rate by Search Depth')
    axes[0].set_xticks(x)
    axes[0].set_xticklabels([str(d) for d in depths])
    axes[0].legend()
    axes[0].set_ylim(0, 1.1)
    axes[0].grid(True, alpha=0.3, axis='y')

    # Time by depth
    mm_times = [depth_results[d]["mm_time"] * 1000 for d in depths]
    ab_times = [depth_results[d]["ab_time"] * 1000 for d in depths]
    axes[1].bar(x - width/2, mm_times, width, label='Minimax', color='#3498db')
    axes[1].bar(x + width/2, ab_times, width, label='Alpha-Beta', color='#2ecc71')
    axes[1].set_xlabel('Search Depth')
    axes[1].set_ylabel('Avg Time per Game (ms)')
    axes[1].set_title('Connect 4: Decision Time by Search Depth')
    axes[1].set_xticks(x)
    axes[1].set_xticklabels([str(d) for d in depths])
    axes[1].legend()
    axes[1].grid(True, alpha=0.3, axis='y')

    for i, (mt, at) in enumerate(zip(mm_times, ab_times)):
        axes[1].annotate(f'{mt:.0f}', xy=(x[i] - width/2, mt), xytext=(0, 3),
                        textcoords="offset points", ha='center', fontsize=8)
        axes[1].annotate(f'{at:.0f}', xy=(x[i] + width/2, at), xytext=(0, 3),
                        textcoords="offset points", ha='center', fontsize=8)

    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, "c4_depth_variation.png"), dpi=150, bbox_inches='tight')
    plt.close()
    print("  Saved: c4_depth_variation.png")

    # Print depth results table
    print("\n  --- C4 Depth Variation Results ---")
    print(f"  {'Depth':<8} {'MM Win%':<10} {'MM Time':<12} {'AB Win%':<10} {'AB Time':<12}")
    for d in depths:
        r = depth_results[d]
        print(f"  {d:<8} {r['mm_win_rate']:<10.1%} {r['mm_time']*1000:<12.1f}ms "
              f"{r['ab_win_rate']:<10.1%} {r['ab_time']*1000:<12.1f}ms")

    print("\n[5b] TTT: Q-Learning Training Episode Variation")
    ttt = TicTacToe()
    default_opp = DefaultOpponent(ttt)

    episode_counts = [10000, 30000, 80000]
    episode_results = {}

    for eps in episode_counts:
        print(f"\n  Training Q-Learning for {eps} episodes...")
        ql = QLearningAgent(ttt, player=1)
        ql.train(DefaultOpponent(ttt), num_episodes=eps, verbose=False)
        res = evaluate_matchup(ttt, ql, default_opp, config["ttt_eval_games"],
                              f"QL({eps//1000}k)", "Default")
        episode_results[eps] = {
            "win_rate": res["agent1_win_rate"],
            "draw_rate": res["draw_rate"],
            "loss_rate": res["agent2_win_rate"],
            "q_table_size": len(ql.q_table),
        }

    # Plot episode variation
    fig, ax = plt.subplots(figsize=(8, 5))
    eps_labels = [f"{e//1000}k" for e in episode_counts]
    w_rates = [episode_results[e]["win_rate"] for e in episode_counts]
    d_rates = [episode_results[e]["draw_rate"] for e in episode_counts]
    l_rates = [episode_results[e]["loss_rate"] for e in episode_counts]

    x = np.arange(len(eps_labels))
    width = 0.25
    ax.bar(x - width, w_rates, width, label='Win', color='#2ecc71')
    ax.bar(x, d_rates, width, label='Draw', color='#f39c12')
    ax.bar(x + width, l_rates, width, label='Loss', color='#e74c3c')
    ax.set_xlabel('Training Episodes')
    ax.set_ylabel('Rate')
    ax.set_title('TTT Q-Learning: Impact of Training Episodes')
    ax.set_xticks(x)
    ax.set_xticklabels(eps_labels)
    ax.legend()
    ax.set_ylim(0, 1.1)
    ax.grid(True, alpha=0.3, axis='y')
    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, "ttt_ql_episode_variation.png"), dpi=150, bbox_inches='tight')
    plt.close()
    print("  Saved: ttt_ql_episode_variation.png")

    print("\n  --- TTT Q-Learning Episode Variation ---")
    print(f"  {'Episodes':<12} {'Win%':<10} {'Draw%':<10} {'Loss%':<10} {'Q-Table':<12}")
    for e in episode_counts:
        r = episode_results[e]
        print(f"  {e:<12} {r['win_rate']:<10.1%} {r['draw_rate']:<10.1%} "
              f"{r['loss_rate']:<10.1%} {r['q_table_size']:<12,}")

    print("\n[5c] TTT: Q-Learning Learning Rate Variation")
    lr_values = [0.05, 0.1, 0.2]
    lr_results = {}

    for lr in lr_values:
        print(f"\n  Training Q-Learning with lr={lr}...")
        ql = QLearningAgent(ttt, player=1, learning_rate=lr)
        ql.train(DefaultOpponent(ttt), num_episodes=config["ttt_ql_episodes"], verbose=False)
        res = evaluate_matchup(ttt, ql, default_opp, config["ttt_eval_games"],
                              f"QL(lr={lr})", "Default")
        lr_results[lr] = {
            "win_rate": res["agent1_win_rate"],
            "draw_rate": res["draw_rate"],
            "loss_rate": res["agent2_win_rate"],
        }

    # Plot learning rate variation
    fig, ax = plt.subplots(figsize=(8, 5))
    lr_labels = [str(lr) for lr in lr_values]
    w_rates = [lr_results[lr]["win_rate"] for lr in lr_values]
    d_rates = [lr_results[lr]["draw_rate"] for lr in lr_values]
    l_rates = [lr_results[lr]["loss_rate"] for lr in lr_values]

    x = np.arange(len(lr_labels))
    width = 0.25
    ax.bar(x - width, w_rates, width, label='Win', color='#2ecc71')
    ax.bar(x, d_rates, width, label='Draw', color='#f39c12')
    ax.bar(x + width, l_rates, width, label='Loss', color='#e74c3c')
    ax.set_xlabel('Learning Rate')
    ax.set_ylabel('Rate')
    ax.set_title('TTT Q-Learning: Impact of Learning Rate')
    ax.set_xticks(x)
    ax.set_xticklabels(lr_labels)
    ax.legend()
    ax.set_ylim(0, 1.1)
    ax.grid(True, alpha=0.3, axis='y')
    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, "ttt_ql_lr_variation.png"), dpi=150, bbox_inches='tight')
    plt.close()
    print("  Saved: ttt_ql_lr_variation.png")

    print("\n  --- TTT Q-Learning Learning Rate Variation ---")
    print(f"  {'LR':<10} {'Win%':<10} {'Draw%':<10} {'Loss%':<10}")
    for lr in lr_values:
        r = lr_results[lr]
        print(f"  {lr:<10} {r['win_rate']:<10.1%} {r['draw_rate']:<10.1%} {r['loss_rate']:<10.1%}")


def create_overall_summary(ttt_results, c4_results, config):
    print("\n" + "=" * 70)
    print("OVERALL SUMMARY")
    print("=" * 70)

    fig, axes = plt.subplots(1, 2, figsize=(16, 6))

    for ax, data, title in [
        (axes[0], ttt_results, "Tic Tac Toe vs Default Opponent"),
        (axes[1], c4_results, "Connect 4 vs Default Opponent"),
    ]:
        if data:
            algs = list(data.keys())
            wins = [data[a]["win_rate"] for a in algs]
            draws = [data[a]["draw_rate"] for a in algs]
            losses = [data[a]["loss_rate"] for a in algs]

            x = np.arange(len(algs))
            width = 0.25
            ax.bar(x - width, wins, width, label='Win', color='#2ecc71')
            ax.bar(x, draws, width, label='Draw', color='#f39c12')
            ax.bar(x + width, losses, width, label='Loss', color='#e74c3c')
            ax.set_xticks(x)
            ax.set_xticklabels(algs, rotation=15, ha='right')
            ax.set_title(title)
            ax.set_ylim(0, 1.05)
            ax.legend()
            ax.grid(True, alpha=0.3, axis='y')

    plt.suptitle("Overall Algorithm Comparison", fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.savefig(os.path.join(RESULTS_DIR, "overall_summary.png"), dpi=150, bbox_inches='tight')
    plt.close()
    print("  Saved: overall_summary.png")

    # Print text summary table
    print("\n--- Results Table ---")
    print(f"{'Game':<15} {'Algorithm':<20} {'Win%':<10} {'Draw%':<10} {'Loss%':<10} {'Avg Time(ms)':<12}")
    print("-" * 77)
    if ttt_results:
        for alg, r in ttt_results.items():
            print(f"{'TTT':<15} {alg:<20} {r['win_rate']:<10.1%} {r['draw_rate']:<10.1%} "
                  f"{r['loss_rate']:<10.1%} {r['avg_time']*1000:<12.1f}")
    if c4_results:
        for alg, r in c4_results.items():
            print(f"{'Connect4':<15} {alg:<20} {r['win_rate']:<10.1%} {r['draw_rate']:<10.1%} "
                  f"{r['loss_rate']:<10.1%} {r['avg_time']*1000:<12.1f}")


def main():
    parser = argparse.ArgumentParser(description="CS7IS2 Assignment 3 - Game AI Experiments")
    parser.add_argument("--quick", action="store_true",
                       help="Quick run with fewer episodes (for testing)")
    parser.add_argument("--scalability", action="store_true",
                       help="Run only the Connect 4 scalability test")
    args = parser.parse_args()

    config = QUICK_CONFIG if args.quick else FULL_CONFIG

    os.makedirs(RESULTS_DIR, exist_ok=True)
    set_seed(SEED)

    start_time = time.time()

    print("=" * 70)
    print("CS7IS2 Assignment 3 - Minimax and RL for Game Playing")
    print(f"Configuration: {'QUICK' if args.quick else 'FULL'}")
    print("=" * 70)

    if args.scalability:
        scalability_test(ConnectFour(), config["scalability_time"])
        return

    # Experiment 1: TTT vs Default
    ttt_results = run_ttt_vs_default(config)

    # Scalability test + Experiment 2: C4 vs Default
    scalability_test(ConnectFour(), config["scalability_time"])
    c4_results = run_c4_vs_default(config)

    # Experiment 3: Head-to-Head
    run_head_to_head(config)

    # Experiment 4: Statistical analysis (BOTH games)
    run_statistical_analysis(config)

    # Experiment 5: Hyperparameter sensitivity
    run_hyperparameter_analysis(config)

    # Overall summary
    create_overall_summary(ttt_results, c4_results, config)

    elapsed = time.time() - start_time
    print(f"\nTotal runtime: {elapsed:.1f}s ({elapsed / 60:.1f} minutes)")
    print(f"Results saved to: {RESULTS_DIR}")
    print("\nAll experiments complete.")


if __name__ == "__main__":
    main()
