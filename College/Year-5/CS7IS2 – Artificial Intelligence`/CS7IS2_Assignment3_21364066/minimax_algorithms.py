import math
import time
import numpy as np


def evaluate_tictactoe(state, game, player):
    # Heuristic: score based on how many lines each player threatens
    score = 0
    lines = []

    # Rows
    for r in range(3):
        lines.append([state[r][c] for c in range(3)])
    # Columns
    for c in range(3):
        lines.append([state[r][c] for r in range(3)])
    # Diagonals
    lines.append([state[i][i] for i in range(3)])
    lines.append([state[i][2 - i] for i in range(3)])

    for line in lines:
        player_count = line.count(player)
        opponent_count = line.count(-player)

        if opponent_count == 0:
            if player_count == 3:
                score += 100
            elif player_count == 2:
                score += 10
            elif player_count == 1:
                score += 1
        if player_count == 0:
            if opponent_count == 3:
                score -= 100
            elif opponent_count == 2:
                score -= 10
            elif opponent_count == 1:
                score -= 1

    return score


def evaluate_connect4(state, game, player):
    rows, cols, win_len = game.rows, game.cols, game.win_length
    opponent = -player
    score = 0

    def score_window(window):
        s = 0
        p_count = np.count_nonzero(window == player)
        o_count = np.count_nonzero(window == opponent)
        empty = np.count_nonzero(window == 0)

        if p_count == win_len:
            s += 1000
        elif p_count == win_len - 1 and empty == 1:
            s += 50
        elif p_count == win_len - 2 and empty == 2:
            s += 5

        if o_count == win_len:
            s -= 1000
        elif o_count == win_len - 1 and empty == 1:
            s -= 50

        return s

    centre_col = cols // 2
    centre_array = state[:, centre_col]
    centre_count = np.count_nonzero(centre_array == player)
    score += centre_count * 3

    for r in range(rows):
        for c in range(cols - win_len + 1):
            window = state[r, c:c + win_len]
            score += score_window(window)

    for r in range(rows - win_len + 1):
        for c in range(cols):
            window = state[r:r + win_len, c]
            score += score_window(window)

    for r in range(rows - win_len + 1):
        for c in range(cols - win_len + 1):
            window = np.array([state[r + i][c + i] for i in range(win_len)])
            score += score_window(window)

    for r in range(win_len - 1, rows):
        for c in range(cols - win_len + 1):
            window = np.array([state[r - i][c + i] for i in range(win_len)])
            score += score_window(window)

    return score


def get_eval_function(game):
    if game.name == "TicTacToe":
        return evaluate_tictactoe
    else:
        return evaluate_connect4


class MinimaxAgent:
    # Standard minimax with transposition table. Supports depth limiting.

    def __init__(self, game, max_depth=None, player=1):
        self.game = game
        self.max_depth = max_depth
        self.player = player
        self.eval_fn = get_eval_function(game)
        self.nodes_visited = 0
        self.transposition_table = {}

    def select_action(self, state, player=None):
        if player is not None:
            self.player = player
        self.nodes_visited = 0
        self.transposition_table = {}

        valid_actions = self.game.get_valid_actions(state)
        best_action = valid_actions[0]
        best_value = -math.inf

        for action in valid_actions:
            next_state = self.game.make_move(state, action, self.player)
            value = self._min_value(next_state, depth=1)
            if value > best_value:
                best_value = value
                best_action = action

        return best_action

    def _max_value(self, state, depth):
        self.nodes_visited += 1

        state_key = (self.game.state_to_tuple(state), True, depth)
        if state_key in self.transposition_table:
            return self.transposition_table[state_key]

        done, winner = self.game.check_winner(state)
        if done:
            if winner == self.player:
                val = 1000 - depth
            elif winner == -self.player:
                val = -1000 + depth
            else:
                val = 0
            self.transposition_table[state_key] = val
            return val

        if self.max_depth is not None and depth >= self.max_depth:
            val = self.eval_fn(state, self.game, self.player)
            self.transposition_table[state_key] = val
            return val

        value = -math.inf
        for action in self.game.get_valid_actions(state):
            next_state = self.game.make_move(state, action, self.player)
            value = max(value, self._min_value(next_state, depth + 1))

        self.transposition_table[state_key] = value
        return value

    def _min_value(self, state, depth):
        self.nodes_visited += 1

        state_key = (self.game.state_to_tuple(state), False, depth)
        if state_key in self.transposition_table:
            return self.transposition_table[state_key]

        done, winner = self.game.check_winner(state)
        if done:
            if winner == self.player:
                val = 1000 - depth
            elif winner == -self.player:
                val = -1000 + depth
            else:
                val = 0
            self.transposition_table[state_key] = val
            return val

        if self.max_depth is not None and depth >= self.max_depth:
            val = self.eval_fn(state, self.game, self.player)
            self.transposition_table[state_key] = val
            return val

        value = math.inf
        for action in self.game.get_valid_actions(state):
            next_state = self.game.make_move(state, action, -self.player)
            value = min(value, self._max_value(next_state, depth + 1))

        self.transposition_table[state_key] = value
        return value


class AlphaBetaAgent:
    # Minimax with alpha-beta pruning. Supports depth limiting.

    def __init__(self, game, max_depth=None, player=1):
        self.game = game
        self.max_depth = max_depth
        self.player = player
        self.eval_fn = get_eval_function(game)
        self.nodes_visited = 0

    def select_action(self, state, player=None):
        if player is not None:
            self.player = player
        self.nodes_visited = 0

        valid_actions = self.game.get_valid_actions(state)
        best_action = valid_actions[0]
        best_value = -math.inf
        alpha = -math.inf
        beta = math.inf

        for action in valid_actions:
            next_state = self.game.make_move(state, action, self.player)
            value = self._min_value(next_state, depth=1, alpha=alpha, beta=beta)
            if value > best_value:
                best_value = value
                best_action = action
            alpha = max(alpha, best_value)

        return best_action

    def _max_value(self, state, depth, alpha, beta):
        self.nodes_visited += 1

        done, winner = self.game.check_winner(state)
        if done:
            if winner == self.player:
                return 1000 - depth
            elif winner == -self.player:
                return -1000 + depth
            else:
                return 0

        if self.max_depth is not None and depth >= self.max_depth:
            return self.eval_fn(state, self.game, self.player)

        value = -math.inf
        for action in self.game.get_valid_actions(state):
            next_state = self.game.make_move(state, action, self.player)
            value = max(value, self._min_value(next_state, depth + 1, alpha, beta))
            if value >= beta:
                return value
            alpha = max(alpha, value)
        return value

    def _min_value(self, state, depth, alpha, beta):
        self.nodes_visited += 1

        done, winner = self.game.check_winner(state)
        if done:
            if winner == self.player:
                return 1000 - depth
            elif winner == -self.player:
                return -1000 + depth
            else:
                return 0

        if self.max_depth is not None and depth >= self.max_depth:
            return self.eval_fn(state, self.game, self.player)

        value = math.inf
        for action in self.game.get_valid_actions(state):
            next_state = self.game.make_move(state, action, -self.player)
            value = min(value, self._max_value(next_state, depth + 1, alpha, beta))
            if value <= alpha:
                return value
            beta = min(beta, value)
        return value


def scalability_test(game, time_limit_seconds=60):
    print(f"\n{'='*60}")
    print("SCALABILITY TEST: Connect 4 Full-Depth Minimax")
    print(f"Time limit: {time_limit_seconds} seconds")
    print(f"Board size: {game.rows}x{game.cols}, Win length: {game.win_length}")
    print(f"Max possible moves: {game.rows * game.cols}")
    print(f"{'='*60}")

    state = game.reset()
    results = {}

    for name, AgentClass in [("Minimax (no pruning)", MinimaxAgent),
                              ("Minimax (alpha-beta)", AlphaBetaAgent)]:
        print(f"\n--- {name} ---")
        total_nodes = 0
        max_depth_reached = 0
        start_time = time.time()

        for depth in range(1, game.rows * game.cols + 1):
            if time.time() - start_time > time_limit_seconds:
                break
            temp_agent = AgentClass(game, max_depth=depth, player=1)
            try:
                temp_agent.select_action(state)
                total_nodes += temp_agent.nodes_visited
                max_depth_reached = depth
                elapsed = time.time() - start_time
                print(f"  Depth {depth}: {temp_agent.nodes_visited:,} nodes "
                      f"({elapsed:.1f}s elapsed)")
                if elapsed > time_limit_seconds:
                    break
            except Exception:
                break

        elapsed = time.time() - start_time
        print(f"  Total nodes visited in {elapsed:.1f}s: {total_nodes:,}")
        print(f"  Maximum depth reached: {max_depth_reached} / {game.rows * game.cols}")
        results[name] = {
            "total_nodes": total_nodes,
            "max_depth": max_depth_reached,
            "time": elapsed
        }

    return results
