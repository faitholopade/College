import numpy as np
import copy


class TicTacToe:
    # Players: 1 (X) and -1 (O). Actions 0-8 (row-major).

    def __init__(self):
        self.rows = 3
        self.cols = 3
        self.name = "TicTacToe"

    def reset(self):
        return np.zeros((self.rows, self.cols), dtype=int)

    def get_valid_actions(self, state):
        return [i for i in range(self.rows * self.cols) if state[i // self.cols][i % self.cols] == 0]

    def make_move(self, state, action, player):
        new_state = state.copy()
        row, col = action // self.cols, action % self.cols
        if new_state[row][col] != 0:
            raise ValueError(f"Invalid move: position {action} is already occupied.")
        new_state[row][col] = player
        return new_state

    def check_winner(self, state):
        # Returns (done, winner). winner: 1, -1, or 0 for draw
        for player in [1, -1]:
            for r in range(self.rows):
                if all(state[r][c] == player for c in range(self.cols)):
                    return True, player
            for c in range(self.cols):
                if all(state[r][c] == player for r in range(self.rows)):
                    return True, player
            if all(state[i][i] == player for i in range(self.rows)):
                return True, player
            if all(state[i][self.cols - 1 - i] == player for i in range(self.rows)):
                return True, player

        if len(self.get_valid_actions(state)) == 0:
            return True, 0

        return False, 0

    def state_to_tuple(self, state):
        return tuple(state.flatten())

    def clone_state(self, state):
        return state.copy()

    def display(self, state):
        symbols = {0: ".", 1: "X", -1: "O"}
        print("  " + " ".join(str(c) for c in range(self.cols)))
        for r in range(self.rows):
            print(f"{r} " + " ".join(symbols[state[r][c]] for c in range(self.cols)))
        print()


class ConnectFour:
    # 6x7 board. Players: 1 (X) and -1 (O). Actions: column index (0-6).

    def __init__(self, rows=6, cols=7, win_length=4):
        self.rows = rows
        self.cols = cols
        self.win_length = win_length
        self.name = "ConnectFour"

    def reset(self):
        return np.zeros((self.rows, self.cols), dtype=int)

    def get_valid_actions(self, state):
        return [c for c in range(self.cols) if state[0][c] == 0]

    def make_move(self, state, action, player):
        new_state = state.copy()
        for r in range(self.rows - 1, -1, -1):
            if new_state[r][action] == 0:
                new_state[r][action] = player
                return new_state
        raise ValueError(f"Invalid move: column {action} is full.")

    def check_winner(self, state):
        for player in [1, -1]:
            # Horizontal
            for r in range(self.rows):
                for c in range(self.cols - self.win_length + 1):
                    if all(state[r][c + i] == player for i in range(self.win_length)):
                        return True, player
            # Vertical
            for r in range(self.rows - self.win_length + 1):
                for c in range(self.cols):
                    if all(state[r + i][c] == player for i in range(self.win_length)):
                        return True, player
            # Diagonal down-right
            for r in range(self.rows - self.win_length + 1):
                for c in range(self.cols - self.win_length + 1):
                    if all(state[r + i][c + i] == player for i in range(self.win_length)):
                        return True, player
            # Diagonal up-right
            for r in range(self.win_length - 1, self.rows):
                for c in range(self.cols - self.win_length + 1):
                    if all(state[r - i][c + i] == player for i in range(self.win_length)):
                        return True, player

        if len(self.get_valid_actions(state)) == 0:
            return True, 0

        return False, 0

    def state_to_tuple(self, state):
        return tuple(state.flatten())

    def clone_state(self, state):
        return state.copy()

    def display(self, state):
        symbols = {0: ".", 1: "X", -1: "O"}
        print("  " + " ".join(str(c) for c in range(self.cols)))
        for r in range(self.rows):
            print(f"{r} " + " ".join(symbols[state[r][c]] for c in range(self.cols)))
        print()
