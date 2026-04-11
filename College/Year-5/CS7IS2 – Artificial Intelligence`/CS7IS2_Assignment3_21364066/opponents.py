import random
import numpy as np


class DefaultOpponent:
    # Semi-intelligent: tries to win, then block, then random.

    def __init__(self, game):
        self.game = game

    def select_action(self, state, player):
        valid_actions = self.game.get_valid_actions(state)
        opponent = -player

        # Try to win
        for action in valid_actions:
            next_state = self.game.make_move(state, action, player)
            done, winner = self.game.check_winner(next_state)
            if done and winner == player:
                return action

        # Try to block
        for action in valid_actions:
            next_state = self.game.make_move(state, action, opponent)
            done, winner = self.game.check_winner(next_state)
            if done and winner == opponent:
                return action

        # For Connect 4, sometimes prefer centre column
        if hasattr(self.game, 'win_length'):
            centre = self.game.cols // 2
            if centre in valid_actions:
                if random.random() < 0.3:
                    return centre

        return random.choice(valid_actions)


class RandomOpponent:

    def __init__(self, game):
        self.game = game

    def select_action(self, state, player):
        return random.choice(self.game.get_valid_actions(state))
