import random
import math
import numpy as np
from collections import defaultdict, deque


class QLearningAgent:
    # Tabular Q-Learning with epsilon-greedy exploration.

    def __init__(self, game, player=1, learning_rate=0.1, discount_factor=0.95,
                 epsilon_start=1.0, epsilon_end=0.01, epsilon_decay=0.9995):
        self.game = game
        self.player = player
        self.lr = learning_rate
        self.gamma = discount_factor
        self.epsilon = epsilon_start
        self.epsilon_start = epsilon_start
        self.epsilon_end = epsilon_end
        self.epsilon_decay = epsilon_decay

        self.q_table = defaultdict(float)

        self.training_rewards = []
        self.training_wins = []
        self.training_epsilon = []

    def _get_q_value(self, state, action):
        state_key = self.game.state_to_tuple(state)
        return self.q_table[(state_key, action)]

    def _get_max_q(self, state):
        valid_actions = self.game.get_valid_actions(state)
        if not valid_actions:
            return 0.0
        return max(self._get_q_value(state, a) for a in valid_actions)

    def _update_q(self, state, action, reward, next_state, done):
        state_key = self.game.state_to_tuple(state)
        old_q = self.q_table[(state_key, action)]

        if done:
            target = reward
        else:
            target = reward + self.gamma * self._get_max_q(next_state)

        self.q_table[(state_key, action)] = old_q + self.lr * (target - old_q)

    def select_action(self, state, player=None, training=False):
        if player is not None:
            self.player = player

        valid_actions = self.game.get_valid_actions(state)
        if not valid_actions:
            return None

        if training and random.random() < self.epsilon:
            return random.choice(valid_actions)

        best_value = -math.inf
        best_actions = []
        for action in valid_actions:
            q_val = self._get_q_value(state, action)
            if q_val > best_value:
                best_value = q_val
                best_actions = [action]
            elif q_val == best_value:
                best_actions.append(action)

        return random.choice(best_actions)

    def train(self, opponent, num_episodes=50000, verbose=True):
        self.epsilon = self.epsilon_start
        win_count = 0
        window_size = 500

        for episode in range(num_episodes):
            state = self.game.reset()
            done = False
            episode_reward = 0
            winner = 0

            agent_turn = random.choice([True, False])

            if not agent_turn:
                opp_action = opponent.select_action(state, -self.player)
                state = self.game.make_move(state, opp_action, -self.player)
                done, winner = self.game.check_winner(state)
                agent_turn = True

            while not done:
                action = self.select_action(state, training=True)
                next_state = self.game.make_move(state, action, self.player)
                done, winner = self.game.check_winner(next_state)

                if done:
                    if winner == self.player:
                        reward = 1.0
                        win_count += 1
                    elif winner == -self.player:
                        reward = -1.0
                    else:
                        reward = 0.3
                    self._update_q(state, action, reward, next_state, done)
                    episode_reward += reward
                    break

                # Opponent's turn
                opp_action = opponent.select_action(next_state, -self.player)
                after_opp_state = self.game.make_move(next_state, opp_action, -self.player)
                done, winner = self.game.check_winner(after_opp_state)

                if done:
                    if winner == self.player:
                        reward = 1.0
                        win_count += 1
                    elif winner == -self.player:
                        reward = -1.0
                    else:
                        reward = 0.3
                    self._update_q(state, action, reward, after_opp_state, done)
                    episode_reward += reward
                    break

                self._update_q(state, action, 0.0, after_opp_state, False)
                state = after_opp_state

            self.epsilon = max(self.epsilon_end, self.epsilon * self.epsilon_decay)

            self.training_rewards.append(episode_reward)
            self.training_wins.append(1 if (done and winner == self.player) else 0)
            self.training_epsilon.append(self.epsilon)

            if verbose and (episode + 1) % (num_episodes // 10) == 0:
                recent_wins = sum(self.training_wins[-window_size:])
                recent_total = min(window_size, episode + 1)
                win_rate = recent_wins / recent_total
                print(f"  Episode {episode + 1}/{num_episodes} | "
                      f"Win rate (last {recent_total}): {win_rate:.2%} | "
                      f"Epsilon: {self.epsilon:.4f} | "
                      f"Q-table size: {len(self.q_table):,}")

        if verbose:
            total_wins = sum(self.training_wins)
            print(f"  Training complete. Total wins: {total_wins}/{num_episodes} "
                  f"({total_wins / num_episodes:.2%})")

        return self.training_rewards, self.training_wins


class DQNAgent:
    # DQN with numpy neural net, experience replay, and target network.
    # Architecture: Input -> 128 (ReLU) -> 64 (ReLU) -> Output

    def __init__(self, game, player=1, learning_rate=0.001, discount_factor=0.95,
                 epsilon_start=1.0, epsilon_end=0.01, epsilon_decay=0.9995,
                 batch_size=64, buffer_size=50000, target_update_freq=50,
                 tau=0.01):
        self.game = game
        self.player = player
        self.lr = learning_rate
        self.gamma = discount_factor
        self.epsilon = epsilon_start
        self.epsilon_start = epsilon_start
        self.epsilon_end = epsilon_end
        self.epsilon_decay = epsilon_decay
        self.batch_size = batch_size
        self.target_update_freq = target_update_freq
        self.tau = tau

        self.input_size = game.rows * game.cols
        self.hidden1_size = 128
        self.hidden2_size = 64
        if game.name == "TicTacToe":
            self.num_actions = game.rows * game.cols  # 9
        else:
            self.num_actions = game.cols  # 7

        self.weights = self._init_weights()
        self.target_weights = self._copy_weights(self.weights)

        self.adam_m = {k: np.zeros_like(v) for k, v in self.weights.items()}
        self.adam_v = {k: np.zeros_like(v) for k, v in self.weights.items()}
        self.adam_t = 0
        self.adam_beta1 = 0.9
        self.adam_beta2 = 0.999
        self.adam_eps = 1e-8

        self.replay_buffer = deque(maxlen=buffer_size)

        self.training_rewards = []
        self.training_wins = []
        self.training_epsilon = []
        self.training_losses = []

    def _init_weights(self):
        # Xavier init
        np.random.seed(None)
        weights = {
            'W1': np.random.randn(self.input_size, self.hidden1_size) * np.sqrt(2.0 / (self.input_size + self.hidden1_size)),
            'b1': np.zeros(self.hidden1_size),
            'W2': np.random.randn(self.hidden1_size, self.hidden2_size) * np.sqrt(2.0 / (self.hidden1_size + self.hidden2_size)),
            'b2': np.zeros(self.hidden2_size),
            'W3': np.random.randn(self.hidden2_size, self.num_actions) * np.sqrt(2.0 / (self.hidden2_size + self.num_actions)),
            'b3': np.zeros(self.num_actions),
        }
        return weights

    def _copy_weights(self, weights):
        return {k: v.copy() for k, v in weights.items()}

    def _soft_update_target(self):
        for key in self.weights:
            self.target_weights[key] = (self.tau * self.weights[key] +
                                         (1 - self.tau) * self.target_weights[key])

    def _forward(self, x, weights):
        z1 = x @ weights['W1'] + weights['b1']
        a1 = np.maximum(0, z1)

        z2 = a1 @ weights['W2'] + weights['b2']
        a2 = np.maximum(0, z2)

        q_values = a2 @ weights['W3'] + weights['b3']

        cache = {'x': x, 'z1': z1, 'a1': a1, 'z2': z2, 'a2': a2}
        return q_values, cache

    def _backward_huber(self, q_values, targets, cache, actions):
        batch_size = q_values.shape[0]

        dq = np.zeros_like(q_values)
        for i in range(batch_size):
            diff = q_values[i, actions[i]] - targets[i]
            dq[i, actions[i]] = np.clip(diff, -1.0, 1.0) / batch_size

        dW3 = cache['a2'].T @ dq
        db3 = np.sum(dq, axis=0)
        da2 = dq @ self.weights['W3'].T
        da2 = da2 * (cache['z2'] > 0)

        dW2 = cache['a1'].T @ da2
        db2 = np.sum(da2, axis=0)
        da1 = da2 @ self.weights['W2'].T
        da1 = da1 * (cache['z1'] > 0)

        dW1 = cache['x'].T @ da1
        db1 = np.sum(da1, axis=0)

        grads = {'W1': dW1, 'b1': db1, 'W2': dW2, 'b2': db2, 'W3': dW3, 'b3': db3}

        # Gradient norm clipping
        total_norm = 0
        for key in grads:
            total_norm += np.sum(grads[key] ** 2)
        total_norm = np.sqrt(total_norm)
        max_norm = 1.0
        if total_norm > max_norm:
            scale = max_norm / (total_norm + 1e-8)
            for key in grads:
                grads[key] *= scale

        return grads

    def _adam_update(self, grads):
        self.adam_t += 1
        for key in self.weights:
            self.adam_m[key] = self.adam_beta1 * self.adam_m[key] + (1 - self.adam_beta1) * grads[key]
            self.adam_v[key] = self.adam_beta2 * self.adam_v[key] + (1 - self.adam_beta2) * (grads[key] ** 2)

            m_hat = self.adam_m[key] / (1 - self.adam_beta1 ** self.adam_t)
            v_hat = self.adam_v[key] / (1 - self.adam_beta2 ** self.adam_t)

            self.weights[key] -= self.lr * m_hat / (np.sqrt(v_hat) + self.adam_eps)

    def _state_to_input(self, state):
        return state.flatten().astype(np.float32)

    def _get_q_values(self, state, use_target=False):
        x = self._state_to_input(state).reshape(1, -1)
        weights = self.target_weights if use_target else self.weights
        q_values, _ = self._forward(x, weights)
        return q_values[0]

    def select_action(self, state, player=None, training=False):
        if player is not None:
            self.player = player

        valid_actions = self.game.get_valid_actions(state)
        if not valid_actions:
            return None

        if training and random.random() < self.epsilon:
            return random.choice(valid_actions)

        q_values = self._get_q_values(state)

        best_action = valid_actions[0]
        best_value = -math.inf
        for action in valid_actions:
            if q_values[action] > best_value:
                best_value = q_values[action]
                best_action = action

        return best_action

    def _store_experience(self, state, action, reward, next_state, done):
        self.replay_buffer.append((
            self._state_to_input(state),
            action,
            reward,
            self._state_to_input(next_state),
            done
        ))

    def _replay(self):
        if len(self.replay_buffer) < self.batch_size:
            return 0.0

        batch = random.sample(self.replay_buffer, self.batch_size)
        states, actions, rewards, next_states, dones = zip(*batch)

        states = np.array(states)
        actions = np.array(actions, dtype=int)
        rewards = np.array(rewards, dtype=np.float32)
        next_states = np.array(next_states)
        dones = np.array(dones, dtype=np.float32)

        rewards = np.clip(rewards, -1.0, 1.0)

        q_values, cache = self._forward(states, self.weights)
        next_q_values, _ = self._forward(next_states, self.target_weights)
        max_next_q = np.max(next_q_values, axis=1)

        targets = rewards + (1 - dones) * self.gamma * max_next_q
        targets = np.clip(targets, -2.0, 2.0)

        grads = self._backward_huber(q_values, targets, cache, actions)
        self._adam_update(grads)

        loss = 0.0
        for i in range(self.batch_size):
            diff = abs(q_values[i, actions[i]] - targets[i])
            if diff < 1.0:
                loss += 0.5 * diff * diff
            else:
                loss += diff - 0.5
        loss /= self.batch_size

        return loss

    def train(self, opponent, num_episodes=50000, verbose=True):
        self.epsilon = self.epsilon_start
        win_count = 0
        window_size = 500

        for episode in range(num_episodes):
            state = self.game.reset()
            done = False
            episode_reward = 0
            episode_loss = 0
            steps = 0
            winner = 0

            agent_turn = random.choice([True, False])

            if not agent_turn:
                opp_action = opponent.select_action(state, -self.player)
                state = self.game.make_move(state, opp_action, -self.player)
                done, winner = self.game.check_winner(state)
                agent_turn = True

            while not done:
                action = self.select_action(state, training=True)
                next_state = self.game.make_move(state, action, self.player)
                done, winner = self.game.check_winner(next_state)

                if done:
                    if winner == self.player:
                        reward = 1.0
                        win_count += 1
                    elif winner == -self.player:
                        reward = -1.0
                    else:
                        reward = 0.3
                    self._store_experience(state, action, reward, next_state, done)
                    episode_reward += reward
                    break

                # Opponent's turn
                opp_action = opponent.select_action(next_state, -self.player)
                after_opp_state = self.game.make_move(next_state, opp_action, -self.player)
                done, winner = self.game.check_winner(after_opp_state)

                if done:
                    if winner == self.player:
                        reward = 1.0
                        win_count += 1
                    elif winner == -self.player:
                        reward = -1.0
                    else:
                        reward = 0.3
                    self._store_experience(state, action, reward, after_opp_state, done)
                    episode_reward += reward
                    break

                self._store_experience(state, action, 0.0, after_opp_state, False)
                state = after_opp_state
                steps += 1

            if len(self.replay_buffer) >= self.batch_size:
                loss = self._replay()
                episode_loss = loss

            self.epsilon = max(self.epsilon_end, self.epsilon * self.epsilon_decay)

            if (episode + 1) % self.target_update_freq == 0:
                self._soft_update_target()

            self.training_rewards.append(episode_reward)
            self.training_wins.append(1 if (done and winner == self.player) else 0)
            self.training_epsilon.append(self.epsilon)
            self.training_losses.append(episode_loss)

            if verbose and (episode + 1) % (num_episodes // 10) == 0:
                recent_wins = sum(self.training_wins[-window_size:])
                recent_total = min(window_size, episode + 1)
                win_rate = recent_wins / recent_total
                avg_loss = np.mean(self.training_losses[-window_size:])
                print(f"  Episode {episode + 1}/{num_episodes} | "
                      f"Win rate (last {recent_total}): {win_rate:.2%} | "
                      f"Epsilon: {self.epsilon:.4f} | "
                      f"Avg loss: {avg_loss:.4f}")

        if verbose:
            total_wins = sum(self.training_wins)
            print(f"  Training complete. Total wins: {total_wins}/{num_episodes} "
                  f"({total_wins / num_episodes:.2%})")

        return self.training_rewards, self.training_wins
