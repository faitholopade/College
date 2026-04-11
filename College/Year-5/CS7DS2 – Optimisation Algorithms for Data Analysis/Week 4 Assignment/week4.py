"""
CS7CS2 - Optimisation for Machine Learning
Week 4 Assignment
"""

import numpy as np
import matplotlib.pyplot as plt

# function defs

class QuadraticFn:
    """f(x, y) = x^2 + 100*y^2"""
    def f(self, x):
        return x[0]**2 + 100 * x[1]**2

    def grad(self, x):
        return np.array([2 * x[0], 200 * x[1]])

    def f_star(self):
        return 0.0


class RosenbrockFn:
    """f(x, y) = (1 - x)^2 + 100*(y - x^2)^2"""
    def f(self, x):
        return (1 - x[0])**2 + 100 * (x[1] - x[0]**2)**2

    def grad(self, x):
        dfdx = -2 * (1 - x[0]) - 400 * x[0] * (x[1] - x[0]**2)
        dfdy = 200 * (x[1] - x[0]**2)
        return np.array([dfdx, dfdy])

    def f_star(self):
        return 0.0


# optimisation algorithms

def polyak_step(fn, x0, num_iters, epsilon=1e-4, alpha_max=None):
    """
    Polyak step size: alpha = (f(x) - f*) / (||grad||^2 + epsilon)
    alpha_max can cap the step size.
    """
    x = np.array(x0, dtype=float)
    f_star = fn.f_star()
    X = [x.copy()]
    F = [fn.f(x)]

    for k in range(num_iters):
        g = fn.grad(x)
        grad_norm_sq = np.dot(g, g)
        alpha = (fn.f(x) - f_star) / (grad_norm_sq + epsilon)
        if alpha_max is not None:
            alpha = min(alpha, alpha_max)
        x = x - alpha * g
        X.append(x.copy())
        F.append(fn.f(x))

    return np.array(X), np.array(F)


def rmsprop(fn, x0, num_iters, alpha=0.01, beta=0.9, epsilon=1e-8):
    """
    RMSProp:
      sum = beta * sum + (1 - beta) * grad^2
      x = x - alpha / sqrt(sum + epsilon) * grad
    """
    x = np.array(x0, dtype=float)
    s = np.zeros_like(x)
    X = [x.copy()]
    F = [fn.f(x)]

    for k in range(num_iters):
        g = fn.grad(x)
        s = beta * s + (1 - beta) * g**2
        step = alpha / (np.sqrt(s) + epsilon) * g
        x = x - step
        X.append(x.copy())
        F.append(fn.f(x))

    return np.array(X), np.array(F)


def heavy_ball(fn, x0, num_iters, alpha=0.01, beta=0.9):
    """
    Heavy Ball / Polyak Momentum:
      z = beta * z + alpha * grad
      x = x - z
    """
    x = np.array(x0, dtype=float)
    z = np.zeros_like(x)
    X = [x.copy()]
    F = [fn.f(x)]

    for k in range(num_iters):
        g = fn.grad(x)
        z = beta * z + alpha * g
        x = x - z
        X.append(x.copy())
        F.append(fn.f(x))

    return np.array(X), np.array(F)


def adam(fn, x0, num_iters, alpha=0.1, beta1=0.9, beta2=0.999, epsilon=1e-8):
    """
    Adam:
      m = beta1 * m + (1 - beta1) * grad
      v = beta2 * v + (1 - beta2) * grad^2
      m_hat = m / (1 - beta1^t)
      v_hat = v / (1 - beta2^t)
      x = x - alpha * m_hat / (sqrt(v_hat) + epsilon)
    """
    x = np.array(x0, dtype=float)
    m = np.zeros_like(x)
    v = np.zeros_like(x)
    X = [x.copy()]
    F = [fn.f(x)]

    for k in range(num_iters):
        t = k + 1
        g = fn.grad(x)
        m = beta1 * m + (1 - beta1) * g
        v = beta2 * v + (1 - beta2) * g**2
        m_hat = m / (1 - beta1**t)
        v_hat = v / (1 - beta2**t)
        x = x - alpha * m_hat / (np.sqrt(v_hat) + epsilon)
        X.append(x.copy())
        F.append(fn.f(x))

    return np.array(X), np.array(F)


# q1

fn1 = QuadraticFn()
x0_q1 = [2.0, 2.0]
n_iters_q1 = 200

print("=" * 60)
print("Q1: Quadratic Function f(x,y) = x^2 + 100*y^2")
print("=" * 60)

# (I)
print("\n--- Q1(I): Convergence comparison ---")

X_polyak_q1, F_polyak_q1 = polyak_step(fn1, x0_q1, n_iters_q1, epsilon=1e-4)
X_rms_q1, F_rms_q1 = rmsprop(fn1, x0_q1, n_iters_q1, alpha=0.2, beta=0.9)
X_hb_q1, F_hb_q1 = heavy_ball(fn1, x0_q1, n_iters_q1, alpha=0.01, beta=0.9)
X_adam_q1, F_adam_q1 = adam(fn1, x0_q1, n_iters_q1, alpha=0.1, beta1=0.9, beta2=0.999)

plt.figure(figsize=(10, 6))
plt.semilogy(F_polyak_q1, label='Polyak Step Size', linewidth=1.5)
plt.semilogy(F_rms_q1, label='RMSProp (α=0.2, β=0.9)', linewidth=1.5)
plt.semilogy(F_hb_q1, label='Heavy Ball (α=0.01, β=0.9)', linewidth=1.5)
plt.semilogy(F_adam_q1, label='Adam (α=0.1, β₁=0.9, β₂=0.999)', linewidth=1.5)
plt.xlabel('Iteration')
plt.ylabel('Function Value (log scale)')
plt.title('Q1(I): Convergence Comparison on f(x,y) = x² + 100y²')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('q1_i_convergence.png', dpi=150)
plt.close()
print("Saved: q1_i_convergence.png")

print(f"  Polyak  final f = {F_polyak_q1[-1]:.6e}")
print(f"  RMSProp final f = {F_rms_q1[-1]:.6e}")
print(f"  HeavyBall final f = {F_hb_q1[-1]:.6e}")
print(f"  Adam    final f = {F_adam_q1[-1]:.6e}")


# (II)
# both x and y  so divergence in y at alpha=0.02 is clearly visible
print("\n--- Q1(II): Heavy Ball stability sweep ---")

alphas_hb = [0.006, 0.01, 0.02]

fig, axes = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

for a in alphas_hb:
    X_hb, F_hb = heavy_ball(fn1, x0_q1, n_iters_q1, alpha=a, beta=0.9)
    axes[0].plot(X_hb[:, 0], label=f'α = {a}', linewidth=1.5)
    axes[1].plot(X_hb[:, 1], label=f'α = {a}', linewidth=1.5)

axes[0].set_ylabel('x-coordinate')
axes[0].set_title('Q1(II): Heavy Ball Stability Sweep (β = 0.9)')
axes[0].legend()
axes[0].grid(True, alpha=0.3)

axes[1].set_xlabel('Iteration')
axes[1].set_ylabel('y-coordinate')
axes[1].legend()
axes[1].grid(True, alpha=0.3)
# clip y-axis so divergence is visible but stable cases remain readable
axes[1].set_ylim(-5, 5)

plt.tight_layout()
plt.savefig('q1_ii_hb_stability.png', dpi=150)
plt.close()
print("Saved: q1_ii_hb_stability.png")

for a in alphas_hb:
    X_hb, F_hb = heavy_ball(fn1, x0_q1, n_iters_q1, alpha=a, beta=0.9)
    print(f"  α={a}: final x={X_hb[-1, 0]:.4e}, final y={X_hb[-1, 1]:.4e}, "
          f"final f={F_hb[-1]:.4e}, max|y|={np.max(np.abs(X_hb[:, 1])):.4e}")


# (III)
print("\n--- Q1(III): Contour plot with trajectories ---")

X_hb_traj, _ = heavy_ball(fn1, x0_q1, n_iters_q1, alpha=0.01, beta=0.9)
X_rms_traj, _ = rmsprop(fn1, x0_q1, n_iters_q1, alpha=0.2, beta=0.9)
X_adam_traj, _ = adam(fn1, x0_q1, n_iters_q1, alpha=0.1, beta1=0.9, beta2=0.999)

xrange = np.linspace(-2.5, 2.5, 300)
yrange = np.linspace(-2.5, 2.5, 300)
Xg, Yg = np.meshgrid(xrange, yrange)
Zg = Xg**2 + 100 * Yg**2

plt.figure(figsize=(10, 8))
levels = np.logspace(-1, 3, 20)
plt.contour(Xg, Yg, Zg, levels=levels, cmap='viridis', alpha=0.6)
plt.colorbar(label='f(x, y)')

plt.plot(X_hb_traj[:, 0], X_hb_traj[:, 1], 'r-o', label='Heavy Ball (α=0.01, β=0.9)',
         markersize=2, linewidth=1, alpha=0.8)
plt.plot(X_rms_traj[:, 0], X_rms_traj[:, 1], 'b-s', label='RMSProp (α=0.2, β=0.9)',
         markersize=2, linewidth=1, alpha=0.8)
plt.plot(X_adam_traj[:, 0], X_adam_traj[:, 1], 'g-^', label='Adam (α=0.1, β₁=0.9, β₂=0.999)',
         markersize=2, linewidth=1, alpha=0.8)

plt.plot(2, 2, 'kx', markersize=10, markeredgewidth=2, label='Start (2, 2)')
plt.plot(0, 0, 'k*', markersize=12, label='Minimum (0, 0)')

plt.xlabel('x')
plt.ylabel('y')
plt.title('Q1(III): Contour Plot with Optimiser Trajectories\nf(x,y) = x² + 100y²')
plt.legend(loc='upper right', fontsize=9)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('q1_iii_contour.png', dpi=150)
plt.close()
print("Saved: q1_iii_contour.png")


# q2

fn2 = RosenbrockFn()
x0_q2 = [-1.25, 0.5]
n_iters_q2 = 3000

print("\n" + "=" * 60)
print("Q2: Rosenbrock Function f(x,y) = (1-x)² + 100(y-x²)²")
print("=" * 60)

# (I)
print("\n--- Q2(I): Convergence comparison ---")

X_polyak_q2, F_polyak_q2 = polyak_step(fn2, x0_q2, n_iters_q2, epsilon=1e-4, alpha_max=0.1)
X_rms_q2, F_rms_q2 = rmsprop(fn2, x0_q2, n_iters_q2, alpha=0.01, beta=0.9)
X_hb_q2, F_hb_q2 = heavy_ball(fn2, x0_q2, n_iters_q2, alpha=2e-4, beta=0.9)
X_adam_q2, F_adam_q2 = adam(fn2, x0_q2, n_iters_q2, alpha=0.05, beta1=0.9, beta2=0.999)

plt.figure(figsize=(10, 6))
plt.semilogy(F_polyak_q2, label='Polyak Step Size (α ≤ 0.1)', linewidth=1.5)
plt.semilogy(F_rms_q2, label='RMSProp (α=0.01, β=0.9)', linewidth=1.5)
plt.semilogy(F_hb_q2, label='Heavy Ball (α=2e-4, β=0.9)', linewidth=1.5)
plt.semilogy(F_adam_q2, label='Adam (α=0.05, β₁=0.9, β₂=0.999)', linewidth=1.5)
plt.xlabel('Iteration')
plt.ylabel('Function Value (log scale)')
plt.title('Q2(I): Convergence Comparison on Rosenbrock Function')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('q2_i_convergence.png', dpi=150)
plt.close()
print("Saved: q2_i_convergence.png")

print(f"  Polyak  final f = {F_polyak_q2[-1]:.6e}")
print(f"  RMSProp final f = {F_rms_q2[-1]:.6e}")
print(f"  HeavyBall final f = {F_hb_q2[-1]:.6e}")
print(f"  Adam    final f = {F_adam_q2[-1]:.6e}")


# (II)
# required alphas + a larger alpha (0.5) to show actual instability threshold
print("\n--- Q2(II): Adam stability sweep ---")

alphas_adam = [0.02, 0.05, 0.12, 0.5]

fig, axes = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

for a in alphas_adam:
    X_ad, F_ad = adam(fn2, x0_q2, n_iters_q2, alpha=a, beta1=0.9, beta2=0.999)
    axes[0].plot(X_ad[:, 0], label=f'α = {a}', linewidth=1.5)

axes[0].set_ylabel('x-coordinate')
axes[0].set_title('Q2(II): Adam Stability Sweep (β₁=0.9, β₂=0.999)')
axes[0].legend()
axes[0].grid(True, alpha=0.3)

# value plot to clearly show instability/oscillation
for a in alphas_adam:
    X_ad, F_ad = adam(fn2, x0_q2, n_iters_q2, alpha=a, beta1=0.9, beta2=0.999)
    axes[1].semilogy(F_ad, label=f'α = {a}', linewidth=1.5)

axes[1].set_xlabel('Iteration')
axes[1].set_ylabel('Function Value (log scale)')
axes[1].legend()
axes[1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('q2_ii_adam_stability.png', dpi=150)
plt.close()
print("Saved: q2_ii_adam_stability.png")

for a in alphas_adam:
    X_ad, F_ad = adam(fn2, x0_q2, n_iters_q2, alpha=a, beta1=0.9, beta2=0.999)
    print(f"  α={a}: final x={X_ad[-1, 0]:.4e}, final y={X_ad[-1, 1]:.4e}, "
          f"final f={F_ad[-1]:.4e}, max|x|={np.max(np.abs(X_ad[:, 0])):.4e}")


# (III)
print("\n--- Q2(III): Contour plot with trajectories ---")

X_hb_traj2, _ = heavy_ball(fn2, x0_q2, n_iters_q2, alpha=2e-4, beta=0.9)
X_rms_traj2, _ = rmsprop(fn2, x0_q2, n_iters_q2, alpha=0.01, beta=0.9)
X_adam_traj2, _ = adam(fn2, x0_q2, n_iters_q2, alpha=0.05, beta1=0.9, beta2=0.999)

xrange2 = np.linspace(-2, 2, 400)
yrange2 = np.linspace(-1, 3, 400)
Xg2, Yg2 = np.meshgrid(xrange2, yrange2)
Zg2 = (1 - Xg2)**2 + 100 * (Yg2 - Xg2**2)**2

plt.figure(figsize=(10, 8))
levels2 = np.logspace(-1, 3.5, 25)
plt.contour(Xg2, Yg2, Zg2, levels=levels2, cmap='viridis', alpha=0.6)
plt.colorbar(label='f(x, y)')

plt.plot(X_hb_traj2[:, 0], X_hb_traj2[:, 1], 'r-', label='Heavy Ball (α=2e-4, β=0.9)',
         linewidth=1, alpha=0.8)
plt.plot(X_rms_traj2[:, 0], X_rms_traj2[:, 1], 'b-', label='RMSProp (α=0.01, β=0.9)',
         linewidth=1, alpha=0.8)
plt.plot(X_adam_traj2[:, 0], X_adam_traj2[:, 1], 'g-', label='Adam (α=0.05, β₁=0.9, β₂=0.999)',
         linewidth=1, alpha=0.8)

plt.plot(-1.25, 0.5, 'kx', markersize=10, markeredgewidth=2, label='Start (-1.25, 0.5)')
plt.plot(1, 1, 'k*', markersize=12, label='Minimum (1, 1)')

plt.xlabel('x')
plt.ylabel('y')
plt.title('Q2(III): Contour Plot with Optimiser Trajectories\nRosenbrock: f(x,y) = (1-x)² + 100(y-x²)²')
plt.legend(loc='upper left', fontsize=9)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('q2_iii_contour.png', dpi=150)
plt.close()
print("Saved: q2_iii_contour.png")

print("\n" + "=" * 60)
print("All plots saved. Done!")
print("=" * 60)