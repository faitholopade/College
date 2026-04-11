import numpy as np
import matplotlib.pyplot as plt
import os

# for reproducibility
np.random.seed(42)

# output directory for plots
OUT = "plots"
os.makedirs(OUT, exist_ok=True)

# contour plotting helper
def contour_plot(cost_fn, trajectories, labels, colors, title, fname,
                 x1_range=(-1, 5), x2_range=(-1, 6), levels=50):
    """
    cost_fn : callable(theta1_grid, theta2_grid) -> cost_grid
    trajectories : list of arrays, each shape (T, 2)
    """
    g1 = np.linspace(*x1_range, 300)
    g2 = np.linspace(*x2_range, 300)
    T1, T2 = np.meshgrid(g1, g2)
    Z = cost_fn(T1, T2)

    fig, ax = plt.subplots(figsize=(7, 6))
    cs = ax.contour(T1, T2, Z, levels=levels, cmap='viridis', alpha=0.7)
    ax.clabel(cs, inline=True, fontsize=7)
    for traj, lab, col in zip(trajectories, labels, colors):
        ax.plot(traj[:, 0], traj[:, 1], 'o-', markersize=2,
                linewidth=1, label=lab, color=col, alpha=0.8)
    ax.set_xlabel(r'$\theta_1$'); ax.set_ylabel(r'$\theta_2$')
    ax.set_title(title); ax.legend(fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(OUT, fname), dpi=150)
    plt.close(fig)
    print(f"  Saved {fname}")


def loss_plot(loss_lists, labels, colors, title, fname, xlabel='Iteration'):
    fig, ax = plt.subplots(figsize=(7, 4))
    for L, lab, col in zip(loss_lists, labels, colors):
        ax.semilogy(L, label=lab, color=col, linewidth=1.2)
    ax.set_xlabel(xlabel); ax.set_ylabel('Loss (log scale)')
    ax.set_title(title); ax.legend(fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(OUT, fname), dpi=150)
    plt.close(fig)
    print(f"  Saved {fname}")


#Question 1
print("=" * 60)
print("Q1: Linear Regression")
print("=" * 60)

# data generation
m = 1000
X_data = np.random.randn(m, 2)
theta_star = np.array([3.0, 4.0])
sigma = 1.0
epsilon = sigma * np.random.randn(m)
y_data = X_data @ theta_star + epsilon

theta0 = np.array([1.0, 1.0])
alpha_q1 = 0.5

# cost and gradient 
def linreg_loss(theta, X, y):
    r = X @ theta - y
    return 0.5 * np.mean(r ** 2)

def linreg_grad(theta, X, y):
    r = X @ theta - y
    return (X.T @ r) / len(y)

# cost surface callable for contour plots
def linreg_cost_surface(t1, t2):
    Z = np.zeros_like(t1)
    for i in range(t1.shape[0]):
        for j in range(t1.shape[1]):
            th = np.array([t1[i, j], t2[i, j]])
            Z[i, j] = linreg_loss(th, X_data, y_data)
    return Z

# full batch GD
print("\n--- Q1(a): Full-batch GD ---")
n_iter_gd = 80
theta = theta0.copy()
gd_losses = [linreg_loss(theta, X_data, y_data)]
gd_traj = [theta.copy()]
for _ in range(n_iter_gd):
    g = linreg_grad(theta, X_data, y_data)
    theta = theta - alpha_q1 * g
    gd_losses.append(linreg_loss(theta, X_data, y_data))
    gd_traj.append(theta.copy())
gd_traj = np.array(gd_traj)

loss_plot([gd_losses], ['Full-batch GD'], ['blue'],
          'Q1(a): Loss vs Iteration (Full-batch GD)', 'q1a_loss.png')
contour_plot(linreg_cost_surface, [gd_traj], ['GD'], ['blue'],
             'Q1(a): GD Trajectory on Contour', 'q1a_contour.png',
             x1_range=(0.5, 3.5), x2_range=(0.5, 4.5))

# mini batch SGD
print("\n--- Q1(b): Mini-batch SGD ---")
def sgd_run(X, y, theta0, alpha, batch_size, n_updates):
    """Mini-batch SGD with shuffle-once-per-epoch."""
    theta = theta0.copy()
    losses = [linreg_loss(theta, X, y)]
    traj = [theta.copy()]
    m_local = len(y)
    idx = np.arange(m_local)
    update_count = 0
    while update_count < n_updates:
        np.random.shuffle(idx)
        for start in range(0, m_local, batch_size):
            if update_count >= n_updates:
                break
            batch = idx[start:start + batch_size]
            g = linreg_grad(theta, X[batch], y[batch])
            theta = theta - alpha * g
            update_count += 1
            losses.append(linreg_loss(theta, X, y))
            traj.append(theta.copy())
    return np.array(losses), np.array(traj)

sgd5_losses, sgd5_traj = sgd_run(X_data, y_data, theta0, alpha_q1, 5, 400)
sgd20_losses, sgd20_traj = sgd_run(X_data, y_data, theta0, alpha_q1, 20, 400)

# extend GD losses with last value to match SGD length for plotting
gd_losses_ext = gd_losses + [gd_losses[-1]] * (400 - len(gd_losses) + 1)

loss_plot([gd_losses_ext, sgd5_losses, sgd20_losses],
          ['GD', 'SGD (b=5)', 'SGD (b=20)'],
          ['blue', 'red', 'green'],
          'Q1(b): Loss Comparison — GD vs SGD', 'q1b_loss.png',
          xlabel='Update step')

contour_plot(linreg_cost_surface,
             [gd_traj, sgd5_traj, sgd20_traj],
             ['GD', 'SGD (b=5)', 'SGD (b=20)'],
             ['blue', 'red', 'green'],
             'Q1(b): Trajectories on Contour', 'q1b_contour.png',
             x1_range=(0.5, 3.5), x2_range=(0.5, 4.5))

# NAG and Adagrad
print("\n--- Q1(c): NAG & Adagrad ---")

def sgd_constant_run(X, y, theta0, alpha, batch_size, n_updates):
    """Plain SGD with constant step size."""
    return sgd_run(X, y, theta0, alpha, batch_size, n_updates)

def nag_run(X, y, theta0, alpha, beta, batch_size, n_updates):
    """Nesterov Accelerated Gradient with mini-batch."""
    theta = theta0.copy()
    v = np.zeros_like(theta)
    losses = [linreg_loss(theta, X, y)]
    traj = [theta.copy()]
    m_local = len(y)
    idx = np.arange(m_local)
    update_count = 0
    while update_count < n_updates:
        np.random.shuffle(idx)
        for start in range(0, m_local, batch_size):
            if update_count >= n_updates:
                break
            batch = idx[start:start + batch_size]
            # look ahead position
            theta_lookahead = theta + beta * v
            g = linreg_grad(theta_lookahead, X[batch], y[batch])
            v = beta * v - alpha * g
            theta = theta + v
            update_count += 1
            losses.append(linreg_loss(theta, X, y))
            traj.append(theta.copy())
    return np.array(losses), np.array(traj)

def adagrad_run(X, y, theta0, alpha0, batch_size, n_updates, eps=1e-8):
    """Adagrad with mini-batch."""
    theta = theta0.copy()
    sum_sq = np.zeros_like(theta)
    losses = [linreg_loss(theta, X, y)]
    traj = [theta.copy()]
    m_local = len(y)
    idx = np.arange(m_local)
    update_count = 0
    while update_count < n_updates:
        np.random.shuffle(idx)
        for start in range(0, m_local, batch_size):
            if update_count >= n_updates:
                break
            batch = idx[start:start + batch_size]
            g = linreg_grad(theta, X[batch], y[batch])
            sum_sq += g ** 2
            theta = theta - (alpha0 / (np.sqrt(sum_sq) + eps)) * g
            update_count += 1
            losses.append(linreg_loss(theta, X, y))
            traj.append(theta.copy())
    return np.array(losses), np.array(traj)

sgd10_losses, sgd10_traj = sgd_constant_run(X_data, y_data, theta0, alpha_q1, 10, 400)
nag_losses, nag_traj = nag_run(X_data, y_data, theta0, alpha_q1, 0.9, 10, 400)
ada_losses, ada_traj = adagrad_run(X_data, y_data, theta0, 2.5, 10, 400)

loss_plot([sgd10_losses, nag_losses, ada_losses],
          ['SGD (constant)', 'NAG', 'Adagrad'],
          ['blue', 'red', 'green'],
          'Q1(c): Loss — SGD vs NAG vs Adagrad', 'q1c_loss.png',
          xlabel='Update step')

contour_plot(linreg_cost_surface,
             [sgd10_traj, nag_traj, ada_traj],
             ['SGD (constant)', 'NAG', 'Adagrad'],
             ['blue', 'red', 'green'],
             'Q1(c): Trajectories — SGD vs NAG vs Adagrad', 'q1c_contour.png',
             x1_range=(0.5, 3.5), x2_range=(0.5, 4.5))


# Question 2
print("\n" + "=" * 60)
print("Q2: Toy Neural Net")
print("=" * 60)

# data generation
m2 = 1000
u_data = np.random.uniform(-2, 2, m2)
x_star = np.array([1.0, 3.0])
noise2 = 0.05 * np.random.randn(m2)

def nn_predict(u, x):
    return x[1] * np.tanh(x[0] * u)

y2_data = nn_predict(u_data, x_star) + noise2
x0_q2 = np.array([1.0, 1.0])
alpha_q2 = 0.75

# cost and gradient for the toy neural net
def nn_loss(x, u, y):
    pred = x[1] * np.tanh(x[0] * u)
    return 0.5 * np.mean((pred - y) ** 2)

def nn_grad(x, u, y):
    """Analytical gradient of J w.r.t. x = [x1, x2]."""
    pred = x[1] * np.tanh(x[0] * u)
    r = pred - y                             # (m,)
    dtanh = 1.0 - np.tanh(x[0] * u) ** 2    # sech^2
    dJ_dx1 = np.mean(r * x[1] * dtanh * u)
    dJ_dx2 = np.mean(r * np.tanh(x[0] * u))
    return np.array([dJ_dx1, dJ_dx2])

def nn_cost_surface(t1, t2):
    Z = np.zeros_like(t1)
    for i in range(t1.shape[0]):
        for j in range(t1.shape[1]):
            xv = np.array([t1[i, j], t2[i, j]])
            Z[i, j] = nn_loss(xv, u_data, y2_data)
    return Z

# full batch GD
print("\n--- Q2(a): Full-batch GD ---")
n_iter_q2 = 500
x_cur = x0_q2.copy()
gd2_losses = [nn_loss(x_cur, u_data, y2_data)]
gd2_traj = [x_cur.copy()]
for _ in range(n_iter_q2):
    g = nn_grad(x_cur, u_data, y2_data)
    x_cur = x_cur - alpha_q2 * g
    gd2_losses.append(nn_loss(x_cur, u_data, y2_data))
    gd2_traj.append(x_cur.copy())
gd2_traj = np.array(gd2_traj)

loss_plot([gd2_losses], ['Full-batch GD'], ['blue'],
          'Q2(a): Loss vs Iteration (Full-batch GD)', 'q2a_loss.png')
contour_plot(nn_cost_surface, [gd2_traj], ['GD'], ['blue'],
             'Q2(a): GD Trajectory on Contour', 'q2a_contour.png',
             x1_range=(0.5, 1.5), x2_range=(0.5, 3.5))

# mini batch SGD
print("\n--- Q2(b): Mini-batch SGD ---")

def nn_sgd_run(u, y, x0, alpha, batch_size, n_updates):
    x = x0.copy()
    losses = [nn_loss(x, u, y)]
    traj = [x.copy()]
    m_l = len(y); idx = np.arange(m_l); uc = 0
    while uc < n_updates:
        np.random.shuffle(idx)
        for st in range(0, m_l, batch_size):
            if uc >= n_updates: break
            b = idx[st:st + batch_size]
            g = nn_grad(x, u[b], y[b])
            x = x - alpha * g
            uc += 1
            losses.append(nn_loss(x, u, y))
            traj.append(x.copy())
    return np.array(losses), np.array(traj)

nn_sgd5_l, nn_sgd5_t = nn_sgd_run(u_data, y2_data, x0_q2, alpha_q2, 5, 500)
nn_sgd20_l, nn_sgd20_t = nn_sgd_run(u_data, y2_data, x0_q2, alpha_q2, 20, 500)

gd2_losses_ext = gd2_losses + [gd2_losses[-1]] * max(0, 500 - len(gd2_losses) + 1)
loss_plot([gd2_losses_ext, nn_sgd5_l, nn_sgd20_l],
          ['GD', 'SGD (b=5)', 'SGD (b=20)'],
          ['blue', 'red', 'green'],
          'Q2(b): Loss Comparison — GD vs SGD', 'q2b_loss.png',
          xlabel='Update step')
contour_plot(nn_cost_surface,
             [gd2_traj, nn_sgd5_t, nn_sgd20_t],
             ['GD', 'SGD (b=5)', 'SGD (b=20)'],
             ['blue', 'red', 'green'],
             'Q2(b): Trajectories on Contour', 'q2b_contour.png',
             x1_range=(0.5, 1.5), x2_range=(0.5, 3.5))

# NAG and Adagrad
print("\n--- Q2(c): NAG & Adagrad ---")

def nn_nag_run(u, y, x0, alpha, beta, batch_size, n_updates):
    x = x0.copy(); v = np.zeros_like(x)
    losses = [nn_loss(x, u, y)]; traj = [x.copy()]
    m_l = len(y); idx = np.arange(m_l); uc = 0
    while uc < n_updates:
        np.random.shuffle(idx)
        for st in range(0, m_l, batch_size):
            if uc >= n_updates: break
            b = idx[st:st + batch_size]
            x_look = x + beta * v
            g = nn_grad(x_look, u[b], y[b])
            v = beta * v - alpha * g
            x = x + v
            uc += 1
            losses.append(nn_loss(x, u, y))
            traj.append(x.copy())
    return np.array(losses), np.array(traj)

def nn_adagrad_run(u, y, x0, alpha0, batch_size, n_updates, eps=1e-8):
    x = x0.copy(); sum_sq = np.zeros_like(x)
    losses = [nn_loss(x, u, y)]; traj = [x.copy()]
    m_l = len(y); idx = np.arange(m_l); uc = 0
    while uc < n_updates:
        np.random.shuffle(idx)
        for st in range(0, m_l, batch_size):
            if uc >= n_updates: break
            b = idx[st:st + batch_size]
            g = nn_grad(x, u[b], y[b])
            sum_sq += g ** 2
            x = x - (alpha0 / (np.sqrt(sum_sq) + eps)) * g
            uc += 1
            losses.append(nn_loss(x, u, y))
            traj.append(x.copy())
    return np.array(losses), np.array(traj)

nn_sgd10_l, nn_sgd10_t = nn_sgd_run(u_data, y2_data, x0_q2, alpha_q2, 10, 500)
nn_nag_l, nn_nag_t = nn_nag_run(u_data, y2_data, x0_q2, alpha_q2, 0.9, 10, 500)
nn_ada_l, nn_ada_t = nn_adagrad_run(u_data, y2_data, x0_q2, 2.5, 10, 500)

loss_plot([nn_sgd10_l, nn_nag_l, nn_ada_l],
          ['SGD (constant)', 'NAG', 'Adagrad'],
          ['blue', 'red', 'green'],
          'Q2(c): Loss — SGD vs NAG vs Adagrad', 'q2c_loss.png',
          xlabel='Update step')
contour_plot(nn_cost_surface,
             [nn_sgd10_t, nn_nag_t, nn_ada_t],
             ['SGD (constant)', 'NAG', 'Adagrad'],
             ['blue', 'red', 'green'],
             'Q2(c): Trajectories — SGD vs NAG vs Adagrad', 'q2c_contour.png',
             x1_range=(0.5, 1.5), x2_range=(0.5, 3.5))


# Question 3
print("\n" + "=" * 60)
print("Q3: Rosenbrock Function")
print("=" * 60)

def rosenbrock(x):
    return (1 - x[0]) ** 2 + 100 * (x[1] - x[0] ** 2) ** 2

def rosenbrock_grad(x):
    """Analytical gradient of Rosenbrock."""
    df_dx1 = -2 * (1 - x[0]) + 200 * (x[1] - x[0] ** 2) * (-2 * x[0])
    df_dx2 = 200 * (x[1] - x[0] ** 2)
    return np.array([df_dx1, df_dx2])

def rosenbrock_surface(t1, t2):
    return (1 - t1) ** 2 + 100 * (t2 - t1 ** 2) ** 2

x0_q3 = np.array([-1.0, 1.0])

# full batch GD
print("\n--- Q3(a): Gradient Descent ---")
n_iter_q3 = 200
alpha_q3_gd = 1e-3
x = x0_q3.copy()
rb_gd_losses = [rosenbrock(x)]
rb_gd_traj = [x.copy()]
for _ in range(n_iter_q3):
    g = rosenbrock_grad(x)
    x = x - alpha_q3_gd * g
    rb_gd_losses.append(rosenbrock(x))
    rb_gd_traj.append(x.copy())
rb_gd_traj = np.array(rb_gd_traj)

loss_plot([rb_gd_losses], ['GD (α=0.001)'], ['blue'],
          'Q3(a): Rosenbrock — GD Loss vs Iteration', 'q3a_loss.png')

# Newton (finite differences)
print("\n--- Q3(b): Newton (finite differences) ---")

def finite_diff_grad(f, x, delta=1e-5):
    n = len(x)
    g = np.zeros(n)
    for i in range(n):
        xp = x.copy(); xp[i] += delta
        xm = x.copy(); xm[i] -= delta
        g[i] = (f(xp) - f(xm)) / (2 * delta)
    return g

def finite_diff_hessian(f, x, delta=1e-5):
    n = len(x)
    H = np.zeros((n, n))
    f0 = f(x)
    for i in range(n):
        for j in range(n):
            xpp = x.copy(); xpp[i] += delta; xpp[j] += delta
            xpm = x.copy(); xpm[i] += delta; xpm[j] -= delta
            xmp = x.copy(); xmp[i] -= delta; xmp[j] += delta
            xmm = x.copy(); xmm[i] -= delta; xmm[j] -= delta
            H[i, j] = (f(xpp) - f(xpm) - f(xmp) + f(xmm)) / (4 * delta ** 2)
    return H

alpha_newton = 0.7
x = x0_q3.copy()
rb_newton_losses = [rosenbrock(x)]
rb_newton_traj = [x.copy()]
for _ in range(n_iter_q3):
    g = finite_diff_grad(rosenbrock, x)
    H = finite_diff_hessian(rosenbrock, x)
    try:
        p = np.linalg.solve(H, g)
    except np.linalg.LinAlgError:
        p = g  # fallback
    x = x - alpha_newton * p
    rb_newton_losses.append(rosenbrock(x))
    rb_newton_traj.append(x.copy())
rb_newton_traj = np.array(rb_newton_traj)

loss_plot([rb_newton_losses], ['Newton (α=0.7)'], ['orange'],
          'Q3(b): Rosenbrock — Newton Loss vs Iteration', 'q3b_loss.png')

# Damped Newton
print("\n--- Q3(c): Damped Newton ---")

def damped_newton(f, x0, n_iters=200, alpha0=1.0, rho=0.5, K=50, lam=1e-6):
    x = x0.copy()
    losses = [f(x)]; traj = [x.copy()]
    for _ in range(n_iters):
        ft = f(x)
        gt = finite_diff_grad(f, x)
        Ht = finite_diff_hessian(f, x)
        # Regularise Hessian
        Ht_reg = Ht + lam * np.eye(len(x))
        try:
            pt = np.linalg.solve(Ht_reg, gt)
        except np.linalg.LinAlgError:
            pt = gt
        # Backtracking
        alpha = alpha0
        accepted = False
        for _ in range(K):
            x_new = x - alpha * pt
            if f(x_new) < ft:
                accepted = True
                break
            alpha *= rho
        if not accepted:
            x_new = x - 1e-4 * gt  # fallback gradient step
        x = x_new
        losses.append(f(x)); traj.append(x.copy())
    return np.array(losses), np.array(traj)

rb_dn_losses, rb_dn_traj = damped_newton(rosenbrock, x0_q3)

# Combined loss plot
loss_plot([rb_gd_losses, rb_newton_losses, rb_dn_losses],
          ['GD (α=0.001)', 'Newton (α=0.7)', 'Damped Newton'],
          ['blue', 'orange', 'red'],
          'Q3(c): Rosenbrock — GD vs Newton vs Damped Newton', 'q3c_loss.png')

# Combined contour plot (subsample GD for clarity)
step_gd = max(1, len(rb_gd_traj) // 40)
contour_plot(rosenbrock_surface,
             [rb_gd_traj[::step_gd], rb_newton_traj, rb_dn_traj],
             ['GD (subsampled)', 'Newton', 'Damped Newton'],
             ['blue', 'orange', 'red'],
             'Q3(c): Rosenbrock — Trajectories on Contour', 'q3c_contour.png',
             x1_range=(-1.5, 1.5), x2_range=(-0.5, 1.5),
             levels=np.logspace(-1, 3.5, 30))

# Final parameter estimates summary
print("\n" + "=" * 60)
print("FINAL PARAMETER ESTIMATES")
print("=" * 60)
print(f"Q1 — θ* = {theta_star}")
print(f"  GD final θ      = {gd_traj[-1]}")
print(f"  SGD(b=5) final θ = {sgd5_traj[-1]}")
print(f"  SGD(b=20) final θ= {sgd20_traj[-1]}")
print(f"  NAG final θ      = {nag_traj[-1]}")
print(f"  Adagrad final θ  = {ada_traj[-1]}")

print(f"\nQ2 — x* = {x_star}")
print(f"  GD final x       = {gd2_traj[-1]}")
print(f"  SGD(b=5) final x = {nn_sgd5_t[-1]}")
print(f"  SGD(b=20) final x= {nn_sgd20_t[-1]}")
print(f"  NAG final x      = {nn_nag_t[-1]}")
print(f"  Adagrad final x  = {nn_ada_t[-1]}")

print(f"\nQ3 — minimum at (1, 1), f=0")
print(f"  GD final x         = {rb_gd_traj[-1]},  f = {rb_gd_losses[-1]:.6e}")
print(f"  Newton final x     = {rb_newton_traj[-1]},  f = {rb_newton_losses[-1]:.6e}")
print(f"  Damped Newton final= {rb_dn_traj[-1]},  f = {rb_dn_losses[-1]:.6e}")

print(f"\nAll plots saved to '{OUT}/' directory.")
print("Done!")