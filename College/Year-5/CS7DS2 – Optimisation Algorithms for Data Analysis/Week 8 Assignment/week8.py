import numpy as np
import matplotlib.pyplot as plt

# ---
# QUESTION 1
# ---

# f(x1, x2) = (x1 - 1.2)^2 + 2(x2 - 2.5)^2 + 0.4*x1*x2

def f1(x):
    x1, x2 = x
    return (x1 - 1.2)**2 + 2*(x2 - 2.5)**2 + 0.4*x1*x2

def grad_f1(x):
    x1, x2 = x
    df_dx1 = 2*(x1 - 1.2) + 0.4*x2
    df_dx2 = 4*(x2 - 2.5) + 0.4*x1
    return np.array([df_dx1, df_dx2])

# (a) & (b): PGD on X1 = [0.5, 2.5] x [0.5, 3.5]
def project_X1(x):
    return np.array([np.clip(x[0], 0.5, 2.5), np.clip(x[1], 0.5, 3.5)])

def pgd_X1(x0, alpha, iters):
    x = x0.copy()
    trajectory = [x.copy()]
    for _ in range(iters):
        g = grad_f1(x)
        x = project_X1(x - alpha * g)
        trajectory.append(x.copy())
    return np.array(trajectory)

x0 = np.array([2.4, 0.7])
alpha1 = 0.1
traj_X1 = pgd_X1(x0, alpha1, 100)

# (c): PGD on X2 = {x1 >= 0.5, x2 >= 0.5, x1 <= x2}
def project_X2(x):
    for _ in range(10):
        x = np.array([max(x[0], 0.5), max(x[1], 0.5)])
        if x[0] > x[1]:
            mid = (x[0] + x[1]) / 2
            x = np.array([mid, mid])
        x = np.array([max(x[0], 0.5), max(x[1], 0.5)])
    return x

def pgd_X2(x0, alpha, iters):
    x = x0.copy()
    trajectory = [x.copy()]
    for _ in range(iters):
        g = grad_f1(x)
        x = project_X2(x - alpha * g)
        trajectory.append(x.copy())
    return np.array(trajectory)

traj_X2 = pgd_X2(x0, alpha1, 100)

# (d): plots for X1
fig, axes = plt.subplots(1, 3, figsize=(15, 4))

x1_vals = np.linspace(0, 3, 200)
x2_vals = np.linspace(0, 4, 200)
X1g, X2g = np.meshgrid(x1_vals, x2_vals)
Z = (X1g - 1.2)**2 + 2*(X2g - 2.5)**2 + 0.4*X1g*X2g

axes[0].contour(X1g, X2g, Z, levels=30, cmap='viridis')
rect = plt.Rectangle((0.5, 0.5), 2.0, 3.0, fill=False, edgecolor='red', linewidth=2, linestyle='--')
axes[0].add_patch(rect)
axes[0].plot(traj_X1[:, 0], traj_X1[:, 1], 'r.-', markersize=3, linewidth=1)
axes[0].plot(traj_X1[0, 0], traj_X1[0, 1], 'go', markersize=8, label='start')
axes[0].plot(traj_X1[-1, 0], traj_X1[-1, 1], 'rs', markersize=8, label='end')
axes[0].set_xlabel('$x_1$')
axes[0].set_ylabel('$x_2$')
axes[0].set_title('Contour + trajectory (X1)')
axes[0].legend()

# f(x^t) vs iteration
f_vals_X1 = [f1(traj_X1[i]) for i in range(len(traj_X1))]
axes[1].plot(f_vals_X1)
axes[1].set_xlabel('Iteration')
axes[1].set_ylabel('$f(x^{(t)})$')
axes[1].set_title('Objective vs iteration (X1)')

# x1^t and x2^t vs iteration
axes[2].plot(traj_X1[:, 0], label='$x_1$')
axes[2].plot(traj_X1[:, 1], label='$x_2$')
axes[2].set_xlabel('Iteration')
axes[2].set_ylabel('Value')
axes[2].set_title('Variables vs iteration (X1)')
axes[2].legend()

plt.tight_layout()
plt.savefig('q1_X1_plots.png', dpi=150, bbox_inches='tight')
plt.show()

# (e): plots for X2
fig, axes = plt.subplots(1, 3, figsize=(15, 4))

axes[0].contour(X1g, X2g, Z, levels=30, cmap='viridis')
feasible_x1 = np.linspace(0.5, 4, 300)
feasible_x2 = np.linspace(0.5, 4, 300)
FX1, FX2 = np.meshgrid(feasible_x1, feasible_x2)
feasible_mask = (FX1 >= 0.5) & (FX2 >= 0.5) & (FX1 <= FX2)
axes[0].contourf(FX1, FX2, feasible_mask.astype(float), levels=[0.5, 1.5], colors=['lightgreen'], alpha=0.3)
axes[0].plot([0.5, 0.5], [0.5, 4], 'r--', linewidth=1.5)
axes[0].plot([0.5, 4], [0.5, 0.5], 'r--', linewidth=1.5)
axes[0].plot([0.5, 4], [0.5, 4], 'r--', linewidth=1.5)
axes[0].plot(traj_X2[:, 0], traj_X2[:, 1], 'r.-', markersize=3, linewidth=1)
axes[0].plot(traj_X2[0, 0], traj_X2[0, 1], 'go', markersize=8, label='start')
axes[0].plot(traj_X2[-1, 0], traj_X2[-1, 1], 'rs', markersize=8, label='end')
axes[0].set_xlabel('$x_1$')
axes[0].set_ylabel('$x_2$')
axes[0].set_title('Contour + trajectory (X2)')
axes[0].set_xlim(0, 3.5)
axes[0].set_ylim(0, 4)
axes[0].legend()

f_vals_X2 = [f1(traj_X2[i]) for i in range(len(traj_X2))]
axes[1].plot(f_vals_X2)
axes[1].set_xlabel('Iteration')
axes[1].set_ylabel('$f(x^{(t)})$')
axes[1].set_title('Objective vs iteration (X2)')

axes[2].plot(traj_X2[:, 0], label='$x_1$')
axes[2].plot(traj_X2[:, 1], label='$x_2$')
axes[2].set_xlabel('Iteration')
axes[2].set_ylabel('Value')
axes[2].set_title('Variables vs iteration (X2)')
axes[2].legend()

plt.tight_layout()
plt.savefig('q1_X2_plots.png', dpi=150, bbox_inches='tight')
plt.show()

# (f): print final iterates
print("=== Q1 Results ===")
print(f"X1 final point: ({traj_X1[-1, 0]:.4f}, {traj_X1[-1, 1]:.4f}), f = {f1(traj_X1[-1]):.4f}")
print(f"X2 final point: ({traj_X2[-1, 0]:.4f}, {traj_X2[-1, 1]:.4f}), f = {f1(traj_X2[-1]):.4f}")

x_final_X1 = traj_X1[-1]
on_boundary_X1 = (abs(x_final_X1[0] - 0.5) < 1e-3 or abs(x_final_X1[0] - 2.5) < 1e-3 or
                   abs(x_final_X1[1] - 0.5) < 1e-3 or abs(x_final_X1[1] - 3.5) < 1e-3)
print(f"X1: on boundary = {on_boundary_X1}")

x_final_X2 = traj_X2[-1]
on_boundary_X2 = (abs(x_final_X2[0] - 0.5) < 1e-3 or abs(x_final_X2[1] - 0.5) < 1e-3 or
                   abs(x_final_X2[0] - x_final_X2[1]) < 1e-3)
print(f"X2: on boundary = {on_boundary_X2}")


# ---
# QUESTION 2
# ---

# f(x1, x2) = (x1 - 0.2)^2 + (x2 - 2)^2
# g1(x) = 0.5 - x1 <= 0, g2(x) = 1 - x1*x2 <= 0

def f2(x):
    return (x[0] - 0.2)**2 + (x[1] - 2)**2

def grad_f2(x):
    return np.array([2*(x[0] - 0.2), 2*(x[1] - 2)])

def g1(x):
    return 0.5 - x[0]

def g2(x):
    return 1 - x[0]*x[1]

def grad_g1(x):
    return np.array([-1, 0])

def grad_g2(x):
    return np.array([-x[1], -x[0]])

# (a): penalty function and augmented objective
def Q(x, lam1, lam2):
    return lam1 * max(0, g1(x)) + lam2 * max(0, g2(x))

def F(x, lam1, lam2):
    return f2(x) + Q(x, lam1, lam2)

def grad_Q(x, lam1, lam2):
    grad = np.zeros(2)
    if g1(x) > 0:
        grad += lam1 * grad_g1(x)
    if g2(x) > 0:
        grad += lam2 * grad_g2(x)
    return grad

def grad_F(x, lam1, lam2):
    return grad_f2(x) + grad_Q(x, lam1, lam2)

# (b): GD on F with small and large penalties
def penalty_gd(x0, lam1, lam2, alpha, iters):
    x = x0.copy()
    trajectory = [x.copy()]
    for _ in range(iters):
        g = grad_F(x, lam1, lam2)
        x = x - alpha * g
        trajectory.append(x.copy())
    return np.array(trajectory)

x0_q2 = np.array([1.4, 0.6])
alpha_q2 = 0.05

traj_small = penalty_gd(x0_q2, 0.5, 0.5, alpha_q2, 200)
traj_large = penalty_gd(x0_q2, 4.0, 4.0, alpha_q2, 200)

# (c): primal-dual method
def primal_dual(x0, alpha, beta, iters):
    x = x0.copy()
    lam = np.array([0.0, 0.0])
    traj = [x.copy()]
    lam_hist = [lam.copy()]
    for _ in range(iters):
        grad_L = grad_f2(x) + lam[0]*grad_g1(x) + lam[1]*grad_g2(x)
        x = x - alpha * grad_L
        lam[0] = max(0, lam[0] + beta * g1(x))
        lam[1] = max(0, lam[1] + beta * g2(x))
        traj.append(x.copy())
        lam_hist.append(lam.copy())
    return np.array(traj), np.array(lam_hist)

traj_pd, lam_pd = primal_dual(x0_q2, 0.06, 0.08, 200)

# (d): plots
fig, axes = plt.subplots(1, 3, figsize=(15, 4))

x1_vals = np.linspace(-0.5, 3, 200)
x2_vals = np.linspace(-0.5, 4, 200)
X1g, X2g = np.meshgrid(x1_vals, x2_vals)
Z2 = (X1g - 0.2)**2 + (X2g - 2)**2

feasible2 = (X1g >= 0.5) & (X1g * X2g >= 1)

axes[0].contour(X1g, X2g, Z2, levels=30, cmap='viridis')
axes[0].contourf(X1g, X2g, feasible2.astype(float), levels=[0.5, 1.5], colors=['lightgreen'], alpha=0.3)
axes[0].plot([0.5, 0.5], [-0.5, 4], 'r--', linewidth=1)
x1_curve = np.linspace(0.3, 3, 200)
x2_curve = 1.0 / x1_curve
axes[0].plot(x1_curve, x2_curve, 'r--', linewidth=1)
axes[0].plot(traj_small[:, 0], traj_small[:, 1], 'b.-', markersize=2, linewidth=1)
axes[0].plot(traj_small[0, 0], traj_small[0, 1], 'go', markersize=8)
axes[0].plot(traj_small[-1, 0], traj_small[-1, 1], 'rs', markersize=8)
axes[0].set_xlabel('$x_1$')
axes[0].set_ylabel('$x_2$')
axes[0].set_title('Small penalty ($\\lambda=0.5$)')
axes[0].set_xlim(-0.5, 3)
axes[0].set_ylim(-0.5, 4)

axes[1].contour(X1g, X2g, Z2, levels=30, cmap='viridis')
axes[1].contourf(X1g, X2g, feasible2.astype(float), levels=[0.5, 1.5], colors=['lightgreen'], alpha=0.3)
axes[1].plot([0.5, 0.5], [-0.5, 4], 'r--', linewidth=1)
axes[1].plot(x1_curve, x2_curve, 'r--', linewidth=1)
axes[1].plot(traj_large[:, 0], traj_large[:, 1], 'b.-', markersize=2, linewidth=1)
axes[1].plot(traj_large[0, 0], traj_large[0, 1], 'go', markersize=8)
axes[1].plot(traj_large[-1, 0], traj_large[-1, 1], 'rs', markersize=8)
axes[1].set_xlabel('$x_1$')
axes[1].set_ylabel('$x_2$')
axes[1].set_title('Large penalty ($\\lambda=4.0$)')
axes[1].set_xlim(-0.5, 3)
axes[1].set_ylim(-0.5, 4)

g1_small = [g1(traj_small[i]) for i in range(len(traj_small))]
g2_small = [g2(traj_small[i]) for i in range(len(traj_small))]
g1_large = [g1(traj_large[i]) for i in range(len(traj_large))]
g2_large = [g2(traj_large[i]) for i in range(len(traj_large))]

axes[2].plot(g1_small, label='$g_1$ (small $\\lambda$)', linestyle='--')
axes[2].plot(g2_small, label='$g_2$ (small $\\lambda$)', linestyle='--')
axes[2].plot(g1_large, label='$g_1$ (large $\\lambda$)')
axes[2].plot(g2_large, label='$g_2$ (large $\\lambda$)')
axes[2].axhline(0, color='k', linestyle=':', linewidth=0.5)
axes[2].set_xlabel('Iteration')
axes[2].set_ylabel('Constraint value')
axes[2].set_title('Constraint violations')
axes[2].legend(fontsize=8)

plt.tight_layout()
plt.savefig('q2_penalty_plots.png', dpi=150, bbox_inches='tight')
plt.show()

# (e): primal-dual plots
fig, axes = plt.subplots(1, 3, figsize=(15, 4))

axes[0].contour(X1g, X2g, Z2, levels=30, cmap='viridis')
axes[0].contourf(X1g, X2g, feasible2.astype(float), levels=[0.5, 1.5], colors=['lightgreen'], alpha=0.3)
axes[0].plot([0.5, 0.5], [-0.5, 4], 'r--', linewidth=1)
axes[0].plot(x1_curve, x2_curve, 'r--', linewidth=1)
axes[0].plot(traj_pd[:, 0], traj_pd[:, 1], 'b.-', markersize=2, linewidth=1)
axes[0].plot(traj_pd[0, 0], traj_pd[0, 1], 'go', markersize=8)
axes[0].plot(traj_pd[-1, 0], traj_pd[-1, 1], 'rs', markersize=8)
axes[0].set_xlabel('$x_1$')
axes[0].set_ylabel('$x_2$')
axes[0].set_title('Primal-dual trajectory')
axes[0].set_xlim(-0.5, 3)
axes[0].set_ylim(-0.5, 4)

axes[1].plot(traj_pd[:, 0], label='$x_1$')
axes[1].plot(traj_pd[:, 1], label='$x_2$')
axes[1].set_xlabel('Iteration')
axes[1].set_ylabel('Value')
axes[1].set_title('Primal variables')
axes[1].legend()

axes[2].plot(lam_pd[:, 0], label='$\\lambda_1$')
axes[2].plot(lam_pd[:, 1], label='$\\lambda_2$')
axes[2].set_xlabel('Iteration')
axes[2].set_ylabel('Value')
axes[2].set_title('Dual variables (multipliers)')
axes[2].legend()

plt.tight_layout()
plt.savefig('q2_primaldual_plots.png', dpi=150, bbox_inches='tight')
plt.show()

print("\n=== Q2 Results ===")
print(f"Small penalty final: ({traj_small[-1, 0]:.4f}, {traj_small[-1, 1]:.4f}), f = {f2(traj_small[-1]):.4f}")
print(f"  g1 = {g1(traj_small[-1]):.4f}, g2 = {g2(traj_small[-1]):.4f}")
print(f"Large penalty final: ({traj_large[-1, 0]:.4f}, {traj_large[-1, 1]:.4f}), f = {f2(traj_large[-1]):.4f}")
print(f"  g1 = {g1(traj_large[-1]):.4f}, g2 = {g2(traj_large[-1]):.4f}")
print(f"Primal-dual final: ({traj_pd[-1, 0]:.4f}, {traj_pd[-1, 1]:.4f}), f = {f2(traj_pd[-1]):.4f}")
print(f"  g1 = {g1(traj_pd[-1]):.4f}, g2 = {g2(traj_pd[-1]):.4f}")
print(f"  lambda1 = {lam_pd[-1, 0]:.4f}, lambda2 = {lam_pd[-1, 1]:.4f}")


# ---
# QUESTION 3
# ---

# f(x1, x2) = (x1 - 1.5)^2 + (x2 - 1.2)^2
# X = {x1 >= 0.5, x2 >= 0.5, x1 + x2 <= 4, x1 <= 3, x2 <= 3}

def f3(x):
    return (x[0] - 1.5)**2 + (x[1] - 1.2)**2

def grad_f3(x):
    return np.array([2*(x[0] - 1.5), 2*(x[1] - 1.2)])

# (a): vertices of the feasible polytope
vertices = np.array([
    [0.5, 0.5],
    [3.0, 0.5],
    [3.0, 1.0],  # x1=3 and x1+x2=4
    [1.0, 3.0],  # x2=3 and x1+x2=4
    [0.5, 3.0],
])

# (c): Frank-Wolfe
def frank_wolfe(x0, beta, iters):
    x = x0.copy()
    traj = [x.copy()]
    z_hist = [x.copy()]
    for _ in range(iters):
        g = grad_f3(x)
        dots = [g @ v for v in vertices]
        z = vertices[np.argmin(dots)].copy()
        x = beta * x + (1 - beta) * z
        traj.append(x.copy())
        z_hist.append(z.copy())
    return np.array(traj), np.array(z_hist)

x0_q3 = np.array([2.8, 0.8])

traj_fw_08, z_fw_08 = frank_wolfe(x0_q3, 0.8, 100)
traj_fw_095, z_fw_095 = frank_wolfe(x0_q3, 0.95, 100)

# (d): plots for each beta
for beta_val, traj_fw, z_fw, label in [(0.8, traj_fw_08, z_fw_08, 'beta08'),
                                         (0.95, traj_fw_095, z_fw_095, 'beta095')]:
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))

    x1_vals = np.linspace(0, 4, 200)
    x2_vals = np.linspace(0, 4, 200)
    X1g, X2g = np.meshgrid(x1_vals, x2_vals)
    Z3 = (X1g - 1.5)**2 + (X2g - 1.2)**2

    feasible3 = (X1g >= 0.5) & (X2g >= 0.5) & (X1g + X2g <= 4) & (X1g <= 3) & (X2g <= 3)

    axes[0].contour(X1g, X2g, Z3, levels=30, cmap='viridis')
    axes[0].contourf(X1g, X2g, feasible3.astype(float), levels=[0.5, 1.5], colors=['lightgreen'], alpha=0.3)
    verts_closed = np.vstack([vertices, vertices[0]])
    axes[0].plot(verts_closed[:, 0], verts_closed[:, 1], 'r-', linewidth=1.5)
    axes[0].plot(vertices[:, 0], vertices[:, 1], 'ko', markersize=6, label='vertices')
    axes[0].plot(traj_fw[:, 0], traj_fw[:, 1], 'b.-', markersize=3, linewidth=1)
    axes[0].plot(traj_fw[0, 0], traj_fw[0, 1], 'go', markersize=8, label='start')
    axes[0].plot(traj_fw[-1, 0], traj_fw[-1, 1], 'rs', markersize=8, label='end')
    axes[0].set_xlabel('$x_1$')
    axes[0].set_ylabel('$x_2$')
    axes[0].set_title(f'Frank-Wolfe trajectory ($\\beta={beta_val}$)')
    axes[0].legend(fontsize=8)

    f_vals_fw = [f3(traj_fw[i]) for i in range(len(traj_fw))]
    axes[1].plot(f_vals_fw)
    axes[1].set_xlabel('Iteration')
    axes[1].set_ylabel('$f(x^{(t)})$')
    axes[1].set_title(f'Objective vs iteration ($\\beta={beta_val}$)')

    axes[2].plot(traj_fw[:, 0], label='$x_1$')
    axes[2].plot(traj_fw[:, 1], label='$x_2$')
    axes[2].plot(z_fw[:, 0], label='$z_1$', linestyle='--', alpha=0.6)
    axes[2].plot(z_fw[:, 1], label='$z_2$', linestyle='--', alpha=0.6)
    axes[2].set_xlabel('Iteration')
    axes[2].set_ylabel('Value')
    axes[2].set_title(f'$x^{{(t)}}$ and $z^{{(t)}}$ ($\\beta={beta_val}$)')
    axes[2].legend(fontsize=8)

    plt.tight_layout()
    plt.savefig(f'q3_fw_{label}_plots.png', dpi=150, bbox_inches='tight')
    plt.show()

# (e): PGD for comparison
def project_X3(x):
    for _ in range(10):
        x = np.array([np.clip(x[0], 0.5, 3.0), np.clip(x[1], 0.5, 3.0)])
        if x[0] + x[1] > 4:
            excess = (x[0] + x[1] - 4) / 2
            x = np.array([x[0] - excess, x[1] - excess])
        x = np.array([np.clip(x[0], 0.5, 3.0), np.clip(x[1], 0.5, 3.0)])
    return x

def pgd_X3(x0, alpha, iters):
    x = x0.copy()
    traj = [x.copy()]
    for _ in range(iters):
        g = grad_f3(x)
        x = project_X3(x - alpha * g)
        traj.append(x.copy())
    return np.array(traj)

traj_pgd_q3 = pgd_X3(x0_q3, 0.1, 100)

fig, axes = plt.subplots(1, 3, figsize=(15, 4))

axes[0].contour(X1g, X2g, Z3, levels=30, cmap='viridis')
axes[0].contourf(X1g, X2g, feasible3.astype(float), levels=[0.5, 1.5], colors=['lightgreen'], alpha=0.3)
axes[0].plot(verts_closed[:, 0], verts_closed[:, 1], 'r-', linewidth=1.5)
axes[0].plot(traj_pgd_q3[:, 0], traj_pgd_q3[:, 1], 'b.-', markersize=3, linewidth=1)
axes[0].plot(traj_pgd_q3[0, 0], traj_pgd_q3[0, 1], 'go', markersize=8, label='start')
axes[0].plot(traj_pgd_q3[-1, 0], traj_pgd_q3[-1, 1], 'rs', markersize=8, label='end')
axes[0].set_xlabel('$x_1$')
axes[0].set_ylabel('$x_2$')
axes[0].set_title('PGD trajectory (Q3)')
axes[0].legend()

f_vals_pgd_q3 = [f3(traj_pgd_q3[i]) for i in range(len(traj_pgd_q3))]
axes[1].plot(f_vals_pgd_q3)
axes[1].set_xlabel('Iteration')
axes[1].set_ylabel('$f(x^{(t)})$')
axes[1].set_title('Objective vs iteration (PGD)')

axes[2].plot(traj_pgd_q3[:, 0], label='$x_1$')
axes[2].plot(traj_pgd_q3[:, 1], label='$x_2$')
axes[2].set_xlabel('Iteration')
axes[2].set_ylabel('Value')
axes[2].set_title('Variables vs iteration (PGD)')
axes[2].legend()

plt.tight_layout()
plt.savefig('q3_pgd_plots.png', dpi=150, bbox_inches='tight')
plt.show()

print("\n=== Q3 Results ===")
print(f"Frank-Wolfe (beta=0.8) final: ({traj_fw_08[-1, 0]:.4f}, {traj_fw_08[-1, 1]:.4f}), f = {f3(traj_fw_08[-1]):.4f}")
print(f"Frank-Wolfe (beta=0.95) final: ({traj_fw_095[-1, 0]:.4f}, {traj_fw_095[-1, 1]:.4f}), f = {f3(traj_fw_095[-1]):.4f}")
print(f"PGD final: ({traj_pgd_q3[-1, 0]:.4f}, {traj_pgd_q3[-1, 1]:.4f}), f = {f3(traj_pgd_q3[-1]):.4f}")
print(f"Unconstrained min at (1.5, 1.2), which is inside X, so optimal f = 0")