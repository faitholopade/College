import numpy as np
import sympy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


# Question 1
# (a) Compute df/dx using SymPy
print("QUESTION 1")
print("="*60)
print("(a) Computing derivative using SymPy")
print("="*60)

x = sympy.symbols('x', real=True)
f_symbolic = x**4
df_symbolic = sympy.diff(f_symbolic, x)

print(f"f(x) = {f_symbolic}")
print(f"df/dx = {df_symbolic}")
print()

# Convert to numerical functions
f_func = sympy.lambdify(x, f_symbolic, 'numpy')
df_func = sympy.lambdify(x, df_symbolic, 'numpy')

# (b) Exact vs Finite Difference
print("="*60)
print("(b) Exact derivative vs finite difference approximation")
print("="*60)

x_range = np.linspace(-2, 2, 100)
delta = 0.01

# Exact derivative
exact_derivative = df_func(x_range)

# Forward finite difference approximation
finite_diff = (f_func(x_range + delta) - f_func(x_range)) / delta

# Plot
plt.figure(figsize=(10, 6))
plt.plot(x_range, exact_derivative, 'b-', linewidth=2, label='Exact derivative (4x³)')
plt.plot(x_range, finite_diff, 'r--', linewidth=2, label=f'Finite difference (δ={delta})')
plt.xlabel('x', fontsize=12)
plt.ylabel('df/dx', fontsize=12)
plt.title('Exact Derivative vs Finite Difference Approximation', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('q1_b.png', dpi=150, bbox_inches='tight')
plt.show()

print(f"Plot saved as 'q1_b.png'")
print()

# (c) Mean Absolute Error vs delta
print("="*60)
print("(c) Mean Absolute Error vs delta")
print("="*60)

delta_values = np.logspace(-3, 0, 50)  # From 0.001 to 1
mae_values = []

x_test = np.linspace(-2, 2, 100)
exact_deriv_test = df_func(x_test)

for delta in delta_values:
    finite_diff_approx = (f_func(x_test + delta) - f_func(x_test)) / delta
    mae = np.mean(np.abs(exact_deriv_test - finite_diff_approx))
    mae_values.append(mae)

# Plot MAE vs delta
plt.figure(figsize=(10, 6))
plt.loglog(delta_values, mae_values, 'b-', linewidth=2, marker='o', markersize=4)
plt.xlabel('δ (perturbation size)', fontsize=12)
plt.ylabel('Mean Absolute Error (MAE)', fontsize=12)
plt.title('MAE of Finite Difference Approximation vs δ', fontsize=14)
plt.grid(True, alpha=0.3, which='both')
plt.tight_layout()
plt.savefig('q1_c.png', dpi=150, bbox_inches='tight')
plt.show()

print(f"Plot saved as 'q1_c.png'")
print(f"Minimum MAE: {min(mae_values):.6e} at δ = {delta_values[np.argmin(mae_values)]:.6f}")
print()

# (d) & (e) Gradient Descent
print("="*60)
print("(d) & (e) Gradient Descent Implementation")
print("="*60)

def gradient_descent(f, df, x0, alpha, num_iters=100):
    """
    Gradient descent algorithm
    
    Parameters:
    - f: function to minimize
    - df: derivative of f
    - x0: initial value
    - alpha: step size (learning rate)
    - num_iters: number of iterations
    
    Returns:
    - X: array of x values at each iteration
    - F: array of f(x) values at each iteration
    - diverged: boolean indicating if algorithm diverged
    """
    x = x0
    X = np.array([x])
    F = np.array([f(x)])
    diverged = False
    
    for k in range(num_iters):
        step = alpha * df(x)
        x = x - step
        
        # Check for overflow/divergence
        if np.abs(x) > 1e10:
            diverged = True
            print(f"  WARNING: Diverged at iteration {k+1} (x = {x:.2e})")
            break
        
        try:
            f_val = f(x)
            # Check if function value is too large
            if np.abs(f_val) > 1e10:
                diverged = True
                print(f"  WARNING: Function value too large at iteration {k+1} (f(x) = {f_val:.2e})")
                break
        except (OverflowError, RuntimeWarning):
            diverged = True
            print(f"  WARNING: Overflow at iteration {k+1}")
            break
            
        X = np.append(X, x)
        F = np.append(F, f_val)
    
    return X, F, diverged

# Initial value and step sizes
x0 = 1.0
alpha_values = [0.05, 0.5, 1.2]
num_iters = 50

results = {}
for alpha in alpha_values:
    X, F, diverged = gradient_descent(f_func, df_func, x0, alpha, num_iters)
    results[alpha] = {'X': X, 'F': F, 'diverged': diverged}
    
    if diverged:
        print(f"Alpha = {alpha}: DIVERGED after {len(X)-1} iterations")
    else:
        print(f"Alpha = {alpha}: Final x = {X[-1]:.6f}, Final f(x) = {F[-1]:.6e}")

print()

# Plotting Results

# Plot 1: x_k vs iteration
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
for alpha in alpha_values:
    linestyle = '--' if results[alpha]['diverged'] else '-'
    plt.plot(results[alpha]['X'], linewidth=2, marker='o', markersize=3, 
             linestyle=linestyle, label=f'α = {alpha}' + (' (diverged)' if results[alpha]['diverged'] else ''))
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('x', fontsize=12)
plt.title('Evolution of x over iterations', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)

# Plot 2: f(x_k) vs iteration
plt.subplot(1, 2, 2)
for alpha in alpha_values:
    linestyle = '--' if results[alpha]['diverged'] else '-'
    plt.plot(results[alpha]['F'], linewidth=2, marker='o', markersize=3, 
             linestyle=linestyle, label=f'α = {alpha}' + (' (diverged)' if results[alpha]['diverged'] else ''))
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('Function value over iterations', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)

plt.tight_layout()
plt.savefig('q1_e_linear.png', dpi=150, bbox_inches='tight')
plt.show()

# Plot 3: f(x_k) vs iteration (log scale) - only for converged cases
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
for alpha in alpha_values:
    if not results[alpha]['diverged']:
        # Only plot positive values on log scale
        F_positive = np.maximum(results[alpha]['F'], 1e-15)
        plt.semilogy(F_positive, linewidth=2, marker='o', markersize=3, 
                     label=f'α = {alpha}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x) [log scale]', fontsize=12)
plt.title('Function value over iterations (log scale)', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3, which='both')

# Plot 4: Trajectory on function
plt.subplot(1, 2, 2)
x_plot = np.linspace(-2, 2, 200)
plt.plot(x_plot, f_func(x_plot), 'k-', linewidth=1.5, label='f(x) = x⁴')
for alpha in alpha_values:
    # Only plot trajectories that stay within reasonable bounds
    X_data = results[alpha]['X']
    F_data = results[alpha]['F']
    
    # Filter points within plotting range
    mask = (X_data >= -2) & (X_data <= 2) & (F_data <= 5)
    X_filtered = X_data[mask]
    F_filtered = F_data[mask]
    
    if len(X_filtered) > 0:
        linestyle = '--' if results[alpha]['diverged'] else '-'
        plt.plot(X_filtered, F_filtered, linewidth=2, marker='o', markersize=5, 
                 linestyle=linestyle, label=f'α = {alpha}' + (' (diverged)' if results[alpha]['diverged'] else ''))

plt.xlabel('x', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('Gradient descent trajectory on f(x)', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.xlim(-2, 2)
plt.ylim(-0.5, 5)

plt.tight_layout()
plt.savefig('q1_e_detailed.png', dpi=150, bbox_inches='tight')
plt.show()

print("="*60)
print("All Question 1 plots saved successfully!")
print("="*60)

# ========================================================================

# Question 2
print("QUESTION 2")

def f_2d(x0, x1):
    """Function f(x0, x1) = 0.5(x0^2 + 10*x1^2)"""
    return 0.5 * (x0**2 + 10 * x1**2)

def grad_f_2d(x):
    """Gradient of f: [x0, 10*x1]"""
    return np.array([x[0], 10 * x[1]])

# Create grid for contour plot
x0_range = np.linspace(-2, 2, 400)
x1_range = np.linspace(-2, 2, 400)
X0, X1 = np.meshgrid(x0_range, x1_range)
Z = f_2d(X0, X1)

# Plot contours
plt.figure(figsize=(10, 8))
contour = plt.contour(X0, X1, Z, levels=20, cmap='viridis')
plt.colorbar(contour, label='f(x₀, x₁)')
plt.xlabel('x₀', fontsize=12)
plt.ylabel('x₁', fontsize=12)
plt.title('Contour plot of f(x₀, x₁) = 0.5(x₀² + 10x₁²)', fontsize=14)
plt.grid(True, alpha=0.3)
plt.axis('equal')
plt.xlim(-2, 2)
plt.ylim(-2, 2)
plt.tight_layout()
plt.savefig('q2_I_a_contour.png', dpi=150, bbox_inches='tight')
plt.show()

print("(a) Contour plot saved as 'q2_I_a_contour.png'")
print()

# (b) Gradient descent with two learning rates

def gradient_descent_2d(f, grad_f, x0, alpha, num_iters=100):
    """Gradient descent for 2D function"""
    x = x0.copy()
    X_history = [x.copy()]
    F_history = [f(x[0], x[1])]
    
    for k in range(num_iters):
        grad = grad_f(x)
        x = x - alpha * grad
        X_history.append(x.copy())
        F_history.append(f(x[0], x[1]))
    
    return np.array(X_history), np.array(F_history)

# Initial point
x_init = np.array([1.5, 1.5])

# Run gradient descent with different learning rates
alpha_values = [0.05, 0.2]
results_2d = {}

print("="*60)
print("(b) Gradient descent with α = 0.05 and α = 0.2")
print("="*60)

for alpha in alpha_values:
    X_hist, F_hist = gradient_descent_2d(f_2d, grad_f_2d, x_init, alpha, num_iters=100)
    results_2d[alpha] = {'X': X_hist, 'F': F_hist}
    print(f"α = {alpha}: Final point = [{X_hist[-1][0]:.6f}, {X_hist[-1][1]:.6f}], "
          f"Final f(x) = {F_hist[-1]:.6e}")

print()

# Plot contours with gradient descent paths overlaid
plt.figure(figsize=(10, 8))
contour = plt.contour(X0, X1, Z, levels=20, cmap='viridis', alpha=0.6)
plt.colorbar(contour, label='f(x₀, x₁)')

# Overlay gradient descent paths
colors = {0.05: 'red', 0.2: 'blue'}
for alpha in alpha_values:
    X_hist = results_2d[alpha]['X']
    plt.plot(X_hist[:, 0], X_hist[:, 1], 'o-', color=colors[alpha], 
             linewidth=2, markersize=4, label=f'α = {alpha}')

# Mark starting point
plt.plot(x_init[0], x_init[1], 'go', markersize=12, label='Start', zorder=5)

plt.xlabel('x₀', fontsize=12)
plt.ylabel('x₁', fontsize=12)
plt.title('Gradient descent paths: narrow valley effect', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.axis('equal')
plt.xlim(-2, 2)
plt.ylim(-2, 2)
plt.tight_layout()
plt.savefig('q2_I_b_paths.png', dpi=150, bbox_inches='tight')
plt.show()

print("Gradient descent paths saved as 'q2_I_b_paths.png'")
print()

# Plot convergence
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
for alpha in alpha_values:
    plt.semilogy(results_2d[alpha]['F'], linewidth=2, marker='o', 
                 markersize=3, color=colors[alpha], label=f'α = {alpha}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x) [log scale]', fontsize=12)
plt.title('Function value convergence', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3, which='both')

plt.subplot(1, 2, 2)
for alpha in alpha_values:
    X_hist = results_2d[alpha]['X']
    distance = np.sqrt(X_hist[:, 0]**2 + X_hist[:, 1]**2)
    plt.semilogy(distance, linewidth=2, marker='o', markersize=3, 
                 color=colors[alpha], label=f'α = {alpha}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('Distance from origin [log scale]', fontsize=12)
plt.title('Distance to minimum', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3, which='both')

plt.tight_layout()
plt.savefig('q2_I_b_convergence.png', dpi=150, bbox_inches='tight')
plt.show()

print("Convergence plots saved as 'q2_I_b_convergence.png'")
print()

# (II)(a) Plot function

def f_nonconvex(x):
    """Function f(x) = x^4 - 2x^2 + 0.1x"""
    return x**4 - 2*x**2 + 0.1*x

def df_nonconvex(x):
    """Derivative of f(x) = 4x^3 - 4x + 0.1"""
    return 4*x**3 - 4*x + 0.1

x_plot = np.linspace(-2, 2, 400)
y_plot = f_nonconvex(x_plot)

plt.figure(figsize=(10, 6))
plt.plot(x_plot, y_plot, 'b-', linewidth=2)
plt.xlabel('x', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('f(x) = x⁴ - 2x² + 0.1x', fontsize=14)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=0.5, alpha=0.5)
plt.tight_layout()
plt.savefig('q2_II_a_function.png', dpi=150, bbox_inches='tight')
plt.show()

print("(a) Function plot saved as 'q2_II_a_function.png'")
print()

# (b) Gradient descent from two starting points

def gradient_descent_1d(f, df, x0, alpha, num_iters=100):
    """Gradient descent for 1D function"""
    x = x0
    X_history = [x]
    F_history = [f(x)]
    
    for k in range(num_iters):
        grad = df(x)
        x = x - alpha * grad
        X_history.append(x)
        F_history.append(f(x))
    
    return np.array(X_history), np.array(F_history)

# Two starting points
x0_values = [-1.5, 1.5]
alpha = 0.05
results_nonconvex = {}

print("="*60)
print("(b) Gradient descent from x₀ = -1.5 and x₀ = 1.5 (α = 0.05)")
print("="*60)

for x0 in x0_values:
    X_hist, F_hist = gradient_descent_1d(f_nonconvex, df_nonconvex, x0, alpha, num_iters=100)
    results_nonconvex[x0] = {'X': X_hist, 'F': F_hist}
    print(f"x₀ = {x0:>5.1f}: Final x = {X_hist[-1]:>8.6f}, Final f(x) = {F_hist[-1]:>9.6f}")

print()

# Plot function with trajectories
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
plt.plot(x_plot, y_plot, 'k-', linewidth=2, label='f(x)')
colors_nc = {-1.5: 'red', 1.5: 'blue'}
for x0 in x0_values:
    X_hist = results_nonconvex[x0]['X']
    F_hist = results_nonconvex[x0]['F']
    plt.plot(X_hist, F_hist, 'o-', color=colors_nc[x0], linewidth=2, 
             markersize=4, label=f'x₀ = {x0}', alpha=0.7)
    plt.plot(X_hist[0], F_hist[0], 'o', color=colors_nc[x0], markersize=10)

plt.xlabel('x', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('Gradient descent trajectories on f(x)', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)

plt.subplot(1, 2, 2)
for x0 in x0_values:
    plt.plot(results_nonconvex[x0]['F'], linewidth=2, marker='o', 
             markersize=3, color=colors_nc[x0], label=f'x₀ = {x0}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('Function value over iterations', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('q2_II_b_local_minima.png', dpi=150, bbox_inches='tight')
plt.show()

print("Local minima comparison saved as 'q2_II_b_local_minima.png'")
print()

print("="*60)
print("All Question 2 plots saved successfully!")
print("="*60)

# ========================================================================


# Question 3
print("QUESTION 3")

def f_quadratic(x):
    """Function f(x) = x^2"""
    return x**2

def df_quadratic(x):
    """Derivative f'(x) = 2x"""
    return 2*x

def gradient_descent_1d(f, df, x0, alpha, num_iters=50):
    """Gradient descent for 1D function"""
    x = x0
    X_history = [x]
    F_history = [f(x)]
    
    for k in range(num_iters):
        grad = df(x)
        x = x - alpha * grad
        
        # Check for overflow/divergence
        if np.abs(x) > 1e10 or np.abs(f(x)) > 1e10:
            print(f"  WARNING: Diverged at iteration {k+1}")
            break
            
        X_history.append(x)
        F_history.append(f(x))
    
    return np.array(X_history), np.array(F_history)

# (a) Run gradient descent with different alphas
x0 = 1.0
alpha_values = [0.1, 0.01, 1.01]
results_3I = {}

print("(a) Gradient descent results")
print("-"*60)

for alpha in alpha_values:
    X_hist, F_hist = gradient_descent_1d(f_quadratic, df_quadratic, x0, alpha, num_iters=50)
    results_3I[alpha] = {'X': X_hist, 'F': F_hist}
    
    if len(X_hist) < 51:  # Diverged
        print(f"α = {alpha}: DIVERGED after {len(X_hist)-1} iterations")
    else:
        print(f"α = {alpha}: Final x = {X_hist[-1]:.6f}, Final f(x) = {F_hist[-1]:.6e}")

print()

# (b) Plot xk and f(xk) versus iteration
plt.figure(figsize=(14, 10))

# Plot 1: x_k vs iteration
plt.subplot(2, 2, 1)
colors_3I = {0.1: 'blue', 0.01: 'red', 1.01: 'green'}
for alpha in alpha_values:
    linestyle = '--' if len(results_3I[alpha]['X']) < 51 else '-'
    label_suffix = ' (diverged)' if len(results_3I[alpha]['X']) < 51 else ''
    plt.plot(results_3I[alpha]['X'], linewidth=2, marker='o', markersize=3,
             color=colors_3I[alpha], linestyle=linestyle, label=f'α = {alpha}{label_suffix}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('x', fontsize=12)
plt.title('Evolution of x over iterations', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)

# Plot 2: f(x_k) vs iteration (linear scale)
plt.subplot(2, 2, 2)
for alpha in alpha_values:
    linestyle = '--' if len(results_3I[alpha]['X']) < 51 else '-'
    label_suffix = ' (diverged)' if len(results_3I[alpha]['X']) < 51 else ''
    plt.plot(results_3I[alpha]['F'], linewidth=2, marker='o', markersize=3,
             color=colors_3I[alpha], linestyle=linestyle, label=f'α = {alpha}{label_suffix}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('Function value over iterations (linear scale)', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)

# Plot 3: f(x_k) vs iteration (log scale) - only converged cases
plt.subplot(2, 2, 3)
for alpha in [0.1, 0.01]:  # Only converged cases
    F_positive = np.maximum(results_3I[alpha]['F'], 1e-15)
    plt.semilogy(F_positive, linewidth=2, marker='o', markersize=3,
                 color=colors_3I[alpha], label=f'α = {alpha}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x) [log scale]', fontsize=12)
plt.title('Function value over iterations (log scale)', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3, which='both')

# Plot 4: Trajectory on function
plt.subplot(2, 2, 4)
x_plot = np.linspace(-1.5, 1.5, 200)
plt.plot(x_plot, f_quadratic(x_plot), 'k-', linewidth=1.5, label='f(x) = x²')
for alpha in alpha_values:
    X_data = results_3I[alpha]['X']
    F_data = results_3I[alpha]['F']
    
    # Filter points within plotting range
    mask = (X_data >= -1.5) & (X_data <= 1.5) & (F_data <= 3)
    X_filtered = X_data[mask]
    F_filtered = F_data[mask]
    
    if len(X_filtered) > 0:
        linestyle = '--' if len(results_3I[alpha]['X']) < 51 else '-'
        label_suffix = ' (diverged)' if len(results_3I[alpha]['X']) < 51 else ''
        plt.plot(X_filtered, F_filtered, linewidth=2, marker='o', markersize=5,
                 color=colors_3I[alpha], linestyle=linestyle, label=f'α = {alpha}{label_suffix}')

plt.xlabel('x', fontsize=12)
plt.ylabel('f(x)', fontsize=12)
plt.title('Gradient descent trajectories on f(x)', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.xlim(-1.5, 1.5)
plt.ylim(-0.1, 3)

plt.tight_layout()
plt.savefig('q3_I_gradient_descent.png', dpi=150, bbox_inches='tight')
plt.show()

print("Plots saved as 'q3_I_gradient_descent.png'")
print()

def f_scaled_quadratic(x, gamma):
    """Function f(x) = γx^2"""
    return gamma * x**2

def df_scaled_quadratic(x, gamma):
    """Derivative f'(x) = 2γx"""
    return 2 * gamma * x

# (a) Run gradient descent for different gamma values
x0 = 1.0
alpha = 0.1
gamma_values = [0.5, 1, 2, 5]
results_3II = {}

print("(a) Gradient descent results for different γ")
print("-"*60)

for gamma in gamma_values:
    X_hist, F_hist = gradient_descent_1d(
        lambda x: f_scaled_quadratic(x, gamma),
        lambda x: df_scaled_quadratic(x, gamma),
        x0, alpha, num_iters=50
    )
    results_3II[gamma] = {'X': X_hist, 'F': F_hist}
    print(f"γ = {gamma}: Final x = {X_hist[-1]:.6f}, Final f(x) = {F_hist[-1]:.6e}")

print()

# (b) Plot f(x_k) versus iteration (log scale)
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
colors_3II = {0.5: 'blue', 1: 'green', 2: 'orange', 5: 'red'}
for gamma in gamma_values:
    F_positive = np.maximum(results_3II[gamma]['F'], 1e-15)
    plt.semilogy(F_positive, linewidth=2, marker='o', markersize=3,
                 color=colors_3II[gamma], label=f'γ = {gamma}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x) [log scale]', fontsize=12)
plt.title('Function value convergence for different γ', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3, which='both')

plt.subplot(1, 2, 2)
for gamma in gamma_values:
    plt.plot(results_3II[gamma]['X'], linewidth=2, marker='o', markersize=3,
             color=colors_3II[gamma], label=f'γ = {gamma}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('x', fontsize=12)
plt.title('Evolution of x for different γ', fontsize=14)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)

plt.tight_layout()
plt.savefig('q3_II_gamma_effect.png', dpi=150, bbox_inches='tight')
plt.show()

print("Plots saved as 'q3_II_gamma_effect.png'")
print()

def f_absolute(x):
    """Function f(x) = |x|"""
    return np.abs(x)

def subgrad_absolute(x):
    """Subgradient g(x) = sign(x), g(0) = 0"""
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0

# (a) Run gradient descent
x0 = 1.0
alpha = 0.1
num_iters = 60

print("(a) Gradient descent with subgradient")
print("-"*60)

x = x0
X_history = [x]
F_history = [f_absolute(x)]

for k in range(num_iters):
    grad = subgrad_absolute(x)
    x = x - alpha * grad
    X_history.append(x)
    F_history.append(f_absolute(x))

X_history = np.array(X_history)
F_history = np.array(F_history)

print(f"Initial: x = {X_history[0]:.6f}, f(x) = {F_history[0]:.6f}")
print(f"Final:   x = {X_history[-1]:.6f}, f(x) = {F_history[-1]:.6f}")
print(f"Oscillates between x = ±{alpha:.1f} after reaching vicinity of minimum")
print()

# (b) Plot xk and f(xk) versus iteration
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
plt.plot(X_history, linewidth=2, marker='o', markersize=4, color='blue')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('x', fontsize=12)
plt.title('Evolution of x over iterations', fontsize=14)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)
plt.axhline(y=0.1, color='r', linestyle=':', linewidth=1, alpha=0.5, label='x = ±0.1')
plt.axhline(y=-0.1, color='r', linestyle=':', linewidth=1, alpha=0.5)
plt.legend(fontsize=10)

plt.subplot(1, 2, 2)
plt.plot(F_history, linewidth=2, marker='o', markersize=4, color='red')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x) = |x|', fontsize=12)
plt.title('Function value over iterations', fontsize=14)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='--', linewidth=1, alpha=0.5)
plt.axhline(y=0.1, color='b', linestyle=':', linewidth=1, alpha=0.5, label='f(x) = 0.1')
plt.legend(fontsize=10)

plt.tight_layout()
plt.savefig('q3_III_absolute_value.png', dpi=150, bbox_inches='tight')
plt.show()

print("Plots saved as 'q3_III_absolute_value.png'")
print()

print("="*60)
print("All Question 3 plots saved successfully!")
print("="*60)


#Question 4
print("QUESTION 4")

def f_quadratic_2d(x1, x2, gamma):
    """Function f(x1, x2) = x1^2 + γx2^2"""
    return x1**2 + gamma * x2**2

def grad_f_quadratic_2d(x, gamma):
    """Gradient ∇f = [2x1, 2γx2]"""
    return np.array([2*x[0], 2*gamma*x[1]])

# Create grid for contour plots
x1_range = np.linspace(-1.5, 1.5, 400)
x2_range = np.linspace(-1.5, 1.5, 400)
X1, X2 = np.meshgrid(x1_range, x2_range)

# Plot contours for γ = 1 and γ = 4
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

gamma_values = [1, 4]
for idx, gamma in enumerate(gamma_values):
    Z = f_quadratic_2d(X1, X2, gamma)
    
    ax = axes[idx]
    contour = ax.contour(X1, X2, Z, levels=20, cmap='viridis')
    ax.contourf(X1, X2, Z, levels=20, cmap='viridis', alpha=0.3)
    plt.colorbar(contour, ax=ax, label='f(x₁, x₂)')
    ax.set_xlabel('x₁', fontsize=12)
    ax.set_ylabel('x₂', fontsize=12)
    ax.set_title(f'Contours of f(x₁, x₂) = x₁² + {gamma}x₂²', fontsize=14)
    ax.grid(True, alpha=0.3)
    ax.set_aspect('equal')

plt.tight_layout()
plt.savefig('q4_a_contours.png', dpi=150, bbox_inches='tight')
plt.show()

print("Contour plots saved as 'q4_a_contours.png'")
print()

print("="*60)
print("(b) Gradient descent with α = 0.1")
print("="*60)

def gradient_descent_2d(f, grad_f, x0, alpha, num_iters=100):
    """Gradient descent for 2D function"""
    x = x0.copy()
    X_history = [x.copy()]
    F_history = [f(x[0], x[1])]
    
    for k in range(num_iters):
        grad = grad_f(x)
        x = x - alpha * grad
        X_history.append(x.copy())
        F_history.append(f(x[0], x[1]))
    
    return np.array(X_history), np.array(F_history)

x0 = np.array([1.0, 1.0])
alpha = 0.1
results_4b = {}

for gamma in gamma_values:
    X_hist, F_hist = gradient_descent_2d(
        lambda x1, x2: f_quadratic_2d(x1, x2, gamma),
        lambda x: grad_f_quadratic_2d(x, gamma),
        x0, alpha, num_iters=100
    )
    results_4b[gamma] = {'X': X_hist, 'F': F_hist}
    print(f"γ = {gamma}: Final point = [{X_hist[-1][0]:.6f}, {X_hist[-1][1]:.6f}], "
          f"Final f(x) = {F_hist[-1]:.6e}")

print()

# Plot contours with gradient descent paths overlaid
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

for idx, gamma in enumerate(gamma_values):
    Z = f_quadratic_2d(X1, X2, gamma)
    
    ax = axes[idx]
    contour = ax.contour(X1, X2, Z, levels=20, cmap='viridis', alpha=0.6)
    plt.colorbar(contour, ax=ax, label='f(x₁, x₂)')
    
    # Overlay gradient descent path
    X_hist = results_4b[gamma]['X']
    ax.plot(X_hist[:, 0], X_hist[:, 1], 'r-o', linewidth=2, markersize=4, 
            label=f'GD path (α={alpha})')
    ax.plot(x0[0], x0[1], 'go', markersize=12, label='Start', zorder=5)
    
    ax.set_xlabel('x₁', fontsize=12)
    ax.set_ylabel('x₂', fontsize=12)
    ax.set_title(f'Gradient descent on f(x₁, x₂) = x₁² + {gamma}x₂²', fontsize=14)
    ax.legend(fontsize=11)
    ax.grid(True, alpha=0.3)
    ax.set_aspect('equal')
    ax.set_xlim(-1.5, 1.5)
    ax.set_ylim(-1.5, 1.5)

plt.tight_layout()
plt.savefig('q4_b_paths.png', dpi=150, bbox_inches='tight')
plt.show()

print("Gradient descent paths saved as 'q4_b_paths.png'")
print()

print("="*60)
print("(c) Rosenbrock function contours")
print("="*60)

def f_rosenbrock(x1, x2):
    """Rosenbrock function f(x1, x2) = (1 - x1)^2 + 100(x2 - x1^2)^2"""
    return (1 - x1)**2 + 100 * (x2 - x1**2)**2

def grad_f_rosenbrock(x):
    """Gradient of Rosenbrock function"""
    x1, x2 = x[0], x[1]
    df_dx1 = -2*(1 - x1) + 100*2*(x2 - x1**2)*(-2*x1)
    df_dx2 = 100*2*(x2 - x1**2)
    return np.array([df_dx1, df_dx2])

# Create grid for Rosenbrock function
x1_range_rb = np.linspace(-2, 2, 400)
x2_range_rb = np.linspace(-1, 3, 400)
X1_rb, X2_rb = np.meshgrid(x1_range_rb, x2_range_rb)
Z_rb = f_rosenbrock(X1_rb, X2_rb)

# Plot contours with log scale for better visualization
plt.figure(figsize=(10, 8))
levels = np.logspace(-1, 3.5, 20)
contour = plt.contour(X1_rb, X2_rb, Z_rb, levels=levels, cmap='viridis')
plt.contourf(X1_rb, X2_rb, Z_rb, levels=levels, cmap='viridis', alpha=0.3)
plt.colorbar(contour, label='f(x₁, x₂) [log scale]')
plt.xlabel('x₁', fontsize=12)
plt.ylabel('x₂', fontsize=12)
plt.title('Rosenbrock function: f(x₁, x₂) = (1 - x₁)² + 100(x₂ - x₁²)²', fontsize=14)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('q4_c_rosenbrock_contours.png', dpi=150, bbox_inches='tight')
plt.show()

print("Rosenbrock contours saved as 'q4_c_rosenbrock_contours.png'")
print()

print("="*60)
print("(d) Gradient descent on Rosenbrock function")
print("="*60)

x0_rb = np.array([-1.25, 0.5])
alpha_values_rb = [0.001, 0.005]
num_iters_rb = 2000
results_4d = {}

for alpha in alpha_values_rb:
    X_hist, F_hist = gradient_descent_2d(
        f_rosenbrock,
        grad_f_rosenbrock,
        x0_rb, alpha, num_iters=num_iters_rb
    )
    results_4d[alpha] = {'X': X_hist, 'F': F_hist}
    print(f"α = {alpha}: Final point = [{X_hist[-1][0]:.6f}, {X_hist[-1][1]:.6f}], "
          f"Final f(x) = {F_hist[-1]:.6e}")

print()

# Plot Rosenbrock contours with gradient descent paths
plt.figure(figsize=(12, 9))

# Main plot: contours with paths
plt.subplot(2, 2, 1)
levels = np.logspace(-1, 3.5, 20)
contour = plt.contour(X1_rb, X2_rb, Z_rb, levels=levels, cmap='viridis', alpha=0.6)
plt.colorbar(contour, label='f(x₁, x₂)')

colors_rb = {0.001: 'red', 0.005: 'blue'}
for alpha in alpha_values_rb:
    X_hist = results_4d[alpha]['X']
    plt.plot(X_hist[:, 0], X_hist[:, 1], '-', color=colors_rb[alpha], 
             linewidth=1.5, alpha=0.7, label=f'α = {alpha}')

plt.plot(x0_rb[0], x0_rb[1], 'go', markersize=12, label='Start', zorder=5)
plt.plot(1, 1, 'r*', markersize=15, label='Global min (1,1)', zorder=5)
plt.xlabel('x₁', fontsize=12)
plt.ylabel('x₂', fontsize=12)
plt.title('Gradient descent paths on Rosenbrock function', fontsize=13)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.xlim(-2, 2)
plt.ylim(-1, 3)

# Plot function values over iterations (log scale)
plt.subplot(2, 2, 2)
for alpha in alpha_values_rb:
    plt.semilogy(results_4d[alpha]['F'], linewidth=2, 
                 color=colors_rb[alpha], label=f'α = {alpha}')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('f(x) [log scale]', fontsize=12)
plt.title('Function value convergence', fontsize=13)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3, which='both')

# Plot x1 component over iterations
plt.subplot(2, 2, 3)
for alpha in alpha_values_rb:
    X_hist = results_4d[alpha]['X']
    plt.plot(X_hist[:, 0], linewidth=2, color=colors_rb[alpha], label=f'α = {alpha}')
plt.axhline(y=1, color='k', linestyle='--', linewidth=1, alpha=0.5, label='Target x₁=1')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('x₁', fontsize=12)
plt.title('x₁ component evolution', fontsize=13)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)

# Plot x2 component over iterations
plt.subplot(2, 2, 4)
for alpha in alpha_values_rb:
    X_hist = results_4d[alpha]['X']
    plt.plot(X_hist[:, 1], linewidth=2, color=colors_rb[alpha], label=f'α = {alpha}')
plt.axhline(y=1, color='k', linestyle='--', linewidth=1, alpha=0.5, label='Target x₂=1')
plt.xlabel('Iteration', fontsize=12)
plt.ylabel('x₂', fontsize=12)
plt.title('x₂ component evolution', fontsize=13)
plt.legend(fontsize=11)
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('q4_d_rosenbrock_descent.png', dpi=150, bbox_inches='tight')
plt.show()

print("Rosenbrock gradient descent results saved as 'q4_d_rosenbrock_descent.png'")
print()

print("="*60)
print("All Question 4 plots saved successfully!")
print("="*60)