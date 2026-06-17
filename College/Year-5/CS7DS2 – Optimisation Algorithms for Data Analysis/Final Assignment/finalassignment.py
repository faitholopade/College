import numpy as np
import matplotlib.pyplot as plt

np.random.seed(0)

# benchmarks
m = 1000
X_data = np.random.randn(m, 2)
theta_star = np.array([3.0, 4.0])
noise = np.random.randn(m)
y_data = X_data @ theta_star + noise

def f_A(theta):
    r = X_data @ theta - y_data
    return (r @ r) / (2*m)

def grad_A(theta):
    r = X_data @ theta - y_data
    return (X_data.T @ r) / m

def f_B(x):
    return (x[0]-1)**2 + 5*(x[1]-2)**2 + np.sin(x[0])

def grad_B(x):
    return np.array([2*(x[0]-1) + np.cos(x[0]), 10*(x[1]-2)])

def f_C(x):
    return (1-x[0])**2 + 100*(x[1]-x[0]**2)**2

def grad_C(x):
    return np.array([-2*(1-x[0]) - 400*x[0]*(x[1]-x[0]**2),
                     200*(x[1]-x[0]**2)])

x0_A = np.array([0.0, 0.0])
x0_B = np.array([-1.0, -1.0])
x0_C = np.array([-1.2, 1.0])


# Q1

def gd(f, grad, x0, alpha, iters):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    for _ in range(iters):
        x = x - alpha*grad(x)
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist)

def polyak(f, grad, x0, fstar, eps, iters):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    steps = []
    for _ in range(iters):
        g = grad(x)
        a = (f(x) - fstar) / (g @ g + eps)
        steps.append(a)
        x = x - a*g
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist), np.array(steps)

def adagrad(f, grad, x0, a0, eps, iters):
    x = x0.copy()
    G = np.zeros_like(x)
    hist = [f(x)]
    xs = [x.copy()]
    steps = []
    for _ in range(iters):
        g = grad(x)
        G += g*g
        a = a0 / (np.sqrt(G) + eps)
        steps.append(np.linalg.norm(a))
        x = x - a*g
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist), np.array(steps)

def rmsprop(f, grad, x0, a0, beta, eps, iters):
    x = x0.copy()
    v = np.zeros_like(x)
    hist = [f(x)]
    xs = [x.copy()]
    steps = []
    for _ in range(iters):
        g = grad(x)
        v = beta*v + (1-beta)*g*g
        a = a0 / (np.sqrt(v) + eps)
        steps.append(np.linalg.norm(a))
        x = x - a*g
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist), np.array(steps)

def heavy_ball(f, grad, x0, alpha, beta, iters):
    x = x0.copy()
    z = np.zeros_like(x)
    hist = [f(x)]
    xs = [x.copy()]
    for _ in range(iters):
        z = beta*z + alpha*grad(x)
        x = x - z
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist)


def run_q1():
    iters = 120

    _, gdA = gd(f_A, grad_A, x0_A, 0.08, iters)
    _, pA, sA = polyak(f_A, grad_A, x0_A, 0.0, 1e-4, iters)
    _, adA, sadA = adagrad(f_A, grad_A, x0_A, 1.8, 1e-5, iters)
    _, rmA, srmA = rmsprop(f_A, grad_A, x0_A, 0.22, 0.9, 1e-5, iters)
    _, hbA = heavy_ball(f_A, grad_A, x0_A, 0.045, 0.88, iters)

    xsgdB, gdB = gd(f_B, grad_B, x0_B, 0.06, iters)
    xspB, pB, sB = polyak(f_B, grad_B, x0_B, 0.0, 1e-4, iters)
    xsadB, adB, sadB = adagrad(f_B, grad_B, x0_B, 1.2, 1e-5, iters)
    xsrmB, rmB, srmB = rmsprop(f_B, grad_B, x0_B, 0.14, 0.9, 1e-5, iters)
    xshbB, hbB = heavy_ball(f_B, grad_B, x0_B, 0.035, 0.90, iters)

    xsgdC, gdC = gd(f_C, grad_C, x0_C, 0.0012, iters)
    xspC, pC, sC = polyak(f_C, grad_C, x0_C, 0.0, 1e-3, iters)
    xsadC, adC, sadC = adagrad(f_C, grad_C, x0_C, 0.45, 1e-5, iters)
    xsrmC, rmC, srmC = rmsprop(f_C, grad_C, x0_C, 0.0035, 0.9, 1e-5, iters)
    xshbC, hbC = heavy_ball(f_C, grad_C, x0_C, 0.0008, 0.86, iters)

    # loss curves
    fig, ax = plt.subplots(1, 3, figsize=(15, 4))
    for a, data, title in [
        (ax[0], [gdA, pA, adA, rmA, hbA], "Benchmark A"),
        (ax[1], [gdB, pB, adB, rmB, hbB], "Benchmark B"),
        (ax[2], [gdC, pC, adC, rmC, hbC], "Benchmark C")]:
        labels = ["GD", "Polyak", "Adagrad", "RMSprop", "HeavyBall"]
        for d, l in zip(data, labels):
            a.semilogy(d, label=l)
        a.set_title(title)
        a.set_xlabel("iteration")
        a.set_ylabel("f(x)")
        a.legend()
    plt.tight_layout()
    plt.savefig("q1_loss.png", dpi=120)
    plt.close()

    # contour B - split into two panels so polyak doesn't dominate
    x1 = np.linspace(-2, 3, 200)
    x2 = np.linspace(-2, 4, 200)
    XX, YY = np.meshgrid(x1, x2)
    Z = np.zeros_like(XX)
    for i in range(XX.shape[0]):
        for j in range(XX.shape[1]):
            Z[i,j] = f_B(np.array([XX[i,j], YY[i,j]]))

    fig, axes = plt.subplots(1, 2, figsize=(13, 5))
    axes[0].contour(XX, YY, Z, 40, cmap="viridis")
    for traj, lab in [(xsgdB, "GD"), (xsadB, "Adagrad"),
                      (xsrmB, "RMSprop"), (xshbB, "HeavyBall")]:
        axes[0].plot(traj[:,0], traj[:,1], "-o", markersize=2, label=lab)
    axes[0].legend()
    axes[0].set_title("Stable methods on Benchmark B")
    axes[0].set_xlabel("x1"); axes[0].set_ylabel("x2")

    axes[1].contour(XX, YY, Z, 40, cmap="viridis")
    axes[1].plot(xspB[:,0], xspB[:,1], "-o", markersize=2,
                 color="orange", label="Polyak")
    axes[1].legend()
    axes[1].set_title("Polyak (shown separately)")
    axes[1].set_xlabel("x1"); axes[1].set_ylabel("x2")
    plt.tight_layout()
    plt.savefig("q1_contourB.png", dpi=120)
    plt.close()

    # contour C
    x1 = np.linspace(-2, 2, 200)
    x2 = np.linspace(-1, 3, 200)
    XX, YY = np.meshgrid(x1, x2)
    Z = np.zeros_like(XX)
    for i in range(XX.shape[0]):
        for j in range(XX.shape[1]):
            Z[i,j] = f_C(np.array([XX[i,j], YY[i,j]]))
    plt.figure(figsize=(7,5))
    plt.contour(XX, YY, Z, 40, cmap="viridis")
    for traj, lab in [(xsgdC, "GD"), (xspC, "Polyak"), (xsadC, "Adagrad"),
                      (xsrmC, "RMSprop"), (xshbC, "HeavyBall")]:
        plt.plot(traj[:,0], traj[:,1], "-o", markersize=2, label=lab)
    plt.legend(); plt.title("Benchmark C trajectories")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q1_contourC.png", dpi=120)
    plt.close()

    # step sizes
    fig, ax = plt.subplots(1, 3, figsize=(15,4))
    ax[0].plot(sA, label="Polyak")
    ax[0].plot(sadA, label="Adagrad")
    ax[0].plot(srmA, label="RMSprop")
    ax[0].set_title("Step size - A"); ax[0].legend(); ax[0].set_yscale("log")
    ax[1].plot(sB, label="Polyak")
    ax[1].plot(sadB, label="Adagrad")
    ax[1].plot(srmB, label="RMSprop")
    ax[1].set_title("Step size - B"); ax[1].legend(); ax[1].set_yscale("log")
    ax[2].plot(sC, label="Polyak")
    ax[2].plot(sadC, label="Adagrad")
    ax[2].plot(srmC, label="RMSprop")
    ax[2].set_title("Step size - C"); ax[2].legend(); ax[2].set_yscale("log")
    plt.tight_layout()
    plt.savefig("q1_steps.png", dpi=120)
    plt.close()

    # how smooth is the convergence
    def smoothness(h):
        h = np.array(h)
        h = np.where(h > 0, h, 1e-12)
        d = np.diff(np.log10(h))
        return np.std(d)

    print("=== Q1 smoothness (std of log10 loss differences) ===")
    for name, runs in [("A", [("GD",gdA),("Polyak",pA),("Adagrad",adA),("RMSprop",rmA),("HB",hbA)]),
                        ("B", [("GD",gdB),("Polyak",pB),("Adagrad",adB),("RMSprop",rmB),("HB",hbB)]),
                        ("C", [("GD",gdC),("Polyak",pC),("Adagrad",adC),("RMSprop",rmC),("HB",hbC)])]:
        vals = " ".join(f"{n}={smoothness(h):.3f}" for n, h in runs)
        print(f"{name}: {vals}")


# Q2

def nesterov(f, grad, x0, alpha, bmax, iters):
    x = x0.copy()
    z = np.zeros_like(x)
    hist = [f(x)]
    xs = [x.copy()]
    for k in range(1, iters+1):
        b = min((k-1)/(k+2), bmax)
        z = b*z - alpha*grad(x + b*z)
        x = x + z
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist)

def adam(f, grad, x0, alpha, b1, b2, eps, iters):
    x = x0.copy()
    mt = np.zeros_like(x)
    vt = np.zeros_like(x)
    hist = [f(x)]
    xs = [x.copy()]
    for k in range(1, iters+1):
        g = grad(x)
        mt = b1*mt + (1-b1)*g
        vt = b2*vt + (1-b2)*g*g
        mh = mt/(1-b1**k)
        vh = vt/(1-b2**k)
        x = x - alpha*mh/(np.sqrt(vh)+eps)
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist)

def sgd(X, y, theta0, alpha, batch, epochs):
    theta = theta0.copy()
    n = X.shape[0]
    hist = []
    for _ in range(epochs):
        idx = np.random.permutation(n)
        for s in range(0, n, batch):
            b = idx[s:s+batch]
            r = X[b] @ theta - y[b]
            g = X[b].T @ r / len(b)
            theta = theta - alpha*g
        r_full = X @ theta - y
        hist.append((r_full @ r_full)/(2*n))
    return theta, np.array(hist)


def run_q2():
    iters = 150

    _, gdA = gd(f_A, grad_A, x0_A, 0.08, iters)
    _, gdB = gd(f_B, grad_B, x0_B, 0.06, iters)
    _, gdC = gd(f_C, grad_C, x0_C, 0.0012, iters)

    _, nA = nesterov(f_A, grad_A, x0_A, 0.06, 0.90, iters)
    xsnB, nB = nesterov(f_B, grad_B, x0_B, 0.035, 0.92, iters)
    xsnC, nC = nesterov(f_C, grad_C, x0_C, 0.0007, 0.90, iters)

    _, amA = adam(f_A, grad_A, x0_A, 0.12, 0.82, 0.999, 1e-8, iters)
    xsamB, amB = adam(f_B, grad_B, x0_B, 0.08, 0.80, 0.999, 1e-8, iters)
    xsamC, amC = adam(f_C, grad_C, x0_C, 0.006, 0.80, 0.999, 1e-8, iters)

    fig, ax = plt.subplots(1, 3, figsize=(15,4))
    for a, data, title in [
        (ax[0], [gdA, nA, amA], "A"),
        (ax[1], [gdB, nB, amB], "B"),
        (ax[2], [gdC, nC, amC], "C")]:
        for d, l in zip(data, ["GD","Nesterov","Adam"]):
            a.semilogy(d, label=l)
        a.set_title("Benchmark "+title)
        a.set_xlabel("iter"); a.set_ylabel("f")
        a.legend()
    plt.tight_layout()
    plt.savefig("q2_loss.png", dpi=120)
    plt.close()

    xsgdB_, _ = gd(f_B, grad_B, x0_B, 0.06, iters)
    xsgdC_, _ = gd(f_C, grad_C, x0_C, 0.0012, iters)

    def contour_plot(f, traj_list, labels, xr, yr, title, fname):
        x1 = np.linspace(*xr, 200)
        x2 = np.linspace(*yr, 200)
        XX, YY = np.meshgrid(x1, x2)
        Z = np.zeros_like(XX)
        for i in range(XX.shape[0]):
            for j in range(XX.shape[1]):
                Z[i,j] = f(np.array([XX[i,j], YY[i,j]]))
        plt.figure(figsize=(7,5))
        plt.contour(XX, YY, Z, 40, cmap="viridis")
        for t, l in zip(traj_list, labels):
            plt.plot(t[:,0], t[:,1], "-o", markersize=2, label=l)
        plt.legend()
        plt.title(title)
        plt.xlabel("x1"); plt.ylabel("x2")
        plt.savefig(fname, dpi=120)
        plt.close()

    contour_plot(f_B, [xsgdB_, xsnB, xsamB],
        ["GD","Nesterov","Adam"], (-2,3), (-2,4),
        "Benchmark B", "q2_contourB.png")
    contour_plot(f_C, [xsgdC_, xsnC, xsamC],
        ["GD","Nesterov","Adam"], (-2,2), (-1,3),
        "Benchmark C", "q2_contourC.png")

    theta0 = np.zeros(2)
    _, h5 = sgd(X_data, y_data, theta0, 0.06, 5, 50)
    _, h40 = sgd(X_data, y_data, theta0, 0.06, 40, 50)

    plt.figure()
    plt.semilogy(h5, label="batch=5")
    plt.semilogy(h40, label="batch=40")
    plt.xlabel("epoch"); plt.ylabel("loss")
    plt.title("Mini-batch SGD")
    plt.legend()
    plt.savefig("q2_sgd.png", dpi=120)
    plt.close()

    y_noisy = X_data @ theta_star + 6.0*np.random.randn(m)
    def sgd_noisy(batch):
        theta = np.zeros(2)
        n = X_data.shape[0]
        hist = []
        for _ in range(50):
            idx = np.random.permutation(n)
            for s in range(0, n, batch):
                b = idx[s:s+batch]
                r = X_data[b] @ theta - y_noisy[b]
                g = X_data[b].T @ r / len(b)
                theta = theta - 0.06*g
            r_full = X_data @ theta - y_noisy
            hist.append((r_full @ r_full)/(2*n))
        return np.array(hist)

    h5n = sgd_noisy(5)
    h40n = sgd_noisy(40)

    plt.figure()
    plt.semilogy(h5n, label="batch=5 noisy")
    plt.semilogy(h40n, label="batch=40 noisy")
    plt.xlabel("epoch"); plt.ylabel("loss")
    plt.title("Noisy SGD")
    plt.legend()
    plt.savefig("q2_sgd_noisy.png", dpi=120)
    plt.close()

    print("\n=== Q2 SGD numbers ===")
    print(f"clean batch=5: final={h5[-1]:.4f}, std last 20 epochs={np.std(h5[-20:]):.4f}")
    print(f"clean batch=40: final={h40[-1]:.4f}, std last 20 epochs={np.std(h40[-20:]):.4f}")
    print(f"noisy batch=5: final={h5n[-1]:.4f}, std last 20 epochs={np.std(h5n[-20:]):.4f}")
    print(f"noisy batch=40: final={h40n[-1]:.4f}, std last 20 epochs={np.std(h40n[-20:]):.4f}")


# Q3

def newton(f, grad, hess, x0, alpha, iters, damp=1e-8):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    updates = []
    for _ in range(iters):
        g = grad(x)
        H = hess(x) + damp*np.eye(len(x))
        step = np.linalg.solve(H, g)
        updates.append(np.linalg.norm(step))
        x = x - alpha*step
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist), np.array(updates)

def hess_A(theta):
    return X_data.T @ X_data / m

def hess_B(x):
    return np.array([[2 - np.sin(x[0]), 0],
                     [0, 10]])

def hess_C(x):
    return np.array([
        [2 - 400*(x[1]-3*x[0]**2), -400*x[0]],
        [-400*x[0], 200]
    ])


def run_q3():
    g = lambda x: x**4
    gp = lambda x: 4*x**3
    gpp = lambda x: 12*x**2
    x0 = 0.25
    xs = np.linspace(-0.5, 0.8, 200)
    first = g(x0) + gp(x0)*(xs - x0)
    second = first + 0.5*gpp(x0)*(xs - x0)**2
    plt.figure()
    plt.plot(xs, g(xs), label="g(x)=x^4")
    plt.plot(xs, first, "--", label="1st order")
    plt.plot(xs, second, "--", label="2nd order")
    plt.scatter([x0],[g(x0)], color="red")
    plt.legend()
    plt.title("Local approximation at x0=0.25")
    plt.xlabel("x"); plt.ylabel("g(x)")
    plt.savefig("q3_approx.png", dpi=120)
    plt.close()

    _, gdA = gd(f_A, grad_A, x0_A, 0.08, 80)
    xsgdB, gdB = gd(f_B, grad_B, x0_B, 0.06, 80)
    xsgdC, gdC = gd(f_C, grad_C, x0_C, 0.001, 80)

    _, nA, uA = newton(f_A, grad_A, hess_A, x0_A, 1.0, 20)
    xsnB, nB, uB = newton(f_B, grad_B, hess_B, x0_B, 0.85, 20)
    xsnC, nC, uC = newton(f_C, grad_C, hess_C, x0_C, 0.22, 20)

    fig, ax = plt.subplots(1, 3, figsize=(15,4))
    ax[0].semilogy(gdA, label="GD")
    ax[0].semilogy(nA, label="Newton")
    ax[0].set_title("A"); ax[0].legend()
    ax[0].set_xlabel("iter"); ax[0].set_ylabel("f")
    ax[1].semilogy(gdB, label="GD")
    ax[1].semilogy(nB, label="Newton")
    ax[1].set_title("B"); ax[1].legend()
    ax[1].set_xlabel("iter"); ax[1].set_ylabel("f")
    ax[2].semilogy(gdC, label="GD")
    ax[2].semilogy(nC, label="Newton")
    ax[2].set_title("C"); ax[2].legend()
    ax[2].set_xlabel("iter"); ax[2].set_ylabel("f")
    plt.tight_layout()
    plt.savefig("q3_loss.png", dpi=120)
    plt.close()

    def contour_plot(f, t1, t2, xr, yr, title, fname):
        x1 = np.linspace(*xr, 200)
        x2 = np.linspace(*yr, 200)
        XX, YY = np.meshgrid(x1, x2)
        Z = np.zeros_like(XX)
        for i in range(XX.shape[0]):
            for j in range(XX.shape[1]):
                Z[i,j] = f(np.array([XX[i,j], YY[i,j]]))
        plt.figure(figsize=(7,5))
        plt.contour(XX, YY, Z, 40, cmap="viridis")
        plt.plot(t1[:,0], t1[:,1], "-o", markersize=2, label="GD")
        plt.plot(t2[:,0], t2[:,1], "-o", markersize=2, label="Newton")
        plt.legend(); plt.title(title)
        plt.xlabel("x1"); plt.ylabel("x2")
        plt.savefig(fname, dpi=120)
        plt.close()

    contour_plot(f_B, xsgdB, xsnB, (-2,3), (-2,4), "B", "q3_contourB.png")
    contour_plot(f_C, xsgdC, xsnC, (-2,2), (-1,3), "C", "q3_contourC.png")

    plt.figure()
    plt.plot(uB, label="Newton (B)")
    plt.plot(uC, label="Newton (C)")
    plt.yscale("log")
    plt.xlabel("iter"); plt.ylabel("step norm")
    plt.legend()
    plt.title("Newton update magnitudes")
    plt.savefig("q3_updates.png", dpi=120)
    plt.close()


# Q4

def fd_grad(f, x, delta):
    n = len(x)
    g = np.zeros(n)
    fx = f(x)
    for i in range(n):
        e = np.zeros(n); e[i] = 1
        g[i] = (f(x + delta*e) - fx)/delta
    return g

def gd_fd(f, x0, alpha, delta, iters):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    for _ in range(iters):
        x = x - alpha*fd_grad(f, x, delta)
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist)

def nesterov_rs(f, x0, alpha, delta, iters):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    for _ in range(iters):
        u = np.random.randn(len(x))
        u = u/np.linalg.norm(u)
        d = (f(x + delta*u) - f(x))/delta
        x = x - alpha*d*u
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist)

def nelder_mead(f, x0, step, iters):
    n = len(x0)
    simplex = [x0.copy()]
    for i in range(n):
        p = x0.copy()
        p[i] += step
        simplex.append(p)
    simplex = np.array(simplex)
    hist = []
    traj = []
    for _ in range(iters):
        vals = np.array([f(p) for p in simplex])
        order = np.argsort(vals)
        simplex = simplex[order]
        vals = vals[order]
        hist.append(vals[0])
        traj.append(simplex[0].copy())
        centroid = simplex[:-1].mean(axis=0)
        xr = centroid + (centroid - simplex[-1])
        if f(xr) < vals[0]:
            xe = centroid + 2*(centroid - simplex[-1])
            simplex[-1] = xe if f(xe) < f(xr) else xr
        elif f(xr) < vals[-2]:
            simplex[-1] = xr
        else:
            xc = centroid + 0.5*(simplex[-1] - centroid)
            if f(xc) < vals[-1]:
                simplex[-1] = xc
            else:
                for i in range(1, len(simplex)):
                    simplex[i] = simplex[0] + 0.5*(simplex[i]-simplex[0])
    return np.array(traj), np.array(hist)

def grid_search(f, xr, yr, n):
    x1 = np.linspace(*xr, n)
    x2 = np.linspace(*yr, n)
    pts = []
    vals = []
    best = np.inf
    best_so_far = []
    for a in x1:
        for b in x2:
            v = f(np.array([a,b]))
            pts.append([a,b])
            vals.append(v)
            if v < best: best = v
            best_so_far.append(best)
    return np.array(pts), np.array(vals), np.array(best_so_far)


def run_q4():
    _, exact = gd(f_B, grad_B, x0_B, 0.06, 120)
    _, fd_good = gd_fd(f_B, x0_B, 0.08, 0.05, 120)
    _, fd_poor = gd_fd(f_B, x0_B, 0.08, 0.8, 120)
    xs_rs, rs = nesterov_rs(f_B, x0_B, 0.025, 0.08, 220)

    plt.figure()
    plt.semilogy(exact, label="Exact GD")
    plt.semilogy(fd_good, label="FD δ=0.05")
    plt.semilogy(fd_poor, label="FD δ=0.8")
    plt.semilogy(rs, label="Nesterov RS")
    plt.legend(); plt.xlabel("iter"); plt.ylabel("f")
    plt.title("Benchmark B - derivative-free")
    plt.savefig("q4_loss.png", dpi=120)
    plt.close()

    x1 = np.linspace(-2, 3, 200)
    x2 = np.linspace(-2, 4, 200)
    XX, YY = np.meshgrid(x1, x2)
    Z = np.zeros_like(XX)
    for i in range(XX.shape[0]):
        for j in range(XX.shape[1]):
            Z[i,j] = f_B(np.array([XX[i,j], YY[i,j]]))

    xs_gd, _ = gd(f_B, grad_B, x0_B, 0.06, 120)
    xs_fdg, _ = gd_fd(f_B, x0_B, 0.08, 0.05, 120)
    xs_fdp, _ = gd_fd(f_B, x0_B, 0.08, 0.8, 120)

    plt.figure(figsize=(7,5))
    plt.contour(XX, YY, Z, 40, cmap="viridis")
    plt.plot(xs_gd[:,0], xs_gd[:,1], "-o", ms=2, label="Exact")
    plt.plot(xs_fdg[:,0], xs_fdg[:,1], "-o", ms=2, label="FD good")
    plt.plot(xs_fdp[:,0], xs_fdp[:,1], "-o", ms=2, label="FD poor")
    plt.plot(xs_rs[:,0], xs_rs[:,1], "-o", ms=1, label="RS")
    plt.legend()
    plt.title("Benchmark B trajectories")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q4_contourB.png", dpi=120)
    plt.close()

    nm_traj, nm_hist = nelder_mead(f_C, np.array([-1.2, 1.0]), 0.35, 160)

    x1 = np.linspace(-2, 2, 200)
    x2 = np.linspace(-1, 3, 200)
    XX, YY = np.meshgrid(x1, x2)
    Z = np.zeros_like(XX)
    for i in range(XX.shape[0]):
        for j in range(XX.shape[1]):
            Z[i,j] = f_C(np.array([XX[i,j], YY[i,j]]))

    plt.figure(figsize=(7,5))
    plt.contour(XX, YY, Z, 40, cmap="viridis")
    plt.plot(nm_traj[:,0], nm_traj[:,1], "-o", ms=2, color="red", label="Nelder-Mead")
    plt.legend(); plt.title("Nelder-Mead on Rosenbrock")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q4_NM.png", dpi=120)
    plt.close()

    pts, vals, best = grid_search(f_C, (-2,2), (-1,3), 55)
    plt.figure(figsize=(7,5))
    plt.contour(XX, YY, Z, 40, cmap="viridis")
    plt.scatter(pts[:,0], pts[:,1], s=4, c="black", alpha=0.5)
    plt.title("Grid search sampling")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q4_grid.png", dpi=120)
    plt.close()

    plt.figure()
    plt.semilogy(best)
    plt.xlabel("samples"); plt.ylabel("best value")
    plt.title("Grid search best-so-far")
    plt.savefig("q4_grid_best.png", dpi=120)
    plt.close()

    # timing
    import time
    t0 = time.perf_counter()
    for _ in range(5): gd(f_B, grad_B, x0_B, 0.06, 120)
    t_gd = (time.perf_counter()-t0)/5
    t0 = time.perf_counter()
    for _ in range(5): gd_fd(f_B, x0_B, 0.08, 0.05, 120)
    t_fd = (time.perf_counter()-t0)/5
    t0 = time.perf_counter()
    for _ in range(5): nesterov_rs(f_B, x0_B, 0.025, 0.08, 220)
    t_rs = (time.perf_counter()-t0)/5
    t0 = time.perf_counter()
    for _ in range(5): nelder_mead(f_C, np.array([-1.2,1.0]), 0.35, 160)
    t_nm = (time.perf_counter()-t0)/5
    t0 = time.perf_counter()
    for _ in range(5): grid_search(f_C, (-2,2), (-1,3), 55)
    t_gs = (time.perf_counter()-t0)/5

    print("\n=== Q4 wall clock (seconds, mean of 5 runs) ===")
    print(f"Exact GD: {t_gd*1000:.2f} ms")
    print(f"FD good:  {t_fd*1000:.2f} ms")
    print(f"Random search: {t_rs*1000:.2f} ms")
    print(f"Nelder-Mead: {t_nm*1000:.2f} ms")
    print(f"Grid search: {t_gs*1000:.2f} ms")


# Q5

def proj_gd(f, grad, x0, alpha, iters):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    viol = [max(0, 0.5 - x[0])]
    for _ in range(iters):
        x = x - alpha*grad(x)
        x[0] = max(0.5, x[0])
        hist.append(f(x))
        xs.append(x.copy())
        viol.append(max(0, 0.5 - x[0]))
    return np.array(xs), np.array(hist), np.array(viol)

def penalty_gd(f, grad, x0, alpha, lam, iters):
    x = x0.copy()
    def F(x): return f(x) + lam*max(0, -x[0]+0.5)
    def gF(x):
        g = grad(x).copy()
        if -x[0]+0.5 > 0:
            g[0] -= lam
        return g
    hist = [F(x)]
    xs = [x.copy()]
    viol = [max(0, 0.5 - x[0])]
    for _ in range(iters):
        x = x - alpha*gF(x)
        hist.append(F(x))
        xs.append(x.copy())
        viol.append(max(0, 0.5 - x[0]))
    return np.array(xs), np.array(hist), np.array(viol)


def run_q5():
    x0 = np.array([0.2, 4.0])
    iters = 100

    xs_u,  h_u           = gd(f_B, grad_B, x0, 0.07, iters)
    xs_p,  h_p,  v_p     = proj_gd(f_B, grad_B, x0, 0.08, iters)
    xs_l1, h_l1, v_l1    = penalty_gd(f_B, grad_B, x0, 0.05, 0.15, iters)
    xs_l2, h_l2, v_l2    = penalty_gd(f_B, grad_B, x0, 0.05, 1.8,  iters)
    xs_l3, h_l3, v_l3    = penalty_gd(f_B, grad_B, x0, 0.03, 4.5,  iters)

    v_u = np.maximum(0, 0.5 - xs_u[:,0])

    plt.figure()
    plt.semilogy(h_u,  label="Unconstrained")
    plt.semilogy(h_p,  label="Projected")
    plt.semilogy(h_l1, label="Penalty λ=0.15")
    plt.semilogy(h_l2, label="Penalty λ=1.8")
    plt.semilogy(h_l3, label="Penalty λ=4.5")
    plt.legend(); plt.xlabel("iter"); plt.ylabel("objective")
    plt.title("Constrained optimisation")
    plt.savefig("q5_loss.png", dpi=120); plt.close()

    x1 = np.linspace(-1, 4, 200)
    x2 = np.linspace(-1, 5, 200)
    XX, YY = np.meshgrid(x1, x2)
    Z = np.zeros_like(XX)
    for i in range(XX.shape[0]):
        for j in range(XX.shape[1]):
            Z[i,j] = f_B(np.array([XX[i,j], YY[i,j]]))

    plt.figure(figsize=(7,5))
    plt.contour(XX, YY, Z, 40, cmap="viridis")
    plt.axvline(0.5, color="red", linestyle="--", label="x1=0.5")
    plt.scatter([0.5527], [2.0], color="red", s=80, marker="*",
                zorder=5, label="optimum")
    plt.plot(xs_u[:,0],  xs_u[:,1],  "-o", ms=2, label="Unconstrained")
    plt.plot(xs_p[:,0],  xs_p[:,1],  "-o", ms=2, label="Projected")
    plt.plot(xs_l1[:,0], xs_l1[:,1], "-o", ms=2, label="Pen λ=0.15")
    plt.plot(xs_l2[:,0], xs_l2[:,1], "-o", ms=2, label="Pen λ=1.8")
    plt.plot(xs_l3[:,0], xs_l3[:,1], "-o", ms=2, label="Pen λ=4.5")
    plt.legend(); plt.title("Trajectories"); plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q5_contour.png", dpi=120); plt.close()

    def masked(v): return np.where(v > 0, v, np.nan)

    plt.figure()
    plt.semilogy(masked(v_u),  label="Unconstrained")
    plt.semilogy(masked(v_p),  label="Projected")
    plt.semilogy(masked(v_l1), label="λ=0.15")
    plt.semilogy(masked(v_l2), label="λ=1.8")
    plt.semilogy(masked(v_l3), label="λ=4.5")
    plt.legend(); plt.xlabel("iter"); plt.ylabel("max(0, 0.5 - x1)")
    plt.title("Constraint violation")
    plt.savefig("q5_viol.png", dpi=120); plt.close()

    plt.figure()
    plt.semilogy(masked(v_u[:30]),  label="Unconstrained")
    plt.semilogy(masked(v_p[:30]),  label="Projected")
    plt.semilogy(masked(v_l1[:30]), label="λ=0.15")
    plt.semilogy(masked(v_l2[:30]), label="λ=1.8")
    plt.semilogy(masked(v_l3[:30]), label="λ=4.5")
    plt.legend(); plt.xlabel("iter"); plt.ylabel("violation")
    plt.title("Violation (first 30 iters)")
    plt.savefig("q5_viol_zoom.png", dpi=120); plt.close()

    def first_feasible(v):
        idx = np.where(v <= 1e-10)[0]
        return int(idx[0]) if len(idx) else -1

    print("\n=== Q5 ===")
    print(f"Unconstrained final f={h_u[-1]:.4f}, first feasible iter={first_feasible(v_u)}")
    print(f"Projected    final f={h_p[-1]:.4f}, first feasible iter={first_feasible(v_p)}")
    print(f"Pen λ=0.15   final f={h_l1[-1]:.4f}, first feasible iter={first_feasible(v_l1)}")
    print(f"Pen λ=1.8    final f={h_l2[-1]:.4f}, first feasible iter={first_feasible(v_l2)}")
    print(f"Pen λ=4.5    final f={h_l3[-1]:.4f}, first feasible iter={first_feasible(v_l3)}")


# Q6

def lp_vertex(grad_vec, bounds):
    x = np.zeros(2)
    for i in range(2):
        x[i] = bounds[i][0] if grad_vec[i] >= 0 else bounds[i][1]
    return x

def frank_wolfe(f, grad, x0, beta, iters, bounds):
    x = x0.copy()
    hist = [f(x)]
    xs = [x.copy()]
    zs = []
    for _ in range(iters):
        g = grad(x)
        z = lp_vertex(g, bounds)
        zs.append(z.copy())
        x = beta*x + (1-beta)*z
        hist.append(f(x))
        xs.append(x.copy())
    return np.array(xs), np.array(hist), np.array(zs)


def run_q6():
    bounds = [(0.5, 5), (-5, 10)]

    x1 = np.linspace(0.5, 5, 200)
    x2 = np.linspace(-5, 10, 200)
    XX, YY = np.meshgrid(x1, x2)
    Z = XX + 2*YY
    plt.figure(figsize=(6,5))
    plt.contour(XX, YY, Z, 30, cmap="viridis")
    plt.scatter([0.5], [-5], color="red", s=120, marker="*", zorder=5,
                label="optimum (0.5, -5)")
    plt.gca().add_patch(plt.Rectangle((0.5,-5), 4.5, 15, fill=False, edgecolor="red"))
    plt.legend()
    plt.title("Linear programme over box")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q6_lp.png", dpi=120)
    plt.close()

    f1 = lambda x: (x[0]-1)**2 + (x[1]-5)**2
    g1 = lambda x: np.array([2*(x[0]-1), 2*(x[1]-5)])
    xs_a, h_a, zs_a = frank_wolfe(f1, g1, np.array([1.0,1.0]), 0.90, 180, bounds)
    xs_b, h_b, zs_b = frank_wolfe(f1, g1, np.array([1.0,1.0]), 0.985, 180, bounds)

    plt.figure()
    plt.semilogy(h_a, label="β=0.90")
    plt.semilogy(h_b, label="β=0.985")
    plt.legend(); plt.xlabel("iter"); plt.ylabel("f")
    plt.title("FW interior optimum")
    plt.savefig("q6_interior_loss.png", dpi=120)
    plt.close()

    XX, YY = np.meshgrid(np.linspace(0.5,5,200), np.linspace(-5,10,200))
    Z = (XX-1)**2 + (YY-5)**2
    plt.figure(figsize=(6,5))
    plt.contour(XX, YY, Z, 30, cmap="viridis")
    plt.plot(xs_a[:,0], xs_a[:,1], "-o", ms=2, label="β=0.90")
    plt.plot(xs_b[:,0], xs_b[:,1], "-o", ms=2, label="β=0.985")
    plt.legend(); plt.title("FW interior trajectory")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q6_interior_contour.png", dpi=120)
    plt.close()

    f2 = lambda x: x[0]**2 + x[1]**2
    g2 = lambda x: 2*x
    xs_c, h_c, zs_c = frank_wolfe(f2, g2, np.array([3.0,3.0]), 0.93, 140, bounds)

    plt.figure()
    plt.semilogy(h_c)
    plt.xlabel("iter"); plt.ylabel("f")
    plt.title("FW boundary optimum")
    plt.savefig("q6_boundary_loss.png", dpi=120)
    plt.close()

    XX, YY = np.meshgrid(np.linspace(0.5,5,200), np.linspace(-5,10,200))
    Z = XX**2 + YY**2
    plt.figure(figsize=(6,5))
    plt.contour(XX, YY, Z, 30, cmap="viridis")
    plt.plot(xs_c[:,0], xs_c[:,1], "-o", ms=2, color="red")
    plt.title("FW boundary trajectory")
    plt.xlabel("x1"); plt.ylabel("x2")
    plt.savefig("q6_boundary_contour.png", dpi=120)
    plt.close()

    plt.figure()
    plt.plot(xs_a[:,0], label="xk[0]")
    plt.plot(xs_a[:,1], label="xk[1]")
    plt.plot(zs_a[:,0], "--", label="zk[0]")
    plt.plot(zs_a[:,1], "--", label="zk[1]")
    plt.legend(); plt.title("FW interior xk vs zk")
    plt.xlabel("iter")
    plt.savefig("q6_xz.png", dpi=120)
    plt.close()


if __name__ == "__main__":
    run_q1()
    run_q2()
    run_q3()
    run_q4()
    run_q5()
    run_q6()
    print("done")