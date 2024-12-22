import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Ridge

data = pd.read_csv('week3.csv')

X = data.iloc[:, :-1].values  # Features
y = data.iloc[:, -1].values   # Target

#(b)
# Function to train and print Ridge regression results for various polynomial degrees up to 5 and C values 0.001, 1, 10, 100 and 1000
def ridge_regression_with_poly_features(X, y, degrees, C_values):
    results = []
    for degree in degrees:
        poly = PolynomialFeatures(degree)
        X_poly = poly.fit_transform(X)
        
        for C in C_values:
            alpha = 1 / (2 * C)  # Ridge uses alpha as the regularisation parameter
            model = Ridge(alpha=alpha)
            model.fit(X_poly, y)

            # Collect results for reporting
            results.append({
                'Degree': degree,
                'C': C,
                'Coefficients': model.coef_,
                'Intercept': model.intercept_
            })
    
    results_df = pd.DataFrame(results)
    return results_df

# Degrees and C values
degrees = range(1, 6)
C_values = [0.001, 1, 10, 100, 1000]

# Run Ridge Regression with polynomial features
ridge_results = ridge_regression_with_poly_features(X, y, degrees, C_values)

# Results for each degree
for degree in degrees:
    print(f"Ridge Regression Parameters for degree of polynomial feature = {degree}")
    subset = ridge_results[ridge_results['Degree'] == degree]
    print(subset[['C', 'Coefficients', 'Intercept']].to_string(index=False))
    print()

# (c) Predictions on a grid for Ridge regression and plot
grid_range = np.linspace(-5, 5, 50)
X_test = []
for i in grid_range:
    for j in grid_range:
        X_test.append([i, j])
X_test = np.array(X_test)

# Transform grid using polynomial features for plotting Ridge model predictions
degree = 5  # Degree 5 polynomial expansion 
poly = PolynomialFeatures(degree)
X_poly_test = poly.fit_transform(X_test)

# Fit Ridge model and generate predictions for different C values
for C in C_values:
    alpha = 1 / (2 * C)
    model = Ridge(alpha=alpha)
    
    # Fit Ridge model to the polynomial-transformed training data
    X_poly = poly.fit_transform(X)
    model.fit(X_poly, y)
    
    # Generate predictions on the grid
    predictions = model.predict(X_poly_test)
    
    # Plot predictions
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    # Reshape predictions for surface plotting
    X1, X2 = X_test[:, 0], X_test[:, 1]
    ax.plot_trisurf(X1, X2, predictions, color='green', alpha=0.6, label='Predictions')

    # Scatter plot for the original training data
    ax.scatter(X[:, 0], X[:, 1], y, c='r', marker='o', label='Training Data')

    ax.set_xlabel('Feature 1')
    ax.set_ylabel('Feature 2')
    ax.set_zlabel('Target')
    ax.set_title(f'Ridge Predictions with C={C}')
    plt.legend()
    plt.show()
