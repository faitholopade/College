import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Lasso

# Load the dataset
data = pd.read_csv('week3.csv')

X = data.iloc[:, :-1].values 
y = data.iloc[:, -1].values

# Grid for the feature space
grid_range = np.linspace(-5, 5, 50) 
X_test = []
for i in grid_range:
    for j in grid_range:
        X_test.append([i, j])
X_test = np.array(X_test)

# Transform grid using polynomial features
degree = 5  # Degree for polynomial expansion
poly = PolynomialFeatures(degree)
X_poly_test = poly.fit_transform(X_test)

# Fit Lasso model and generate predictions
C_values = [0.001, 1, 10, 100, 1000]  # Regularisation params
for C in C_values:
    alpha = 1 / (2 * C)
    model = Lasso(alpha=alpha)
    
    X_poly = poly.fit_transform(X)  # Transform training data features
    model.fit(X_poly, y)  # Fit model to the training data
    
    # Generate predictions on the grid
    predictions = model.predict(X_poly_test)
    
    # Plot for predictions
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    
    # Reshape predictions for surface plotting
    X1, X2 = X_test[:, 0], X_test[:, 1]
    ax.plot_trisurf(X1, X2, predictions, color='blue', alpha=0.6, label='Predictions')  # Uniform color for predictions
    
    # Scatter plot for the original training data
    ax.scatter(X[:, 0], X[:, 1], y, c='r', marker='o', label='Training Data')  # Keep training data in red
    
    ax.set_xlabel('Feature 1')
    ax.set_ylabel('Feature 2')
    ax.set_zlabel('Target')
    ax.set_title(f'Lasso Predictions with C={C}')
    
    plt.legend()
    
    plt.show()
