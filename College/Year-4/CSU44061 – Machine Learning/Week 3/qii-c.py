from sklearn.linear_model import Ridge
from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error
import numpy as np
import pandas as pd
from sklearn.preprocessing import PolynomialFeatures
import matplotlib.pyplot as plt

data = pd.read_csv('week3.csv')

X = data.iloc[:, :-1].values 
y = data.iloc[:, -1].values

# Cross-validation and plotting function for Ridge Regression
def ridge_cross_validation_plot(X, y, c_values, degree):
    mean_error = []
    std_error = []

    # Polynomial feature transformation
    poly = PolynomialFeatures(degree=degree)
    X_poly = poly.fit_transform(X)
    
    # 5-fold cross-validation
    kf = KFold(n_splits=5, shuffle=True, random_state=42)
    
    for C in c_values:
        alpha = 1 / (2 * C)  # Ridge uses alpha = 1 / (2 * C)
        model = Ridge(alpha=alpha)
        
        temp_errors = []
        for train_idx, test_idx in kf.split(X_poly):
            X_train, X_test = X_poly[train_idx], X_poly[test_idx]
            y_train, y_test = y[train_idx], y[test_idx]
            
            model.fit(X_train, y_train)
            y_pred = model.predict(X_test)
            
            # ,MSE for this fold
            error = mean_squared_error(y_test, y_pred)
            temp_errors.append(error)
        
        # mean and standard deviation of the errors
        mean_error.append(np.mean(temp_errors))
        std_error.append(np.std(temp_errors))
    
    # Plot the results using error bars
    plt.errorbar(c_values, mean_error, yerr=std_error, fmt='-o', capsize=5)
    plt.xlabel('C values')
    plt.ylabel('Mean Squared Error')
    plt.title(f'5-Fold CV - Mean and Std of Prediction Error vs C (degree {degree})')
    plt.xscale('log')
    plt.show()

# C values
c_values = [0.001, 0.01, 0.1, 1, 10, 100, 1000]  
degree = 5  # Using polynomial features of degree 5 as in previous parts

# Run cross-validation and plot the results for Ridge Regression
ridge_cross_validation_plot(X, y, c_values, degree)
