import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.svm import LinearSVC


df = pd.read_csv("week2.csv")
df.columns = ['X1', 'X2', 'y']


X = df[['X1', 'X2']].values
y = df['y'].values


C_values = [0.001, 1, 100]


for C in C_values:
    print(f"Training Linear SVM with C = {C}")
   
    svm_model = LinearSVC(C=C, max_iter=10000)  # Increase max_iter if needed
    svm_model.fit(X, y)
   
    print(f"Intercept: {svm_model.intercept_}")
    print(f"Coefficients: {svm_model.coef_}")
   
    # Make predictions using the SVM model
    predictions = svm_model.predict(X)
   
# Plot the data points along with predictions
    plt.figure(figsize=(8, 6))
    positive_class = (y == 1)
    negative_class = (y == -1)
    plt.scatter(X[positive_class, 0], X[positive_class, 1], c='green', marker='+', label='Class +1 (Actual)')
    plt.scatter(X[negative_class, 0], X[negative_class, 1], c='blue', marker='o', label='Class -1 (Actual)')
   
    # Plot the predictions with different markers
    plt.scatter(X[predictions == 1, 0], X[predictions == 1, 1], c='orange', marker='^', label='Predicted Class +1')
    plt.scatter(X[predictions == -1, 0], X[predictions == -1, 1], c='red', marker='v', label='Predicted Class -1')


    # Decision boundary
    intercept = svm_model.intercept_[0]
    coef = svm_model.coef_[0]
    x_values = np.linspace(X[:, 0].min(), X[:, 0].max(), 100)
    decision_boundary = -(coef[0] * x_values + intercept) / coef[1]
    plt.plot(x_values, decision_boundary, color='black', linestyle='--', label=f'Decision Boundary (C={C})')
   
    plt.xlabel('X1')
    plt.ylabel('X2')
    plt.legend()
    plt.title(f'Linear SVM Predictions and Decision Boundary with C={C}')
    plt.show()
