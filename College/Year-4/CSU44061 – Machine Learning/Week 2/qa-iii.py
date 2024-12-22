import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression


df = pd.read_csv("week2.csv")
df.columns = ['X1', 'X2', 'y']


X = df[['X1', 'X2']].values
y = df['y'].values


log_reg = LogisticRegression(penalty=None, solver='lbfgs')  # No regularisation
log_reg.fit(X, y)


print("Intercept:", log_reg.intercept_)
print("Coefficients:", log_reg.coef_)




# Predict target values using the trained logistic regression model
predictions = log_reg.predict(X)


positive_class = (y == 1)
negative_class = (y == -1)


# Plot original data points with different markers for predictions
plt.figure(figsize=(8, 6))
plt.scatter(X[positive_class, 0], X[positive_class, 1], c='green', marker='+', label='Class +1')
plt.scatter(X[negative_class, 0], X[negative_class, 1], c='blue', marker='o', label='Class -1')


# Plot predictions with orange triangles and red inverted triangles
plt.scatter(X[predictions == 1, 0], X[predictions == 1, 1], c='orange', marker='^', label='Predictions Class +1')
plt.scatter(X[predictions == -1, 0], X[predictions == -1, 1], c='red', marker='v', label='Predictions Class -1')


plt.xlabel('X1')
plt.ylabel('X2')
plt.legend()


# Decision boundary occurs where the model's output is 0: theta_0 + theta_1 * X1 + theta_2 * X2 = 0
intercept = log_reg.intercept_[0]
coef = log_reg.coef_[0]
x_values = np.linspace(X[:, 0].min(), X[:, 0].max(), 100)
decision_boundary = -(coef[0] * x_values + intercept) / coef[1]


# Plot the decision boundary
plt.plot(x_values, decision_boundary, color='black', linestyle='--', label='Decision Boundary')


plt.legend()
plt.show()
