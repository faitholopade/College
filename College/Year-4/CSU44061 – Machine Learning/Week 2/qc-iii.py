import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score


df = pd.read_csv("week2.csv")
df.columns = ['X1', 'X2', 'y']


X = df[['X1', 'X2']].values
y = df['y'].values


log_reg = LogisticRegression(penalty=None, solver='lbfgs')  # No regularisation
log_reg.fit(X, y)


predictions = log_reg.predict(X)


# Accuracy for the logistic regression model
log_reg_accuracy = accuracy_score(y, predictions)
print(f"Logistic Regression Accuracy: {log_reg_accuracy * 100:.2f}%")


# Baseline predictor: always the most common class (-1)
most_common_class = np.full_like(y, fill_value=-1)


# Accuracy for the baseline model
baseline_accuracy = accuracy_score(y, most_common_class)
print(f"Baseline Accuracy: {baseline_accuracy * 100:.2f}%")


# Original training data and predictions for logistic regression
plt.figure(figsize=(8, 6))
positive_class = (y == 1)
negative_class = (y == -1)
plt.scatter(X[positive_class, 0], X[positive_class, 1], c='green', marker='+', label='Class +1')
plt.scatter(X[negative_class, 0], X[negative_class, 1], c='blue', marker='o', label='Class -1')


# Logistic regression predictions with a different marker
plt.scatter(X[predictions == 1, 0], X[predictions == 1, 1], c='orange', marker='^', label='Predicted Class +1')
plt.scatter(X[predictions == -1, 0], X[predictions == -1, 1], c='red', marker='v', label='Predicted Class -1')


intercept = log_reg.intercept_[0]
coef = log_reg.coef_[0]
x_values = np.linspace(X[:, 0].min(), X[:, 0].max(), 100)
decision_boundary = -(coef[0] * x_values + intercept) / coef[1]


plt.plot(x_values, decision_boundary, color='black', linestyle='--', label='Decision Boundary')


plt.xlabel('X1')
plt.ylabel('X2')
plt.legend()
plt.title('Logistic Regression vs Baseline')
plt.show()
