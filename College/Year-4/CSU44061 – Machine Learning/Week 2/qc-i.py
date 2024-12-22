import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression


df = pd.read_csv("week2.csv")
df.columns = ['X1', 'X2', 'y']


X1 = df['X1'].values
X2 = df['X2'].values
y = df['y'].values


# Create additional squared features to capture non-linear relationships
X1_squared = X1 ** 2
X2_squared = X2 ** 2


# Combine the original and squared features into a new feature matrix
X_new = np.column_stack((X1, X2, X1_squared, X2_squared))


# Train the logistic regression model using the new feature set
log_reg = LogisticRegression(penalty=None, solver='lbfgs')
log_reg.fit(X_new, y)


print("Intercept:", log_reg.intercept_)
print("Coefficients:", log_reg.coef_)


predictions = log_reg.predict(X_new)


plt.figure(figsize=(8, 6))
positive_class = (y == 1)
negative_class = (y == -1)
plt.scatter(X1[positive_class], X2[positive_class], c='green', marker='+', label='Class +1')
plt.scatter(X1[negative_class], X2[negative_class], c='blue', marker='o', label='Class -1')


plt.scatter(X1[predictions == 1], X2[predictions == 1], c='orange', marker='^', label='Predictions Class +1')
plt.scatter(X1[predictions == -1], X2[predictions == -1], c='red', marker='v', label='Predictions Class -1')


plt.xlabel('X1')
plt.ylabel('X2')
plt.legend()
plt.title('Logistic Regression with Additional Squared Features')


plt.show()
