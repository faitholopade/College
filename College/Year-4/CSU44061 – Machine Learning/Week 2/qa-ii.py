import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression


df = pd.read_csv("week2.csv")
df.columns = ['X1', 'X2', 'y']


X = df[['X1', 'X2']].values
y = df['y'].values


# Train  logistic regression model
log_reg = LogisticRegression(penalty=None, solver='lbfgs')  # No regularisation for this task
log_reg.fit(X, y)


# Output  model parameters
print("Intercept:", log_reg.intercept_)
print("Coefficients:", log_reg.coef_)


# Predict with  logistic regression model
predictions = log_reg.predict(X)


#Feature importance
feature_importance = log_reg.coef_[0]
print(f"Feature X1 coefficient: {feature_importance[0]}")
print(f"Feature X2 coefficient: {feature_importance[1]}")


#Feature influence
if feature_importance[0] > feature_importance[1]:
    print("X1 has a greater influence on the predictions than X2.")
else:
    print("X2 has a greater influence on the predictions than X1.")


# Predictions  made based on the sign of the linear combination of features.
# A positive coefficient means that an increase in that feature leads to an increase in the likelihood of class 1.
