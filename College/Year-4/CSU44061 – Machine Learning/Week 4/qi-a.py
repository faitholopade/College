import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
import matplotlib.pyplot as plt

# Load dataset
data = pd.read_csv('week4_d1.csv', header=None)
X = data.iloc[:, :-1].values
y = data.iloc[:, -1].values

# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Pipeline that includes polynomial feature creation and logistic regression
pipeline = Pipeline([
    ('poly', PolynomialFeatures()),
    ('logreg', LogisticRegression(penalty='l2', solver='lbfgs'))
])

# Parameters grid for cross-validation
param_grid = {
    'poly__degree': [1, 2, 3, 4, 5],  # Poly degrees to try
    'logreg__C': [0.001, 1, 10, 100, 1000]  # C vals
}

# Grid search with cross-validation
grid = GridSearchCV(pipeline, param_grid, cv=5, scoring='accuracy') 

# Fit  model
grid.fit(X_train, y_train)

# Best model's parameters and score
print("Best parameters:", grid.best_params_)
print("Best cross-validation score: {:.2f}".format(grid.best_score_))

# Evaluate  model on  test set
test_score = grid.score(X_test, y_test)
print("Test set score: {:.2f}".format(test_score))

# Plot  results
results = pd.DataFrame(grid.cv_results_)
best_index = results['mean_test_score'].idxmax()
best_score = results.loc[best_index, 'mean_test_score']

plt.figure(figsize=(10, 6))
plt.errorbar(results['param_poly__degree'], results['mean_test_score'], yerr=results['std_test_score'], label='Mean CV score', color='blue')
plt.plot(results['param_poly__degree'], np.full_like(results['param_poly__degree'], fill_value=best_score, dtype=float), linestyle='--', color='red', label=f'Best score ({best_score:.2f})')
plt.xlabel('Polynomial Degree')
plt.ylabel('Mean Cross-Validation Score')
plt.title('Polynomial Degree vs. CV Performance')
plt.legend()
plt.show()
