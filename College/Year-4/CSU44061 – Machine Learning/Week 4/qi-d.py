import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import roc_curve
from sklearn.dummy import DummyClassifier
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier

data = pd.read_csv('week4_d1.csv', header=None)
X = data.iloc[:, :-1].values
y = data.iloc[:, -1].values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Logistic Regression Model
poly = PolynomialFeatures(degree=3)
X_train_poly = poly.fit_transform(X_train)
X_test_poly = poly.transform(X_test)
logreg_model = LogisticRegression(C=25).fit(X_train_poly, y_train)
logreg_predictions = logreg_model.predict(X_test_poly)

# kNN Model
knn_model = KNeighborsClassifier(n_neighbors=40).fit(X_train, y_train)
knn_predictions = knn_model.predict(X_test)

# Baseline Models
# Most Frequent Classifier
most_frequent_model = DummyClassifier(strategy="most_frequent").fit(X_train, y_train)
most_frequent_predictions = most_frequent_model.predict(X_test)

# Random Classifier
random_model = DummyClassifier(strategy="uniform").fit(X_train, y_train)
random_predictions = random_model.predict(X_test)

# Logistic Regression predictions
logreg_probs = logreg_model.decision_function(X_test_poly)

# kNN predictions (using predict_proba)
knn_probs = knn_model.predict_proba(X_test)[:, 1]

# Baseline classifiers: Most Frequent and Random
most_frequent_model = DummyClassifier(strategy="most_frequent").fit(X_train, y_train)
random_model = DummyClassifier(strategy="uniform").fit(X_train, y_train)

# Get the probabilities for baseline models
most_frequent_probs = most_frequent_model.predict_proba(X_test)[:, 1]
random_probs = random_model.predict_proba(X_test)[:, 1]

# Calculate the ROC curve points
fpr_logreg, tpr_logreg, _ = roc_curve(y_test, logreg_probs)
fpr_knn, tpr_knn, _ = roc_curve(y_test, knn_probs)
fpr_most_frequent, tpr_most_frequent, _ = roc_curve(y_test, most_frequent_probs)
fpr_random, tpr_random, _ = roc_curve(y_test, random_probs)

# Plot the ROC curves
plt.figure(figsize=(10, 6))
plt.plot(fpr_logreg, tpr_logreg, label='Logistic Regression', color='cyan')
plt.plot(fpr_knn, tpr_knn, label='kNN Classifier', color='orange')
plt.plot(fpr_most_frequent, tpr_most_frequent, label='Most Frequent Classifier', color='blue')
plt.plot(fpr_random, tpr_random, label='Random Classifier', color='red')
plt.plot([0, 1], [0, 1], linestyle='--', color='green', label='Random Baseline')

# Customize the plot
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curves for Logistic Regression, kNN, and Baseline Models')
plt.legend(loc='lower right')

plt.show()
