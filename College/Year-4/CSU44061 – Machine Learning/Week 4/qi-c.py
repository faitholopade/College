import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.dummy import DummyClassifier
from sklearn.preprocessing import PolynomialFeatures

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

# Confusion Matrices
print("Confusion Matrix for Logistic Regression:\n", confusion_matrix(y_test, logreg_predictions))
print("Classification Report for Logistic Regression:\n", classification_report(y_test, logreg_predictions))

print("Confusion Matrix for kNN Classifier:\n", confusion_matrix(y_test, knn_predictions))
print("Classification Report for kNN Classifier:\n", classification_report(y_test, knn_predictions))

print("Confusion Matrix for Most Frequent Classifier:\n", confusion_matrix(y_test, most_frequent_predictions))
print("Classification Report for Most Frequent Classifier:\n", classification_report(y_test, most_frequent_predictions))

print("Confusion Matrix for Random Classifier:\n", confusion_matrix(y_test, random_predictions))
print("Classification Report for Random Classifier:\n", classification_report(y_test, random_predictions))
