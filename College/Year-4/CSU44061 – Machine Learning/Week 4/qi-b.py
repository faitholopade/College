import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.neighbors import KNeighborsClassifier
import matplotlib.pyplot as plt

data = pd.read_csv('week4_d1.csv', header=None)
X = data.iloc[:, :-1].values
y = data.iloc[:, -1].values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Function to evaluate kNN for different values of k
def evaluate_knn(k_range, X_train, y_train):
    mean_accuracy = []
    std_accuracy = []
    
    for k in k_range:
        # Initialize kNN model
        knn_model = KNeighborsClassifier(n_neighbors=k)
        
        # Perform cross-validation and get  accuracy scores
        scores = cross_val_score(knn_model, X_train, y_train, cv=5, scoring='accuracy')
        
        # Store mean and standard deviation of  cross-validation scores
        mean_accuracy.append(np.mean(scores))
        std_accuracy.append(np.std(scores))
    
    return mean_accuracy, std_accuracy

# Range of k values to test
k_range = range(1, 31)

# Evaluate kNN for different values of k
mean_accuracy, std_accuracy = evaluate_knn(k_range, X_train, y_train)

# Plot  cross-validation results
plt.figure(figsize=(10, 6))
plt.errorbar(k_range, mean_accuracy, yerr=std_accuracy, fmt='-o', ecolor='red', capsize=5, capthick=2)
plt.title('kNN Classifier: Cross-Validation Accuracy for Different k Values')
plt.xlabel('Number of Neighbors (k)')
plt.ylabel('Mean Cross-Validation Accuracy')
plt.xticks(k_range)
plt.grid(True)
plt.show()
