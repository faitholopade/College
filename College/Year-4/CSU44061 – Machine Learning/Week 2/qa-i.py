import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


# Load data from CSV
df = pd.read_csv("week2.csv")
df.columns = ['X1', 'X2', 'y']
print(df.head())


# Extract the X1, X2 features and target variable y
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
y = df.iloc[:, 2]


# Separate the data into two classes: +1 and -1
positive_class = (y == 1)
negative_class = (y == -1)


# Plot the data points
plt.figure(figsize=(8, 6))
plt.scatter(X1[positive_class], X2[positive_class], c='green', marker='+', label='Class +1')
plt.scatter(X1[negative_class], X2[negative_class], c='blue', marker='o', label='Class -1')


# Add labels and legend
plt.xlabel('x_1')
plt.ylabel('x_2')
plt.legend()
plt.show()
