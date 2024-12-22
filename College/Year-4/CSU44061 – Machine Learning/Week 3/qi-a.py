import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Load  dataset
data = pd.read_csv('week3.csv')

# Extract features and target from the dataset
X = data.iloc[:, :-1].values  # Features
y = data.iloc[:, -1].values   # Target

# 3D scatter plot
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Plot data points: X[:, 0] is the first feature, X[:, 1] is the second feature, and y is the target
ax.scatter(X[:, 0], X[:, 1], y, c='b', marker='o')

# Axis labels
ax.set_xlabel('Feature 1')
ax.set_ylabel('Feature 2')
ax.set_zlabel('Target')

plt.title('3D Scatter Plot of the Data')
plt.show()
