# Importing necessary libraries
import matplotlib.pyplot as plt
import pandas as pd

# Load  dataset
data = pd.read_csv('week4_d2.csv')

# Renaming columns for easier access
data.columns = ['X1', 'X2', 'y']

# Splitting  data based on the labels
positive_class = data[data['y'] == 1]
negative_class = data[data['y'] == -1]

# Plotting the scatter plot
plt.scatter(positive_class['X1'], positive_class['X2'], c='r', marker='+', label='Positive')
plt.scatter(negative_class['X1'], negative_class['X2'], c='g', marker='o', label='Negative')

# Adding labels and title
plt.xlabel('X1')
plt.ylabel('X2')
plt.title('Scatter Plot of X1 vs X2')

# Adding a legend
plt.legend(loc='upper right')

# Display the plot
plt.show()
