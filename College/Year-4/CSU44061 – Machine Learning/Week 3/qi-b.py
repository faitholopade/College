import pandas as pd
import numpy as np
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Lasso

np.set_printoptions(threshold=np.inf, suppress=True, linewidth=200)

data = pd.read_csv('week3.csv')

X = data.iloc[:, :-1].values 
y = data.iloc[:, -1].values

# Range of degrees and C values
degrees = range(1, 6)
C_values = [0.001, 1, 10, 100, 1000]

results = []

# Train models for each degree of polynomial features
for degree in degrees:
    poly = PolynomialFeatures(degree)
    X_poly = poly.fit_transform(X)
    
    for C in C_values:
        # Lasso uses alpha as the regularisation parameter, alpha = 1 / (2 * C)
        alpha = 1 / (2 * C)
        model = Lasso(alpha=alpha)
        model.fit(X_poly, y)
        

        results.append({
            'Degree': degree,
            'C': C,
            'Coefficients': model.coef_,
            'Intercept': model.intercept_
        })

results_df = pd.DataFrame(results)

# Print results for each degree
for degree in degrees:
    print(f"Parameters for degree of polynomial feature = {degree}")
    subset = results_df[results_df['Degree'] == degree]
    print(subset[['C', 'Coefficients', 'Intercept']].to_string(index=False))
    print()
