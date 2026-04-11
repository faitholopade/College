# ------------------------------------------------------------------------------
# Lab 6 – Linear Regression, OLS by Hand, Confidence & Prediction Intervals,
# Model Improvement, Multicollinearity (VIF), and performance metrics
# ------------------------------------------------------------------------------


# 0) - Libraries ----------------------------------------------------------
library(dplyr)
library(car)
library(caret)   # for CV


# 1) - Load and prepare the data -----------------------------------------
dataset <- read.csv("labs/chapter_6/austinHousingData.csv")
dataset <- dataset[, !names(dataset) %in% c("description","homeImage")]

set.seed(123)  # reproducibility

# Shuffle rows (the original data is sorted by city)
dataset <- dataset[sample(nrow(dataset)), ]

# Display column names neatly
cols <- colnames(dataset)
n <- 5
for (i in seq(1, length(cols), by = n)) {
  cat(paste(cols[i:min(i + n - 1, length(cols))], collapse = " | "), "\n")
}

# Feature selection (similar set as for Naive Bayes)
covariates <- c(
  "livingAreaSqFt", "lotSizeSqFt", "numOfBedrooms", "numOfBathrooms",
  "numOfStories", "yearBuilt", "garageSpaces", "parkingSpaces",
  "hasCooling", "hasHeating", "hasGarage", "hasView", "hasSpa",
  "hasAssociation", "homeType", "avgSchoolRating", "avgSchoolDistance",
  "MedianStudentsPerTeacher", "propertyTaxRate", "latitude", "longitude"
)

# Subset
housing_mod <- dataset %>%
  select(all_of(c("latestPrice", covariates))) %>%
  mutate(
    # Not used in linear regression now, but left in for consistency
    price_class = cut(
      latestPrice,
      breaks = quantile(latestPrice, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE),
      labels = c("low", "medium", "high", "very high"),
      include.lowest = TRUE
    )
  )



# ------------------------------------------------------------------------------
# Exercise 1 – OLS "by hand"
# ------------------------------------------------------------------------------

# We'll use a small model for the matrix algebra:
# latestPrice ~ livingAreaSqFt + numOfBedrooms
dat <- housing_mod %>%
  select(latestPrice, livingAreaSqFt, numOfBedrooms) %>%
  na.omit()

n <- nrow(dat)
head(dat)

# Does it make sense to treat numOfBedrooms as a factor?
unique(dat$numOfBedrooms)


# Fit the reference model using lm()
m1 <- lm(latestPrice ~ livingAreaSqFt + numOfBedrooms, data = dat)
summary(m1)

# You will now recreate the core components of this summary manually
# using the OLS formula.


# --- TO DO: Construct the model matrix "X"
# Find out how to create a model matrix in R and what it contains
X <- "YOUR CODE"
y <- dat$latestPrice

dim(X)
head(X)


# --- TO DO: Compute the OLS estimates for beta_hat (all beta coefficients)

# solve() in R gives matrix inverses
solve(X) # This would be the inverse of the matrix X

# Compare with model summary
summary(m1)$coefficients

# --- TO DO: Compute fitted values, residuals, RSS (Sum of squared residuals),
# p (number of estimated coefficients), df (degrees of freedom),
# residual variance sigma^2, and residual standard deviation sigma

y_hat      <- "YOUR CODE"   # Fitted values
resid      <- "YOUR CODE"   # Residuals
RSS        <- "YOUR CODE"   # Sum of squared residuals
p          <- "YOUR CODE"   # number of beta parameters (excluding sigma)
df         <- "YOUR CODE"   # Degrees of freedom (n - p)
sigma2_hat <- "YOUR CODE"   # residual variance
sigma_hat  <- "YOUR CODE"   # residual standard error

# Compare with summary(m1)
summary(m1)
summary(m1)$sigma^2


# --- TO DO: Compute Var(beta_hat) = sigma^2 * (X'X)^(-1)
#           and standard errors SE(beta)

varcov_beta <- "YOUR CODE"
SE_beta     <- "YOUR CODE"

# Compare with summary(m1)
summary(m1)$coefficients


# --- TO DO: Hypothesis tests on coefficients
# H0: beta_j = 0
# t_j = beta_hat_j / SE_j
# p_j = check calculation of p-values in the last lab. Reference distribution is
# a t-distribution with degrees of freedom equal to df from the last task

t_stats <- "YOUR CODE"
p_vals  <- "YOUR CODE"

# Compare with model summary
summary(m1)$coefficients



# 2) - Confidence and prediction Intervals --------------------------------
# (Next lab will include calculating them by hand)


new_data   <- dat[1:5, ]
new_data_2 <- data.frame(livingAreaSqFt = 15000, numOfBedrooms = 3)

# Confidence intervals for the MEAN response
pred_ci <- predict(m1, newdata = new_data, interval = "confidence")
pred_ci

# Prediction intervals for a NEW observation
pred_pi <- predict(m1, newdata = new_data, interval = "prediction")
pred_pi

# Prediction interval for a hypothetical house
pred_pi_2 <- predict(m1, newdata = new_data_2, interval = "prediction")
pred_pi_2


# Interpretation of confidence and prediction intervals:
#
# - Confidence Interval ("confidence"):
# This interval describes the uncertainty in the "mean" predicted house 
# price for a group of houses with certain characteristics. It tells you 
# where the true average price for houses like these is likely to lie.
#
# - Prediction Interval ("prediction"):
# This interval describes the uncertainty for an individual future house 
# with the given characteristics. It is always wider because it must also
# account for the natural variability in individual house prices (residual variance).
#
# Interpreting the values:
# - If an interval contains 0, this means that, statistically, the model allows
# the predicted mean (or predicted future value) to be as low as zero. This 
# typically happens when the standard error is large and/or the model is weak.
#
# - For house prices, negative values in a prediction interval are not meaningful.
# Linear regression does not automatically respect real-world bounds; it can 
# predict values below 0 simply because the model is a straight line with noise.
#
# Very wide intervals or negative lower bounds indicate that:
# 1) the model explains little of the variation in the data,
# 2) important predictors may be missing, or
# 3) a linear model may not be appropriate for accurate price prediction.

# Notice how prediction intervals are extremely wide in our case!
# This reflects very large residual variance. Our simple model explains
# little of the variation in house prices. It seems like we have to try to 
# improve our model!!!!


# ------------------------------------------------------------------------------
# Exercise 3 – Improve your model :)
# ------------------------------------------------------------------------------
# TO DO: Explore different combinations of predictors.
# Compare models using R_squared, Adjusted R², RMSE, and AIC.
# Try to reduce the residual variation and improve predictive accuracy.


# 3) - Introducing Variance Inflation Factors (VIF) -----------------------
m2 <- lm(latestPrice ~ livingAreaSqFt + garageSpaces + parkingSpaces,
         data = housing_mod)

summary(m2)

# Multicollinearity check
car::vif(m2)

# Rule of thumb:
#   VIF > 5   → moderate multicollinearity
#   VIF > 10  → severe multicollinearity
# Consider removing one of the variables with high VIF.


# 4) Regression metrics and cross-validation ------------------------------
# In linear regression problems, we often cannot use classification metrics like
# (accuracy, confusion matrix, etc.). Instead, we evaluate models using
# error- and variance-based metrics:


# 1) RMSE – Root Mean Squared Error
# Measures the average prediction error in the same units as the response.
# Lower RMSE = better predictive accuracy.
train_control <- trainControl(method = "cv", number = 5)

# 5-fold cross-validation using RMSE
m_cv <- train(
  latestPrice ~ livingAreaSqFt + garageSpaces + parkingSpaces,
  data = housing_mod,
  method = "lm",
  trControl = train_control,
  metric = "RMSE"
)

m_cv


# 2) R-squared (R^2) – Coefficient of Determination
# R^2 measures the proportion of variability in the response that is explained
# by the model.

# 3) Adjusted R-squared
# Adjusted R^2 corrects R^2 by penalising unnecessary predictors. It is more
# reliable for comparing models with different numbers of variables.

# Both R-squared and adjusted R-squared can be found in summary(model)


# 4) Akaike’s Information Criterion (AIC)
#    AIC balances model fit and model complexity.
#    Lower AIC = better model (difference of >= 2 is generally meaningful).
AIC(m1, m2)


# Have fun! :)






