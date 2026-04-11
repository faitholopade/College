# 0) - Libraries ----------------------------------------------------------
library(dplyr)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)


# 1) - Load data ----------------------------------------------------------
dataset <- read.csv("!!!YOUR_PATH!!!/austinHousingData.csv")
dataset <- dataset[, !names(dataset) %in% c("description","homeImage")]

# for reproducibility
set.seed(123) 

# shuffle to do not have them sorted by city
dataset <- dataset[sample(nrow(dataset)), ]

# Get the column names
cols <- colnames(dataset)
n <- 5 # number of columns per line

# print the names of the columns
for (i in seq(1, length(cols), by = n)) {
  cat(paste(cols[i:min(i + n - 1, length(cols))], collapse = " | "), "\n")
}


# Feature selection
covariates <- c(
  "livingAreaSqFt", "lotSizeSqFt", "numOfBedrooms", "numOfBathrooms",
  "numOfStories", "yearBuilt", "garageSpaces", "parkingSpaces",
  "hasCooling", "hasHeating", "hasGarage", "hasView", "hasSpa",
  "hasAssociation", "homeType", "avgSchoolRating", "avgSchoolDistance",
  "MedianStudentsPerTeacher", "propertyTaxRate", "latitude", "longitude"
)

# Subset data
housing_mod <- dataset %>%
  select(all_of(c("latestPrice", covariates))) %>%
  mutate(
    price_class = cut(
      latestPrice,
      breaks = quantile(latestPrice, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE),
      labels = c("low", "medium", "high", "very high"),
      include.lowest = TRUE
    )
  ) %>%
  select(-latestPrice)





# 2) - Hypothesis test ----------------------------------------------------
# ------------------------------------------------------------------------------
# A hypothesis test is a way to check if there is enough evidence in our sample
# to support or reject a certain claim about a population (the null hypothesis H0).
#
# We compare H0 to an alternative hypothesis (H1) that represents a different idea.
#
# The test works by calculating a "test statistic". This measures how far
# our sample result is from what we would expect if H0 were true.
# 
# We then check how likely it is to see such a result (or more extreme)
# under the assumption that H0 is true. This probability is called the p-value.
#
# If the p-value is small (usually below 0.05), it means our observed difference
# would be very unlikely just by chance, so we reject H0 in favour of H1.
#
# Below are two short examples showing a one-sample t-test:
# one where there is no significant difference, and one where there is.
# ------------------------------------------------------------------------------


# Example with no significant difference
x <- c(9.8, 10.1, 9.9, 10.0, 10.2, 10.5)

n <- length(x)    # length of vector
xbar <- mean(x)   # sample mean
s <- sd(x)        # sample standard deviation
mu0 <- 10         # alternative hypothesis 

# Calculate test statistic, degrees of freedom and p-value
t_stat <- (xbar - mu0) / (s / sqrt(n))
df <- n - 1
p_value <- 2 * (1 - pt(abs(t_stat), df))
c(t_stat = t_stat, df = df, p_value = p_value)

# Compare with built-in R function
t.test(x, mu = 10, alternative = "two.sided")


# Example with Significant difference
x <- c(10.5, 10.8, 10.4, 10.6, 10.9, 11.2)

n <- length(x)
xbar <- mean(x)
s <- sd(x)
mu0 <- 10

t_stat <- (xbar - mu0) / (s / sqrt(n))
df <- n - 1
p_value <- 2 * (1 - pt(abs(t_stat), df))
c(t_stat = t_stat, df = df, p_value = p_value)

t.test(x, mu = 10, alternative = "two.sided")




# 3) - Several ways to test independence ----------------------------------

# Categorical vs. Categorical (X^2 chi-squared test)

# Example: Garage presence vs price class (target feature)
tbl <- table(housing_mod$hasGarage, housing_mod$price_class)
chisq.test(tbl)
# Extremely low p-value, so there seems to be a relationship between the presence
# of a garage and the house price

# Concept of conditional independence
subset_high <- subset(housing_mod, price_class == "high")
chisq.test(table(subset_high$hasGarage, subset_high$hasView))



# Categorical vs. Numeric

# Example: living area vs price class
fit_aov <- aov(livingAreaSqFt ~ price_class, data = housing_mod)
summary(fit_aov)

# Example: conditional independence between features
fit_aov_2 <- aov(livingAreaSqFt ~ hasGarage, data = subset_high)
summary(fit_aov_2)



# Numeric vs. Numeric

# Example: conditional independence between numerical features
plot(subset_high$livingAreaSqFt, subset_high$avgSchoolDistance)
cor(subset_high$livingAreaSqFt, subset_high$avgSchoolDistance)
cor.test(subset_high$livingAreaSqFt, subset_high$avgSchoolDistance)

# Extremely low p-value, but very low correlation coefficient too.
# Does it make sense to assume dependence here?


# ------------------------------------------------------------------------------
# Exercise: Predicting Housing Price Category
# ------------------------------------------------------------------------------
# Use everything you have learned so far to fit multiple models that predict the
# housing price category using the `housing_mod` data frame.
#
# You can explore different modelling strategies, such as:
# - Using different sets of covariates (e.g. a subset of features only vs. all features)
# - Converting continuous variables into categorical ones (e.g., using cut() or bins)
# - Trying different cross-validation (CV) settings (e.g., different k values)
# - Using different train/test split ratios
#
# For each approach:
#   - Fit the model
#   - Evaluate its performance (e.g., Accuracy)
#   - Compare all models to see which performs best
#
# Remember that you have also learned about tree-based models, so it would be good
# to include and compare a few of those as well (e.g., decision trees, random forest).
#
# ------------------------------------------------------------------------------
# Note on data splitting and cross-validation
# ------------------------------------------------------------------------------
# When you split your data, you usually separate it into two parts:
#   1) Training data
#   2) Test (or validation) data
#
# Cross-validation (e.g. k-fold CV) is then applied WITHIN the training data.
# The training portion is split again into k subsets (folds). Each fold is used
# once as a validation set, while the remaining k-1 folds are used for training.
# The model performance is then averaged across folds.
#
# The final performance should ideally be checked on the separate test set,
# which was never seen during model training or cross-validation.
#
# ------------------------------------------------------------------------------
# Food for thought
# ------------------------------------------------------------------------------
# - Exploratory analysis is very important. Plot variables against each other:
#   scatterplots for continuous features, boxplots for categorical vs. numeric.
#   Doing hypothesis test between ALL combinations might take a lot of time...
#
# - Does it make sense to include ALL variables? Think about your data.
#   Could some features be highly correlated or dependent on each other?
#   If two features are strongly correlated, does including both help or harm?
#
# - Try including and excluding correlated variables. Does it change model accuracy?
#
# - How can you detect signs of overfitting? What happens to accuracy between
#   training and validation sets when overfitting occurs?
#
# - Efficiency also matters! Especially for very high-dimensional data.
#   There can be a big difference between a model that takes 5 days to train
#   versus one that takes 3 days for similar performance.
#
# You now have all the tools to perform a proper machine learning workflow:
# fit, tune, compare, and interpret different models to find the best one.
# ------------------------------------------------------------------------------


# Have fun :) 



























