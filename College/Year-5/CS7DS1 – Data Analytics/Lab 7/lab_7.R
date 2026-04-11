# ------------------------------------------------------------------------------
# Lab 7 – Logistic Regression, MLE by hand using optim(), categorical variables
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
# Exercise 1 – MLE "by hand" using optim()
# ------------------------------------------------------------------------------
set.seed(123)

# Simulate data from a true linear regression model
n <- 100
x <- runif(n, 0, 10)

beta0_true <- 3
beta1_true <- 2
sigma_true <- 2

mu <- beta0_true + beta1_true * x
y  <- rnorm(n, mean = mu, sd = sigma_true)


# Fit with optim() using the NEGATIVE log-likelihood
negloglik_lm <- function(par, x, y) {
  beta0 <- par[1]
  beta1 <- par[2]
  sigma <- abs(par[3])
  mu    <- beta0 + beta1 * x
  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
}

start <- c(0, 0, 1)

fit_mle <- optim(start, negloglik_lm, x = x, y = y)
fit_mle$par

# Fit a lm() object as comparison
lm_fit <- lm(y ~ x)
summary(lm_fit) # Coefficients should be very similar to the MLE coefficients.



# --- TO DO: Compute the MLE estimates for logistic regression parameters

# Load simulated logistic regression dataset
sim_data <- readRDS("labs/chapter_6/logistic_sim_data.rds") # Adjust path

head(sim_data)

# x = numeric predictor
# y = binary response (0/1)

X <- cbind(1, sim_data$x)
y <- sim_data$y

# Your task:
# 1) Write the logistic negative log-likelihood
# 2) Use optim() to estimate beta0 and beta1
# 3) Compare with glm(..., family = binomial)


# 2. Define the negative log-likelihood for logistic regression
# Hints:
# - eta = X %*% beta
# - p   = inverse logit of eta
# - log-likelihood of Bernoulli = (try to find it out :)

negloglik_logit <- function(par, X, y) {
  # TODO: linear predictor
  # eta <-
  
  # TODO: apply inverse logit to get probabilities
  # p <-
  
  # TODO: numerical protection for p = 0 or p = 1
  # eps <- 1e-10
  # p <- pmin(pmax(p, eps), 1 - eps)
  
  # TODO: return NEGATIVE log-likelihood
  # return(...)
}

# Start values for optim()
start <- rep(0, ncol(X))

# Get MLE estimates
fit_mle_logit <- optim(start, negloglik_logit, X = X, y = y, method = "BFGS")
fit_mle_logit$par

# Compare with glm()
fit_glm <- glm(y ~ x, data = sim_data, family = binomial)
coef(fit_glm)



# Factor variables -------------------------------------------------------------

# Toy data frame
df <- data.frame(
  color = factor(c("red", "blue", "green", "blue", "red")),
  x     = c(1, 2, 3, 4, 5),
  y     = c(0, 1, 1, 0, 1)
)
df

# Show levels of the factor variable
levels(df$color)

# What happens if we create a model matrix?
model.matrix(~ color + x, data = df)

# Quick logistic regression model
glm(y ~ color + x, family = binomial, data = df)
# How are coefficients interpreted?

# Change reference level
df$color <- relevel(df$color, ref = "red")
model.matrix(~ color + x, data = df)
glm(y ~ color + x, family = binomial, data = df)




# ------------------------------------------------------------------------------
# Exercise 2 – Logistic Regression on the Housing Dataset
# ------------------------------------------------------------------------------
# 
# Working steps for this exercise:
#
# 1) Create a binary response variable from `latestPrice` using a cutoff of your
#    choice (for example the median, the 75th percentile, the 90th percentile, 
#    or any value that seems reasonable). Repeat the analysis with at a few
#    different cutoff rules and compare how the choice of cutoff affects 
#    the model's predictive performance.
#
# 2) Split the dataset into training and test sets using the same approach as 
#    in previous labs (e.g., 70/30 split with set.seed for reproducibility).
#
# 3) Fit several logistic regression models using glm(..., family = binomial),
#    each using different combinations of predictors. Try both continuous and
#    categorical predictors
#
# 4) Evaluate each model on the test set. Compute the performance metrics you 
#    used before, such as classification accuracy, confusion matrix, and 
#    Cohen’s kappa (since we have a classification task again, we can use
#    confusion matrices again!)
#
# 5) Adapt your k-fold cross-validation helper functions from previous labs so 
#    they work with logistic regression. Use your CV function to assess and 
#    compare the models' cross-validated performance.
#
# 6) Compare how the different definitions of the binary response (different 
#    cutoffs) influence the training performance, test performance, and 
#    cross-validation performance of your logistic regression models.
#
# 7) Experiment with improving the model fit by changing predictor sets, trying 
#    different cutoff values, adding interactions, removing redundant predictors, 
#    or applying transformations. Check whether any of these changes improve the 
#    predictive performance.
#
#
# ------------------------------------------------------------------------------




