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
set.seed(123)

# true coefficients
beta0_true <- -1
beta1_true <- 2

n <- 200
x <- rnorm(n, 0, 1)

eta <- beta0_true + beta1_true * x
p   <- 1 / (1 + exp(-eta))

y <- rbinom(n, 1, p)

sim_data <- data.frame(
  x = x,
  y = y
)


# Load simulated logistic regression dataset
sim_data <- readRDS("labs/chapter_6/logistic_sim_data.rds") # Adjust path

head(sim_data)

# x = numeric predictor
# y = binary response (0/1)

X <- cbind(1, sim_data$x) # this creates the design matrix
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
  eta <- as.vector(X %*% par)
  p   <- 1 / (1 + exp(-eta))
  
  eps <- 1e-10
  p <- pmin(pmax(p, eps), 1 - eps)
  
  -sum(y * log(p) + (1 - y) * log(1 - p))
}

# Start values for optim()
start <- rep(0, ncol(X))

# Get MLE estimates
fit_mle <- optim(start, negloglik_logit, X = X, y = y)
fit_mle$par

# Compare with glm()
fit_glm <- glm(y ~ x, data = sim_data, family = binomial)
coef(fit_glm)
# In this case, the coefficients should be identical
# EDIT: I realised they are not, this is because slight differences in optimisers
#     and how glm() handles the boundary cases of p = 0 and p = 1. But the coefs
#     are near identical.




