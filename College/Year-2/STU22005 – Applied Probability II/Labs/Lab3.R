# Create a vector containing the data
data <- c(13.1, 19.6, 21.3, 11.6, 15.4, 23.7, 18.6, 16.1, 19.3, 17.4, 21.5, 16.8, 14.9)
# Print out the data
data
# Generate a histogram
hist(data)
# Find the confidence interval using t.test (default is 95% confidence interval)
t.test(data)
?t.test
# Find the 90% confidence interval using t.test
t.test(data, conf.level = 0.9)
# Create a function where x is the data and alpha to determine the level
# of confidence (100*(1-alpha)%)
CI <- function(x, alpha, round) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(n)
  tval <- qt(0.5*alpha, df = n-1, lower.tail=FALSE )
  round(c( m - tval * se, m + tval * se ), round)
}
# Using the CI function
CI(x = data, alpha = 0.05, round = 2)
# Using the CI function
CI(x = data, alpha = 0.1, round = 2)
# Simulate 1000 datasets of size 15 from N(35, 9) and store in a matrix
set.seed(2646537)
mean_val = 35
sd_val = 3
samples = 1000
size = 15
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)
# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

#1.
data <- c(69, 74, 79, 81, 85, 86, 89, 90, 94, 97, 100, 105)
data
hist(data)
t.test(data,  conf.level = 0.99)

#2.
CI <- function(x, alpha, round){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(n)
  tval <- qt(0.5*alpha, df = n-1, lower.tail=FALSE)
  round(c(m - tval * se, m + tval * se), round)
}
CI(x = data, alpha = 0.01, round = 2)

#3.
data <- c(69, 74, 79, 81, 85, 86, 89, 90, 94, 97, 100, 105)
data
hist(data)
t.test(data,  conf.level = 0.90)
CI <- function(x, alpha, round){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(n)
  tval <- qt(0.5*alpha, df = n-1, lower.tail=FALSE)
  round(c(m - tval * se, m + tval * se), round)
}
CI(x = data, alpha = 0.1, round = 2)

#4.
set.seed(9498)
mean_val = 50
sd_val = 4
samples = 10000
size = 20
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)

head(x_sim)


coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)

