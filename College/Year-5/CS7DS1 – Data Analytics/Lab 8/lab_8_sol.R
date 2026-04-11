# ------------------------------------------------------------------------------
# Exercise: Learn the true weights of a perceptron by gradient descent
# ------------------------------------------------------------------------------

# We will simulate data from a perceptron with "true" parameters.
# Your task is to recover these parameters using gradient descent.

# 1. Simulate data
#    - Set n = 100
#    - Draw x1 and x2 from rnorm()
#    - True weights: w1_true = 1.0, w2_true = -0.5, b_true = 0.2
#    - Generate y = w1_true*x1 + w2_true*x2 + b_true + noise
#      (noise = rnorm(n, sd = 0.1))

set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
w1_true <- 1.0
w2_true <- -0.5
b_true  <- 0.2

y <- w1_true*x1 + w2_true*x2 + b_true + rnorm(n, sd = 0.1)

# 2. Initialise estimated parameters
#    - Set w1, w2, b to arbitrary starting values, e.g. 0
#    - Choose a learning rate lr = 0.05

w1 <- 0
w2 <- 0
b  <- 0
lr <- 0.05

# 3. Define the prediction function:
#    y_pred = w1*x1 + w2*x2 + b

# 4. Activation function is the identity activation
#    f(y_pred) = y_pred

# 4. Define the loss function:
#    MSE = mean( (y_pred - y)^2 ) / 2

# 5. Compute gradients for w1, w2, b over the whole dataset (see PowerPoint):
#    grad_w1 = dL/dw1 = mean( ... )
#    grad_w2 = dL/dw2 = mean( ... )
#    grad_b  = dL/db  = mean( ... )

# 6. Gradient descent loop
#    - For epoch in 1:200:
#         - compute y_pred
#         - compute gradients
#         - update w1, w2, b
#         - store or print loss every 20 epochs
#
#    - After training, compare estimated weights with the true weights

num_epochs <- 200
for (epoch in 1:num_epochs) {
  
  y_pred <- w1*x1 + w2*x2 + b
  
  # gradients
  grad <- (y_pred - y)
  dw1 <- mean(grad * x1)
  dw2 <- mean(grad * x2)
  db  <- mean(grad)
  
  # update
  w1 <- w1 - lr * dw1
  w2 <- w2 - lr * dw2
  b  <- b  - lr * db
  
  if (epoch %% 20 == 0) {
    loss <- mean((y_pred - y)^2) / 2
    cat("Epoch:", epoch, "Loss:", loss, "\n")
  }
}

# 7. Report results:
cat("True w1:", w1_true, "Estimated w1:", w1, "\n")
cat("True w2:", w2_true, "Estimated w2:", w2, "\n")
cat("True b:",  b_true,  "Estimated b:",  b,  "\n")
