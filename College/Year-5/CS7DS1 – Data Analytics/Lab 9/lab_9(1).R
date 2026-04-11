# ------------------------------------------------------------------------------
# MNIST (Kaggle CSV) with torch in R
# Simple feed-forward neural net for digit recognition
# ------------------------------------------------------------------------------

library(torch)

# Make sure torch backend is installed once on the machine:
# torch::install_torch()

# 1) Load Kaggle data ----------------------------------------------------------
# Kaggle "Digit Recognizer"
# - column "label"   = digit 0..9
# - columns pixel0..pixel783 = 28x28 image flattened

mnist <- read.csv("labs/chapter_7/train.csv")  # adjust path if needed

dim(mnist)       # should be something like 42000 x 785
head(mnist[1:5]) # quick peek at first few columns


# 2) Separate features (X) and labels (y) -------------------------------------
# y = true digit labels
# X = all pixel columns

y <- mnist$label
X <- mnist[, grep("^pixel", names(mnist))]   # all columns starting with "pixel"
X <- as.matrix(X)

# Scale pixels from [0,255] to [0,1]
X <- X / 255


# 3) Train/test split ---------------------------------------------------------
# Split the data into training and test sets.
# Here: 32k for training, 10k for testing.

set.seed(123)
train_idx <- sample(nrow(X), 32000)

X_train <- X[train_idx, ]
y_train <- y[train_idx]

X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]

# torch's cross-entropy loss expects class indices starting at 1..K
# Kaggle labels are 0..9, so we add +1 to them
y_train <- y_train + 1
y_test  <- y_test  + 1


# 4) Convert to torch tensors -------------------------------------------------
# torch_tensor() turns ordinary R objects into torch tensors.
# - X_* become float tensors
# - y_* become integer (long) tensors with class indices

X_train_t <- torch_tensor(X_train, dtype = torch_float())
X_test_t  <- torch_tensor(X_test,  dtype = torch_float())

y_train_t <- torch_tensor(y_train, dtype = torch_long())
y_test_t  <- torch_tensor(y_test,  dtype = torch_long())

# Constants used for training
input_dim   <- ncol(X_train)            # number of input features (should be 784)
num_classes <- length(unique(y_train))  # number of classes (should be 10)


# 5) Define a simple neural network -------------------------------------------
# We build a multi-layer perceptron (MLP):
# - input layer: 784 features (pixels)
# - hidden layer: 128 units with ReLU
# - output layer: 10 units (one score per digit)

net <- nn_module(
  "MLP_MNIST",
  
  # This runs once when we create the model
  initialize = function() {
    # First fully-connected layer: 784 -> 128
    self$fc1 <- nn_linear(input_dim, 128)
    
    # Second fully-connected layer: 128 -> 10
    self$fc2 <- nn_linear(128, num_classes)
  },
  
  # This defines what happens when we call model(x)
  forward = function(x) {
    x |>
      self$fc1() |>   # linear layer 1
      nnf_relu() |>   # ReLU activation
      self$fc2()      # linear layer 2 → returns logits (unnormalised scores)
    
    # Note: we don't apply softmax here.
    # nnf_cross_entropy() will take care of softmax + cross-entropy for us.
  }
)

# Create a model instance
model <- net()


# 6) Optimizer -----------------------------------------------------------------
# We use Adam as the optimizer. It does gradient descent with some tweaks.
# model$parameters collects all weights and biases in the network.

optimizer <- optim_adam(model$parameters, lr = 0.001)


# 7) Training loop -------------------------------------------------------------
# We now train the model for a number of epochs.
# Each epoch:
#   - forward pass: compute predictions and loss
#   - backward pass: compute gradients via backpropagation
#   - optimizer step: update all parameters using the gradients

num_epochs <- 20

for (epoch in 1:num_epochs) {
  model$train()  # put model in "training mode" (relevant for dropout, etc.)
  
  optimizer$zero_grad()  # reset gradients to zero
  
  # Forward pass: predictions for all training data at once
  logits <- model(X_train_t)  # outputs shape: [n_train, 10]
  
  # Cross-entropy loss between logits and true labels
  loss <- nnf_cross_entropy(logits, y_train_t)
  
  # Backward pass: compute gradients of loss w.r.t. all parameters
  loss$backward()
  
  # Update step: apply Adam update to all parameters
  optimizer$step()
  
  # Compute training accuracy
  # with_no_grad(): we don't want gradients while just measuring accuracy
  with_no_grad({
    # For each row, pick the class index with the largest logit
    pred_train <- logits$argmax(dim = 2)
    
    # Compare predictions to true labels
    # TRUE counts as 1, FALSE as 0 when we sum.
    correct_train <- (pred_train == y_train_t)$sum()$item()
    
    # correct_train is now a plain R number with "how many were correct"
    train_acc <- correct_train / length(y_train)
  })
  
  cat(
    "Epoch:", epoch,
    "- loss:", as.numeric(loss),
    "- train acc:", round(train_acc, 4), "\n"
  )
}


# 8) Evaluation on the test set -----------------------------------------------
# Now we evaluate accuracy on data the model has not seen before.

model$eval()  # switch to "evaluation mode"

with_no_grad({
  logits_test <- model(X_test_t)
  
  # Again, take the class with the highest logit as the prediction
  pred_test <- logits_test$argmax(dim = 2)
  
  # pred_test == y_test_t → TRUE for correctly classified samples, FALSE otherwise
  # Summing converts TRUE/FALSE to 1/0 and counts how many are correct.
  correct_test <- (pred_test == y_test_t)$sum()$item()
  
  # Turn that into accuracy (proportion correct)
  test_acc <- correct_test / length(y_test)
})

cat("Test accuracy:", round(test_acc, 4), "\n")




# TO DO: Play around with this neural network routine, add more hidden layer,
#       include dropout and batch normalisation, try to increase the accuracy
#       as much as possible.




