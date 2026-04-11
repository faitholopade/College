# 0) - Libraries ----------------------------------------------------------
library(dplyr)


# Exercise 1 --------------------------------------------------------------
# Multinomial Naive-Bayes toy example

# Small training dataframe
df <- data.frame(
  free    = c(2,1,0,0,0,1,0,1),
  win     = c(1,0,2,0,0,0,0,1),
  meeting = c(0,0,0,2,1,1,0,0),
  class   = factor(c("spam","spam","spam","ham","ham","ham","ham","spam"))
)


# TO DO 1: Calculate the priors for each class (p(spam), p(ham))
table(df$class)



# TO DO 2: Calculate the conditional probabilities for each word in each class

# WE NEED SMOOTHING HERE:
# If a word never appeared in a class (e.g. "meeting" in spam here),
# then P(meeting | spam) = 0. In the Multinomial NB model, the likelihood
# is a product of all conditional probabilities raised to their word counts.
# If any term is zero and the count for that word in a new query count > 0,
# the entire product becomes zero, wiping out that class probability completely.
#
# Laplace smoothing fixes this by adding a small "pseudo count" alpha to every word:
#   numerator = count(w, c) + alpha
#   denominator = total_words_in_c + alpha * V
# where:
#   alpha = 1  (Laplace smoothing)
#   V = number of unique words or features (3 here: free, win, meeting)
#
# This prevents zero probabilities and keeps all classes comparable.


# alpha and V for Laplace smoothing
alpha <- 1
V <- 3

# Using Laplace smoothing, calculate conditional probabilities:
#   P(free | spam), P(win | spam), P(meeting | spam)
#   P(free | ham),  P(win | ham),  P(meeting | ham)
#
# (Hint: sum word counts for each class, then apply the formula above.)



# TO DO 3: Compute the probabilities and prediction for a new email (the query)
query <- c(free = 2, win = 1, meeting = 0)

# You now have all the components you need to make predictions using Bayes rule!
# So use your conditional probabilities and priors to calculate:
#
# p(query | spam)
# p(query | ham)
#
# and then the posterior probabilities:
#
# p(spam | query)
# p(ham | query)

## IMPORTANT !!!
# When multiplying many small probabilities, numerical underflow can occur.
# A practical solution is to work in the logarithm space

# Review the rules for applying log to multiplications and exponents.
# You can always exponentiate at the end if you want to recover probabilities






# ------------------------------------------------------------------------------




# Exercise 2 --------------------------------------------------------------
# Apply a Multinomial Naive Bayes classifier using the "naivebayes" package

# TO DO 1: Load required package and the email_clean.csv dataset
# TO DO 2: Split the dataset into training (90%) and test (10%) sets
# TO DO 3: Fit a Naive Bayes model with laplace = 1
# TO DO 4: Use predict() to classify the test set
# TO DO 5: Create and print a confusion matrix using table()
# TO DO 6: Compute the model accuracy and interpret it
# -------------------------------------------------------------------------



# TO DO 1: Load required package and the email_clean.csv dataset
install.packages("naivebayes")
library(naivebayes)

# Load dataset (could take a few seconds to load)
emails <- NULL # Your code/path

# Last column is the target feature "Class" !!! CAPITAL "C" !!!
colnames(emails)[ncol(emails)]
head(emails[, ncol(emails)])
  
emails$Class <- as.factor(emails$Class)



# TO DO 2: Split the dataset into training (90%) and test (10%) sets

# Set a seed for reproducibility 
set.seed(1234)

# ------------------------------------------------------------------------------
# Example for using sample() function
tmp_vec <- round(runif(n = 10, min = 15, max = 25), digits = 2)
tmp_n <- length(tmp_vec)

seq_len(tmp_n)
tmp_idx <- sample(x = seq_len(tmp_n), size = 0.9 * tmp_n, replace = FALSE)
tmp_idx

tmp_vec[tmp_idx]
tmp_vec[-tmp_idx] # There is a minus (-) sign here
# ------------------------------------------------------------------------------

# Now split "emails" into 90% train and 10% test
dim(emails)
train_idx <- sample("your code")

emails_train <- emails["your code"] # Remember that "emails" is 2 dimensional
emails_test <- NULL # Your code

# Divide training and test sets into X (input features) and y (target feature)
X_train <- emails_train[, -ncol(emails_train)] # Negative here because we exclude last column
y_train <- emails_train[, "Class"]

X_test <- emails_test[, -ncol(emails_test)] # Negative here because we exclude last column
y_test <- emails_test[, "Class"]



# TO DO 3: Fit a multinomial Naive Bayes model with laplace = 1 using the
# "naivebayes" package

# Find the right function using ?"package_name-package", check the help page
# (?function_name) for required parameters
?"naivebayes-package"

# your code




# TO DO 4: Use predict() to classify the test set (newdata = )

# The predict function for a multinomial NB model in the "naivebayes" package
# requires a matrix for "newdata", not a data frame
pred <- NULL # Your code



# TO DO 5: Print a confusion matrix using table() to evaluate the performance

# ------------------------------------------------------------------------------
# Example:
tmp1 <- c(1, 1, 1, 1, 0, 0, 0, 0)
tmp2 <- c(1, 1, 1, 0, 0, 0, 0, 1)
table(predictions = tmp1, actual = tmp2)
# ------------------------------------------------------------------------------

# your code



# TO DO 6: Compute the model accuracy and interpret it

# Hint: Compare if the individual values of two vectors are the same
tmp1 == tmp2
# We then want a probability
mean(tmp1 == tmp2)


