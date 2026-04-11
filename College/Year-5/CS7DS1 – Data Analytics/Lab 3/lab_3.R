# ------------------------------------------------------------------------------
# Lab 3 - Entropy, Gini index, Continuous splitting, IG ratio, Random Forest
# ------------------------------------------------------------------------------

# 0) - Libraries ----------------------------------------------------------
library(dplyr)
library(rpart)
library(rpart.plot)


# Exercise 1 --------------------------------------------------------------
# Entropy, Gini index, Continuous splitting, IG ratio

# Download "streaming.rds" from blackboard, then load it into R
# (use your path where you saved the data)
streaming <- readRDS("your_path/to_data/streaming.rds")


# ------------------------------------------------------------------------------
# Dataset Description — Streaming Service Example
# ------------------------------------------------------------------------------
# This dataset describes users of a streaming platform and their chosen
# subscription plan. Each row corresponds to one individual user.
# The goal is to predict which subscription plan a user is most likely
# to choose based on their age, viewing habits, and device preferences.
#
# There are four descriptive features and one target feature in this dataset:
#
# AGE: a continuous feature listing the age of the individual.
# DEVICE TYPE: a categorical feature indicating the main device used
#   to stream content (mobile, laptop, smart TV).
# VIEWING PARTNER: a categorical feature describing with whom the individual
#   most often watches content (solo, couple, family).
# PREFERRED CONTENT: a categorical feature listing the type of content
#   most frequently streamed (music, movies, podcasts, anime).
# PLAN TYPE: the target feature with three levels (Free, Standard, Premium).
# ------------------------------------------------------------------------------


# TO DO 1: Check data for correct data types and change accordingly

# Your code


# TO DO 2: Calculate the entropy for this dataset

# Your code


# TO DO 3: Calculate the Gini index for this dataset

# Your code


# TO DO 4: Find the optimal thresholds to split the continuous AGE feature and
# calculate partition entropy, remainder and information gain for each new label

# Your code


# TO DO 5: Calculate information gain (based on entropy) for the features
# "Device_Type" , "Viewing_Partner", and "Preferred_Content"

# Your code


# TO DO 6: Calculate the information gain ratio (based on entropy) for the
# features "Device_Type" , "Viewing_Partner", and "Preferred_Content" 

# Your code


# TO DO 7: Calculate information gain using the Gini index for the features
# "Device_Type" , "Viewing_Partner", and "Preferred_Content" 

# Your code






# Exercise 2 --------------------------------------------------------------
# Random Forest on a small social media well being study

# The following dataset contains the details of five participants in a
# social media well being study, and a target feature "Burnout_Risk" which
# describes each participant’s risk of social media burnout. Each participant is
# described in terms of four descriptive features:

#
# App_Usage — how often the participant opens social apps
#     Values: daily, weekly, rarely
#
# Notifications_On — are push notifications enabled?
#     Values: no, yes
#
# Multiple_Accounts — does the participant use more than one social account?
#     Values: no, yes
#
# Has_Close_Friends — does the participant maintain close offline friendships?
#     Values: yes, no
#
# Burnout_Risk — the target feature describing burnout risk
#     Values: low, high
#
#
# As part of the study, researchers want to create a predictive model to
# identify participants at risk of social media burnout. You are asked to
# implement this model using a "Random Forest". The three datasets provided below
# in the exercise list bootstrap samples that have been generated from the dataset.




# Common level orders (keep these consistent across all tables)
levels_app     <- c("rarely", "weekly", "daily")
levels_yesno   <- c("no", "yes")
levels_risk    <- c("low", "high")


# Main dataset
burnout <- data.frame(
  ID = 1:5,
  App_Usage         = factor(c("rarely", "weekly", "rarely", "daily", "daily"),
                             levels = levels_app),
  Notifications_On  = factor(c("no", "yes", "no", "yes", "yes"),
                             levels = levels_yesno),
  Multiple_Accounts = factor(c("no", "no", "no", "yes", "yes"),
                             levels = levels_yesno),
  Has_Close_Friends = factor(c("yes", "yes", "no", "yes", "no"),
                             levels = levels_yesno),
  Burnout_Risk      = factor(c("low", "high", "low", "high", "high"),
                             levels = levels_risk)
)

burnout


# Bootstrap Sample A ------------------------------------------------------
boot_A <- data.frame(
  ID = c(1, 2, 2, 5, 5),
  App_Usage         = factor(c("rarely", "weekly", "weekly", "daily", "daily"),
                             levels = levels_app),
  Has_Close_Friends = factor(c("yes", "yes", "yes", "no", "no"),
                             levels = levels_yesno),
  Burnout_Risk      = factor(c("low", "high", "high", "high", "high"),
                             levels = levels_risk)
)

boot_A


# Bootstrap Sample B ------------------------------------------------------
boot_B <- data.frame(
  ID = c(1, 2, 2, 4, 5),
  Notifications_On  = factor(c("no", "yes", "yes", "yes", "yes"),
                             levels = levels_yesno),
  Multiple_Accounts = factor(c("no", "no", "no", "yes", "yes"),
                             levels = levels_yesno),
  Burnout_Risk      = factor(c("low", "high", "high", "high", "high"),
                             levels = levels_risk)
)

boot_B



# Bootstrap Sample C ------------------------------------------------------
boot_C <- data.frame(
  ID = c(1, 1, 2, 4, 5),
  Multiple_Accounts = factor(c("no", "no", "no", "yes", "yes"),
                             levels = levels_yesno),
  Has_Close_Friends = factor(c("yes", "yes", "yes", "yes", "no"),
                             levels = levels_yesno),
  Burnout_Risk      = factor(c("low", "low", "high", "high", "high"),
                             levels = levels_risk)
)

boot_C



# TO DO 1: Using these bootstrap samples, create the corresponding decision trees
# that will be in the random forest model (use entropy based information gain as
# the feature selection criterion).



# TO DO 2: Assuming the random forest model you have created uses majority voting,
# what prediction will it return for the following query:

# Prediction query --------------------------------------------------------
query <- data.frame(
  App_Usage         = factor("daily"),
  Notifications_On  = factor("no"),
  Multiple_Accounts = factor("yes"),
  Has_Close_Friends = factor("yes")
)

query


# TO DO 3: Create bootstrap trees using the rpart() function and re-do the
# majority voting in TO DO 2. Compare your result.

# Don't forget the argument "control = rpart.control(minsplit=2, minbucket=1, cp=0)"
# in the rpart() function, or play around with different settings.

# Assuming your tree model for bootstrap sample A is stored in "tree_A", you can
# predict from it using:
predict(tree_A, newdata = query, type = "class")
# newdata has to be an object of class "data.frame"





