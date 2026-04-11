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

# Check data types of variables
head(streaming)
str(streaming)  # Convert "chr" to "factor" 

streaming$Device_Type <- as.factor(streaming$Device_Type)
streaming$Viewing_Partner <- as.factor(streaming$Viewing_Partner)
streaming$Preferred_Content <- as.factor(streaming$Preferred_Content)
streaming$Plan_Type <- as.factor(streaming$Plan_Type)

str(streaming)


# TO DO 2: Calculate the entropy for this dataset
entropy <- function(p) {
  p <- p[p > 0] # avoid 0 probabilities
  -sum(p * log2(p))
}

# Prob table of target feature
p_plan_type <- prop.table(table(streaming$Plan_Type))

# Entropy of traget feature
H_plan_type <- entropy(p_plan_type)
H_plan_type # Answer: 1.2988


# TO DO 3: Calculate the Gini index for this dataset
gini <- function(p) {
  p <- p[p > 0] # Technically not necessary since no log, but still safe to do.
  1 - sum(p^2)
}

# Gini index of target feature
gini_plan_type <- gini(p_plan_type)
gini_plan_type # Answer: 0.5313


# TO DO 4: Find the optimal thresholds to split the continuous AGE feature and
# calculate partition entropy, remainder and information gain for each new label

# Sort target feature by the continuous variable
streaming %>% select(Age, Plan_Type) %>% arrange(Age)
# Possible split points are therefore at "Age" values 26, 39.5, and 45. Add
# a new boolean feature variable now for each of those split points and calculate
# entropy.

# Split point: 26
streaming$Age_cut_26 <- cut(unlist(streaming$Age),
                            breaks = c(-Inf, 26, Inf),
                            labels = c("<26", ">26"))

# Summary df for Age cutoff at 26 to calculate part. entropy
summary_age_26 <- streaming %>%
  group_by(Age_cut_26) %>%
  summarise(n = n(),
            n_free = sum(Plan_Type == "Free"),
            n_standard = sum(Plan_Type == "Standard"),
            n_premium = sum(Plan_Type == "Premium"),
            p_free = n_free / n,
            p_standard = n_standard / n,
            p_premium = n_premium / n) %>%
  mutate(weight = n / sum(n))
summary_age_26

# Part. entropy for Plan_Type with first cutoff (26)
entropy(summary_age_26[1, c("p_free", "p_standard", "p_premium")]) # Answer: 0
entropy(summary_age_26[2, c("p_free", "p_standard", "p_premium")]) # Answer: 0.6500

# Remainder for "Age > 26"
rem_gini_age_26 <- 
  as.numeric(summary_age_26[1, "weight"]) * 
  entropy(as.numeric(summary_age_26[1, c("p_free", "p_standard", "p_premium")])) +
  as.numeric(summary_age_26[2, "weight"]) *
  entropy(as.numeric(summary_age_26[2, c("p_free", "p_standard", "p_premium")]))
# Answer: 0.4875

# Information gain (IG) is then:
IG_age_26 <- H_plan_type - rem_gini_age_26
IG_age_26 # Answer: 0.8113


# Split point: 39.5
streaming$Age_cut_39.5 <- cut(unlist(streaming$Age),
                            breaks = c(-Inf, 39.5, Inf),
                            labels = c("<39.5", ">39.5"))

# Summary df for Age cutoff at 26 to calculate part. entropy
summary_age_39.5 <- streaming %>%
  group_by(Age_cut_39.5) %>%
  summarise(n = n(),
            n_free = sum(Plan_Type == "Free"),
            n_standard = sum(Plan_Type == "Standard"),
            n_premium = sum(Plan_Type == "Premium"),
            p_free = n_free / n,
            p_standard = n_standard / n,
            p_premium = n_premium / n) %>%
  mutate(weight = n / sum(n))
summary_age_39.5

# Part. entropy for Plan_Type with first cutoff (26)
entropy(summary_age_39.5[1, c("p_free", "p_standard", "p_premium")]) # Answer: 0.9710
entropy(summary_age_39.5[2, c("p_free", "p_standard", "p_premium")]) # Answer: 0.9183

# Remainder for "Age > 26"
rem_gini_age_39.5 <- 
  as.numeric(summary_age_39.5[1, "weight"]) * 
  entropy(as.numeric(summary_age_39.5[1, c("p_free", "p_standard", "p_premium")])) +
  as.numeric(summary_age_39.5[2, "weight"]) *
  entropy(as.numeric(summary_age_39.5[2, c("p_free", "p_standard", "p_premium")]))
# Answer: 0.9512

# Information gain (IG) is then:
IG_age_39.5 <- H_plan_type - rem_gini_age_39.5
IG_age_39.5 # Answer: 0.8113


# Split point: 45
streaming$Age_cut_45 <- cut(unlist(streaming$Age),
                              breaks = c(-Inf, 45, Inf),
                              labels = c("<45", ">45"))

# Summary df for Age cutoff at 26 to calculate part. entropy
summary_age_45 <- streaming %>%
  group_by(Age_cut_45) %>%
  summarise(n = n(),
            n_free = sum(Plan_Type == "Free"),
            n_standard = sum(Plan_Type == "Standard"),
            n_premium = sum(Plan_Type == "Premium"),
            p_free = n_free / n,
            p_standard = n_standard / n,
            p_premium = n_premium / n) %>%
  mutate(weight = n / sum(n))
summary_age_45

# Part. entropy for Plan_Type with first cutoff (26)
entropy(summary_age_45[1, c("p_free", "p_standard", "p_premium")]) # Answer: 1.4591
entropy(summary_age_45[2, c("p_free", "p_standard", "p_premium")]) # Answer: 0

# Remainder for "Age > 26"
rem_gini_age_45 <- 
  as.numeric(summary_age_45[1, "weight"]) * 
  entropy(as.numeric(summary_age_45[1, c("p_free", "p_standard", "p_premium")])) +
  as.numeric(summary_age_45[2, "weight"]) *
  entropy(as.numeric(summary_age_45[2, c("p_free", "p_standard", "p_premium")]))
# Answer: 1.0944


# Information gain (IG) is then:
IG_age_45 <- H_plan_type - rem_gini_age_45
IG_age_45 # Answer: 0.2044


# Conclusion: Threshold of 26 has the highest IG and consequently is the best
# threshold to use if we are splitting the dataset using the "Age" feature



# TO DO 5: Calculate information gain (based on entropy) for the features
# "Device_Type" , "Viewing_Partner", and "Preferred_Content"

# remainder Device_Type
rem_gini_device_type <- streaming %>%
  group_by(Device_Type) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Plan_Type)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_gini_device_type = sum(weight * H)) %>%
  pull(rem_gini_device_type)

# IG for Device_Type
IG_device_type <- H_plan_type - rem_gini_device_type
IG_device_type # Answer: 0.7988


# remainder Viewing_Partner
rem_gini_viewing_partner <- streaming %>%
  group_by(Viewing_Partner) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Plan_Type)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_gini_device_type = sum(weight * H)) %>%
  pull(rem_gini_device_type)

# IG for Device_Type
IG_viewing_partner <- H_plan_type - rem_gini_viewing_partner
IG_viewing_partner # Answer: 0.5488


# remainder Preferred_Content
rem_gini_preferred_content <- streaming %>%
  group_by(Preferred_Content) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Plan_Type)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_gini_preferred_content = sum(weight * H)) %>%
  pull(rem_gini_preferred_content)

# IG for preferred_content
IG_preferred_content <- H_plan_type - rem_gini_preferred_content
IG_preferred_content # Answer: 0.7044



# TO DO 6: Calculate the information gain ratio (based on entropy) for the
# features "Device_Type" , "Viewing_Partner", and "Preferred_Content" 

# IG ratio of "Device_Type"
H_device_type <- entropy(prop.table(table(streaming$Device_Type)))
IGR_device <- IG_device_type / H_device_type # Answer: 0.5683

# IG ratio of "Viewing_Partner"
H_viewing_partner <- entropy(prop.table(table(streaming$Viewing_Partner)))
IGR_viewing_partner <- IG_viewing_partner / H_viewing_partner # Answer: 0.3904

# IG ratio of "Preferred_Content"
H_preferred_content <- entropy(prop.table(table(streaming$Preferred_Content)))
IGR_preferred_content <- IG_preferred_content / H_preferred_content # Answer: 0.3697



# TO DO 7: Calculate information gain using the Gini index for the features
# "Device_Type" , "Viewing_Partner", and "Preferred_Content" 

# remainder Device_Type
rem_gini_device_type <- streaming %>%
  group_by(Device_Type) %>%
  summarise(n = n(),
            H = gini(prop.table(table(Plan_Type)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_gini_device_type = sum(weight * H)) %>%
  pull(rem_gini_device_type)

# IG for Device_Type
IG_gini_device_type <- gini_plan_type - rem_gini_device_type
IG_gini_device_type # Answer: 0.2813


# remainder Viewing_Partner
rem_gini_viewing_partner <- streaming %>%
  group_by(Viewing_Partner) %>%
  summarise(n = n(),
            H = gini(prop.table(table(Plan_Type)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_gini_device_type = sum(weight * H)) %>%
  pull(rem_gini_device_type)

# IG for Device_Type
IG_gini_viewing_partner <- gini_plan_type - rem_gini_viewing_partner
IG_gini_viewing_partner # Answer: 0.1771


# remainder Preferred_Content
rem_gini_preferred_content <- streaming %>%
  group_by(Preferred_Content) %>%
  summarise(n = n(),
            H = gini(prop.table(table(Plan_Type)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_gini_preferred_content = sum(weight * H)) %>%
  pull(rem_gini_preferred_content)

# IG for preferred_content
IG_gini_preferred_content <- gini_plan_type - rem_gini_preferred_content
IG_gini_preferred_content # Answer: 0.2396






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

# Answer: See power point slides



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

# Answer: See power point slides (prediction is "high")

# TO DO 3: Create bootstrap trees using the rpart() function and re-do the
# majority voting in TO DO 2. Compare your result.

# Don't forget the argument "control = rpart.control(minsplit=2, minbucket=1, cp=0)"
# in the rpart() function, or play around with different settings.

# Assuming your tree model for bootstrap sample A is stored in "tree_A", you can
# predict from it using:
predict(tree_A, newdata = query, type = "class")
# newdata has to be an object of class "data.frame"


# Create tree for bootstrap sample A
tree_A <- rpart(Burnout_Risk ~ App_Usage + Has_Close_Friends,
                data = boot_A,
                method = "class",
                control = rpart.control(minsplit=2, minbucket=1, cp=0))

# Predict from tree_A
predict(tree_A, newdata = query, type = "class")


# Create tree for bootstrap sample B
tree_B <- rpart(Burnout_Risk ~ Notifications_On + Multiple_Accounts,
                data = boot_B,
                method = "class",
                control = rpart.control(minsplit=2, minbucket=1, cp=0))

# Predict from tree_B
predict(tree_B, newdata = query, type = "class")


# Create tree for bootstrap sample C
tree_C <- rpart(Burnout_Risk ~ Multiple_Accounts + Has_Close_Friends,
                data = boot_C,
                method = "class",
                control = rpart.control(minsplit=2, minbucket=1, cp=0))

# Predict from tree_C
predict(tree_C, newdata = query, type = "class")


# Answer: Same predictions, majority vote is still "high"

