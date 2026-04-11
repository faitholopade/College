# ---------------------------------------------------------------------
# Lab 2 — Entropy and ID3
# ---------------------------------------------------------------------

# 0) Libraries -----------------------------------------------------------------
# Install these beforehand if needed
library(dplyr)
library(rpart)
library(data.tree)
library(DescTools)  # for Entropy()
library(rpart.plot)


# 1) Data ----------------------------------------------------------------------
# Example from lecture: spam/ham dataset
emails <- data.frame(
  ID = c(376, 489, 541, 693, 782, 976),
  Suspicious_Words = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  Unknown_Sender   = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  Contains_Images  = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
  Class            = c("spam", "spam", "spam", "ham", "ham", "ham")
)

# ---------------------------------------------------------------------
# TO DO 1:
# Check that columns have the right data types/classes:
#   - "Class" is a categorical variable
#   - The three predictors are logical (TRUE/FALSE), which is fine
# Convert types where necessary.

# SOLUTION:
# "Class" column appeared as character, should ideally be a factor
emails$Class <- as.factor(emails$Class)



# 2 - Entropy and Information Gain ---------------------------------------------
# Check definition and calculation of entropy again

# Contingency table and probabilities for the target
ctable_class <- table(emails$Class)
ctable_class

# ---------------------------------------------------------------------
# TO DO 2:
# Convert the contingency table to probabilities and store in p_class.

p_class <- "Your code"

# SOLUTION:
p_class <- prop.table(ctable_class)   # Using baseR functions
p_class <- ctable_class / sum(ctable_class)   # Calculating by hand


# ---------------------------------------------------------------------
# TO DO 3:
# Write a function "entropy" to compute the entropy H of "Class" in bits.

entropy <- function(p) {
  # Your Code
}
# Get entropy for "Class" feature
H_class <- entropy(p = p_class)

# Sanity check using DescTools::Entropy (works with counts directly)
# (base = 2 uses log function with base 2 and returns bits)
H_class_check <- DescTools::Entropy(ctable_class, base = 2)


# SOLUTION:
# Function to calculate entropy of a probability vector
#   Input: p - probability vector from contingency table of a feature
#   Output: Entropy p
entropy <- function(p) {
  p <- p[p > 0]          # avoid log2(0)
  -sum(p * log2(p))
}
# Get entropy for "Class" feature
H_class <- entropy(p = p_class)



# 3 - Using "dplyr" to summarise data frame ------------------------------------
# This is just an example for illustration

summary_sender <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"),
            p_ham = n_ham / n,
            p_spam = n_spam / n) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(weighted_p = sum(weight * p_ham)) %>%
  pull(weighted_p)    # Returns the variable "weighted_p" as a vector

summary_sender


# Explore this pipeline step-by-step

# 1. group_by() and summarise() command
emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n())

# 2. After group_by() we have access to all other features too
emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"))

# 3. Inside summarise(), we can use the new variables straight away
emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"),
            p_ham = n_ham / n,
            p_spam = n_spam / n)

# 4. Using that summary data frame, we can add new variables
emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"),
            p_ham = n_ham / n,
            p_spam = n_spam / n) %>%
  mutate(weight = n / sum(n))

# 5. We can summarise() again and safe in new object
summary_sender <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"),
            p_ham = n_ham / n,
            p_spam = n_spam / n) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(weighted_n = sum(weight * n))

summary_sender

# Check type and class of object
typeof(summary_sender)
class(summary_sender)

# 6. Return as vector/numeric object
summary_sender <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"),
            p_ham = n_ham / n,
            p_spam = n_spam / n) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(weighted_n = sum(weight * n)) %>%
  pull(weighted_n)

# Check out type and class of object again
typeof(summary_sender)
class(summary_sender)



# 4 - Remainder and Information Gain (IG) --------------------------------------

# ---------------------------------------------------------------------
# TO DO 4:
# Adapt the pipeline below to compute the remainder for feature "Unknown_Sender"
# and store it in rem_sender (a single numeric value). Then compute IG.

rem_sender <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            # Calculate entropy H for a feature
            H = "Your Code") %>%
  mutate(weight = n / sum(n)) %>%
  # Calculated remainder of feature
  summarise(rem_sender = "Your code") %>%
  pull(rem_sender)

# Information gain (IG) is then:
IG_sender <- H_class - rem_sender


# SOLUTION:
rem_sender <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Class)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_sender = sum(weight * H)) %>%
  pull(rem_sender)

# Information gain (IG) is then:
IG_sender <- H_class - rem_sender


# ---------------------------------------------------------------------
# TO DO 4 (v2):
# Using summary_sender_v2 and your entropy() function to compute the remainder

summary_sender_v2 <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            n_ham = sum(Class == "ham"),
            n_spam = sum(Class == "spam"),
            p_ham = n_ham / n,
            p_spam = n_spam / n) %>%
  mutate(weight = n / sum(n))

summary_sender_v2

# As a reminder: How to access specific columns in a data frame
summary_sender_v2[1, "weight"]
summary_sender_v2[1, c("p_ham", "p_spam")]

rem_sender_v2 <- "Your code"

# Information gain (IG) is then:
IG_sender_v2 <- H_class - rem_sender_v2


# SOLUTION
rem_sender_v2 <- 
  as.numeric(summary_sender_v2[1, "weight"]) * 
    entropy(as.numeric(summary_sender_v2[1, c("p_ham", "p_spam")])) +
  as.numeric(summary_sender_v2[2, "weight"]) *
    entropy(as.numeric(summary_sender_v2[2, c("p_ham", "p_spam")]))

# Information gain (IG) is then:
IG_sender_v2 <- H_class - rem_sender_v2
IG_sender_v2


# ---------------------------------------------------------------------
# TO DO 5:
# Compute remainder and information gain for:
#   - Suspicious_Words
#   - Contains_Images
# (Copy your pipeline and change the grouping variable.)

# Suspicious_Words
rem_words <- emails %>%
  group_by(Suspicious_Words) %>%
  # Your Code

IG_words <- H_class - rem_words


# Contains_Images
rem_image <- emails %>%
  group_by(Contains_Images) %>%
  # Your Code
  
IG_image <- H_class - rem_image



# SOLUTION
# Suspicious_Words
rem_words <- emails %>%
  group_by(Suspicious_Words) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Class)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_words = sum(weight * H)) %>%
  pull(rem_words)

# IG for Suspicious_Words
IG_words <- H_class - rem_words
IG_words

# Unknown_Sender
rem_sender <- emails %>%
  group_by(Unknown_Sender) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Class)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_sender = sum(weight * H)) %>%
  pull(rem_sender)

# IG for Unknown_Sender
IG_sender <- H_class - rem_sender
IG_sender

# Contains_Images
rem_image <- emails %>%
  group_by(Contains_Images) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(Class)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_image = sum(weight * H)) %>%
  pull(rem_image)

# IG for Contains_Images
IG_image <- H_class - rem_image
IG_image




# 5 - ID3 in practice using rpart ----------------------------------------------
# Entropy, remainder, and information gain are the building blocks of ID3.
# Implementing full ID3 by hand is tedious, instead we’ll use the decision tree
# building function rpart().
# Note: rpart() uses by default the Gini impurity instead of entropy,
# but the principal behind building the tree is the same.


# -----------------------------------------------------------------------
# 5.1 Decision tree for emails dataset
# -----------------------------------------------------------------------

# Ensure predictors are factors (Class already converted earlier)
emails$Suspicious_Words <- factor(emails$Suspicious_Words)
emails$Unknown_Sender   <- factor(emails$Unknown_Sender)
emails$Contains_Images  <- factor(emails$Contains_Images)

str(emails)

# Fit a small-data tree:
# Because dataset is so small, control argument has to be used to change settings
emails_model <- rpart(
  Class ~ Suspicious_Words + Unknown_Sender + Contains_Images,
  data    = emails,
  method  = "class",
  control = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
)

# Inspect model and plot
# Using base R
plot(emails_model)
text(emails_model, use.n = TRUE)
# Using rpart specific function
rpart.plot(emails_model,
           type = 4,
           extra = "auto",
           roundint = FALSE,
           main = "Decision Tree")


# -----------------------------------------------------------------------
# 5.2 Decision tree for vegetation dataset
# -----------------------------------------------------------------------
veg <- data.frame(
  ID        = 1:7,
  Stream    = factor(c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)),
  Slope     = factor(c("steep","moderate","steep","steep","flat","steep","steep"),
                     levels = c("flat","moderate","steep")),
  Elevation = factor(c("high","low","medium","medium","high","highest","high"),
                     levels = c("low","medium","high","highest")),
  Vegetation = factor(c("chapparal","riparian","riparian","chapparal","conifer","conifer","chapparal"),
                      levels = c("chapparal", "riparian", "conifer"))
)

str(veg)

H_veg <- entropy(prop.table(table(veg$Vegetation)))

fit_veg <- rpart(
  Vegetation ~ Stream + Slope + Elevation,
  data = veg,
  method = "class",
  control = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
)

# Messy output at first but still worth looking at
summary(fit_veg)
# Plot using base R
plot(fit_veg)
text(fit_veg, use.n = TRUE, cex = 1)

# Plot using rpart specific function
rpart.plot(fit_veg,
           type = 4,
           extra = "auto",
           roundint = FALSE,
           main = "Decision Tree")


# 6) Homework —-----------------------------------------------------------------
# Build Your Own Decision Tree (WeatherPlay dataset)

# ------------------------------------------------------------------------------
# In this homework exercise, you will apply what you’ve learned to a new dataset.
# The "WeatherPlay" dataset (from the "partykit" package) is the classic example
# used to illustrate the ID3 algorithm. It contains information about weather
# conditions and whether or not to play outside.
#
# Your task:
#   1. Inspect the dataset (str(), summary(), head()) and check variable types.
#   2. If any variables appear as numeric (e.g., Temperature, Humidity),
#      convert them into categorical (factor) variables by grouping their values
#      into meaningful bins using the cut() function (see examples below).
#   3. Calculate the entropy of the target variable (Play).
#   4. Calculate the remainder and information gain for each predictor variable.
#   5. Build the decision tree by hand using your computed information gains.
#   6. Fit a tree using rpart() and compare your manual tree function output

# ------------------------------------------------------------------------------

# Load dataset
library(partykit)
data("WeatherPlay")

# Inspect structure
str(WeatherPlay)

# Example of how numeric columns could be converted to categorical bins:
# (The actual WeatherPlay dataset already stores these as factors,
# but this illustrates how you would handle numeric features if needed.)
WeatherPlay$temperature <- cut(WeatherPlay$temperature,
                               breaks = 3,
                               labels = c("cool", "mild", "hot"))

WeatherPlay$humidity <- cut(WeatherPlay$humidity,
                            breaks = 2,
                            labels = c("low", "high"))

# Inspect the final structure
str(WeatherPlay)


# ID3 manual decision tree -----------------------------------------------------
# Manually creating a decision tree using ID3 algorithm. PowerPoint slides
# contain the results from these calculations

# 1) Target feature entropy ----------------------------------------------------
p_play <- prop.table(table(WeatherPlay$play))

# Get entropy for "Class" feature
H_play <- entropy(p = p_play)


# 2) Part. entropy, remainder and IG for each predictor ------------------------

# remainder outlook
rem_outlook <- WeatherPlay %>%
  group_by(outlook) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(play)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_outlook = sum(weight * H)) %>%
  pull(rem_outlook)

# IG for outlook
IG_outlook <- H_play - rem_outlook
IG_outlook


# remainder temperature
rem_temperature <- WeatherPlay %>%
  group_by(temperature) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(play)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_temperature = sum(weight * H)) %>%
  pull(rem_temperature)

# IG for outlook
IG_temperature <- H_play - rem_temperature
IG_temperature


# remainder humidity
rem_humidity <- WeatherPlay %>%
  group_by(humidity) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(play)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_humidity = sum(weight * H)) %>%
  pull(rem_humidity)

# IG for Contains_Images
IG_humidity <- H_play - rem_humidity
IG_humidity


# remainder windy
rem_windy <- WeatherPlay %>%
  group_by(windy) %>%
  summarise(n = n(),
            H = entropy(prop.table(table(play)))) %>%
  mutate(weight = n / sum(n)) %>%
  summarise(rem_windy = sum(weight * H)) %>%
  pull(rem_windy)

# IG for outlook
IG_windy <- H_play - rem_windy
IG_windy


# Rank the IG values
# Based on IG: outlook > humidity > windy > temperature
ig_values <- c(IG_outlook, IG_temperature, IG_humidity, IG_windy)
sort(ig_values, decreasing = TRUE)
# outlook has the highest IG


# Creating summary dataframes for each feature to calculate part. entropy

# Summary df for "outlook" to calculate part. entropy
summary_outlook <- WeatherPlay %>%
  group_by(outlook) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_outlook

# Part. entropy outlook
entropy(summary_outlook[1, c(5, 6)])
entropy(summary_outlook[2, c(5, 6)])
entropy(summary_outlook[3, c(5, 6)])


# Summary df for "humidity" to calculate part. entropy
summary_humidity <- WeatherPlay %>%
  group_by(humidity) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_humidity

# Part. entropy humidity
entropy(summary_humidity[1, c(5, 6)])
entropy(summary_humidity[2, c(5, 6)])


# Summary df for "windy" to calculate part. entropy
summary_windy <- WeatherPlay %>%
  group_by(windy) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_windy

# Part. entropy windy
entropy(summary_windy[1, c(5, 6)])
entropy(summary_windy[2, c(5, 6)])


# Summary df for "temperature" to calculate part. entropy
summary_temperature <- WeatherPlay %>%
  group_by(temperature) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_temperature

# Part. entropy temperature
entropy(summary_temperature[1, c(5, 6)])
entropy(summary_temperature[2, c(5, 6)])
entropy(summary_temperature[3, c(5, 6)])


# Subset D1 (outlook = "sunny") ------------------------------------------------
D1 <- WeatherPlay %>%
  filter(outlook == "sunny")
D1

# Entropy of target feature in D1
H_D1_play <- entropy(prop.table(table(D1$play)))

# Summarise D1 regarding "humidity"
summary_D1_humidity <- D1 %>%
  group_by(humidity) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_D1_humidity

# Part entropy
entropy(summary_D1_humidity[1, c(5, 6)])
entropy(summary_D1_humidity[2, c(5, 6)])

# Remainder for humidity in D1
rem_D1_humidity <- 
  as.numeric(summary_D1_humidity[1, "weight"]) * 
  entropy(as.numeric(summary_D1_humidity[1, c("p_yes", "p_no")])) +
  as.numeric(summary_D1_humidity[2, "weight"]) *
  entropy(as.numeric(summary_D1_humidity[2, c("p_yes", "p_no")]))

# Information gain (IG) is then:
IG_D1_humidity <- H_D1_play - rem_D1_humidity
IG_D1_humidity


# Summarise D1 regarding "windy"
summary_D1_windy <- D1 %>%
  group_by(windy) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_D1_windy

# Part. entropy
entropy(summary_D1_windy[1, c(5, 6)])
entropy(summary_D1_windy[2, c(5, 6)])

# Remainder for windy in D1
rem_D1_windy <- 
  as.numeric(summary_D1_windy[1, "weight"]) * 
  entropy(as.numeric(summary_D1_windy[1, c("p_yes", "p_no")])) +
  as.numeric(summary_D1_windy[2, "weight"]) *
  entropy(as.numeric(summary_D1_windy[2, c("p_yes", "p_no")]))

# Information gain (IG) is then:
IG_D1_windy <- H_D1_play - rem_D1_windy
IG_D1_windy


# Summarise D1 regarding "temperature"
summary_D1_temperature <- D1 %>%
  group_by(temperature) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_D1_temperature

# Part entropy
entropy(summary_D1_temperature[1, c(5, 6)])
entropy(summary_D1_temperature[2, c(5, 6)])
entropy(summary_D1_temperature[3, c(5, 6)])

# Remainder for temperature in D1
rem_D1_temperature <- 
  as.numeric(summary_D1_temperature[1, "weight"]) * 
  entropy(as.numeric(summary_D1_temperature[1, c("p_yes", "p_no")])) +
  as.numeric(summary_D1_temperature[2, "weight"]) *
  entropy(as.numeric(summary_D1_temperature[2, c("p_yes", "p_no")]))

# Information gain (IG) is then:
IG_D1_temperature <- H_D1_play - rem_D1_temperature
IG_D1_temperature

# Highest IG in D1 has humidity -> perfectly separates yes/no.


# Create D3 and partition it ---------------------------------------------------
D3 <- WeatherPlay %>%
  filter(outlook == "rainy")
D3

# Entropy of target feature in D3
H_D3_play <- entropy(prop.table(table(D3$play)))


# Summarise D3 regarding "humidity"
summary_D3_humidity <- D3 %>%
  group_by(humidity) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_D3_humidity

# Part entropy
entropy(summary_D3_humidity[1, c(5, 6)])
entropy(summary_D3_humidity[2, c(5, 6)])

# Remaind for humidity in D3
rem_D3_humidity <- 
  as.numeric(summary_D3_humidity[1, "weight"]) * 
  entropy(as.numeric(summary_D3_humidity[1, c("p_yes", "p_no")])) +
  as.numeric(summary_D3_humidity[2, "weight"]) *
  entropy(as.numeric(summary_D3_humidity[2, c("p_yes", "p_no")]))

# Information gain (IG) is then:
IG_D3_humidity <- H_D3_play - rem_D3_humidity
IG_D3_humidity


# Summarise D3 regarding "windy"
summary_D3_windy <- D3 %>%
  group_by(windy) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_D3_windy

# Part. entropy
entropy(summary_D3_windy[1, c(5, 6)])
entropy(summary_D3_windy[2, c(5, 6)])

# Remainder for windy in D3
rem_D3_windy <- 
  as.numeric(summary_D3_windy[1, "weight"]) * 
  entropy(as.numeric(summary_D3_windy[1, c("p_yes", "p_no")])) +
  as.numeric(summary_D3_windy[2, "weight"]) *
  entropy(as.numeric(summary_D3_windy[2, c("p_yes", "p_no")]))

# Information gain (IG) is then:
IG_D3_windy <- H_D3_play - rem_D3_windy
IG_D3_windy


# Summarise D3 regarding "temperature"
summary_D3_temperature <- D3 %>%
  group_by(temperature) %>%
  summarise(n = n(),
            n_yes = sum(play == "yes"),
            n_no = sum(play == "no"),
            p_yes = n_yes / n,
            p_no = n_no / n) %>%
  mutate(weight = n / sum(n))
summary_D3_temperature

# Part entropy
entropy(summary_D3_temperature[1, c(5, 6)])
entropy(summary_D3_temperature[2, c(5, 6)])
entropy(summary_D3_temperature[3, c(5, 6)])

# Remainder for temperature in D3
rem_D3_temperature <- 
  as.numeric(summary_D3_temperature[1, "weight"]) * 
  entropy(as.numeric(summary_D3_temperature[1, c("p_yes", "p_no")])) +
  as.numeric(summary_D3_temperature[2, "weight"]) *
  entropy(as.numeric(summary_D3_temperature[2, c("p_yes", "p_no")]))

# Information gain (IG) is then:
IG_D3_temperature <- H_D3_play - rem_D3_temperature
IG_D3_temperature


# Highest IG in D3 → windy → perfectly separates yes/no.



# Create tree using rpart() to compare -----------------------------------------
# Don't forget, rpart() uses binary splits and the gini index in this setting.

# Decision tree from rpart()
fit_play <- rpart(
  play ~ outlook + humidity + windy + temperature,
  data = WeatherPlay,
  method = "class",
  control = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
)

# Compare tree output with ID3 tree in the PowerPoint slides. Outcomes for all
# scenarios should be identical
rpart.plot(fit_play,
           type = 4,
           extra = "auto",
           roundint = FALSE,
           main = "Decision Tree")
