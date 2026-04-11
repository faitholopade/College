# 0) Install and Load libraries -------------------------------------------

# Install the required packages only if missing, otherwise just load. 
# Make sure the package name is in quotation marks
install.packages("arrow")

# Load required libraries. "arrow" is required to load a ".parquet" file
library(arrow)


# 1) Working directory (WD) & project paths -------------------------------
# Best practice: use an RStudio Project and relative paths
# getwd() shows the current working directory:
getwd()

# Setting a WD for demonstration:
# (Avoid hard-coding personal paths in shared material)
# Replace "your_path" with your actual path
setwd("your_path/Data_Analytics/Lab1/")


# 2) Load data ------------------------------------------------------------
# Read TLC Yellow Taxi July 2023 data into R session
# Use a relative path inside your working directory:
df <- read_parquet("labs/chapter_3/data/yellow_tripdata_2023-07.parquet")
# Equivalent command:
df <- arrow::read_parquet("labs/chapter_3/data/yellow_tripdata_2023-07.parquet")

# In R, tabular data is commonly stored in "data.frame"s (or tibbles).
is.data.frame(df)
head(df)


# 3) First look -----------------------------------------------------------
# How many rows (observations) and columns (variables)?
nrow(df)       # rows
ncol(df)       # cols
dim(df)        # rows, cols (same info)

# Column names:
names(df)

# "str()" gives a compact overview (data types + a few values):
str(df)

# Only column classes: 
sapply(df, class)           
sapply(df, function(x) {    # Same command, same output
    class(x)
  }
)

# A mixed summary across all columns:
summary(df)


# 4) Convert categorical variables to explicit factors --------------------
# Use summary() and str() to assess variables
str(df)
summary(df)

# Also see link for TLC Trip Record Data
# https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page
# Try and find description of variables

# Some "integer" or "character" variables are actually categories.
# Convert them step-by-step
df$VendorID <- as.factor(df$VendorID)
df$RatecodeID <- as.factor(df$RatecodeID)
df$store_and_fwd_flag <- as.factor(df$store_and_fwd_flag)
df$DOLocationID <- as.factor(df$DOLocationID)
df$PULocationID <- as.factor(df$PULocationID)
df$payment_type <- as.factor(df$payment_type)

# Add a new variable to df, change factor levels
df$store_and_fwd_flag_2 <- factor(df$store_and_fwd_flag,
                                levels = c("N", "Y"),
                                labels = c("No", "Yes"))

# Re-run the summary() now that factors are set (factors are now frequency tables)
summary(df)


# 5) Missing values & Simple sanity checks --------------------------------
# NA counts per column
na_counts <- colSums(is.na(df))
na_counts

# Example with for-loop (don't use loops whenever possible)
na_counts_loop_2 <- numeric(ncol(df))
names(na_counts_loop_2) <- names(df)
for (j in seq_along(df)) {
  na_counts_loop_2[j] <- sum(is.na(df[[j]]))
}
na_counts_loop_2

# Zero counts per column
zero_counts <- colSums(df==0)
zero_counts

# Put them into a small table
na_zero_table <- data.frame(
  # variable = names(df),    # double counting in this case
  n_NA = na_counts,
  n_zero = zero_counts
)
na_zero_table

# Use specific package for data quality table
# link to CRAN "https://cran.r-project.org/web/packages/dlookr/index.html" contains
# manual and vignettes
install.packages("dlookr")
library(dlookr)

# Example function
diagnose(df)


# 6) Outlier checks and plots ---------------------------------------------
# Extract as a numeric vector (drop NAs for clean plotting)
fa <- df$fare_amount
anyNA(fa)

# Boxplot of fare_amount (vertical)
boxplot(fa,
        main = "Fare amount — boxplot",
        ylab = "USD")   # show outlier points (default TRUE)

# Horizontal boxplot (same data, flipped)
boxplot(fa,
        horizontal = TRUE,
        main = "Fare amount — horizontal boxplot",
        xlab = "USD")

# Histogram of all fares 
hist(fa,
     main = "Fare amount — histogram (all data)",
     xlab = "USD")

# Increase amount of bars
hist(fa,
     breaks = 100,
     main = "Fare amount — histogram (all data)",
     xlab = "USD")

# Filter fare amount for values between 0 and 250
fa_0_250 <- fa[fa < 250 & fa > 0]

# Histogram of filtered fares with 100 bins
hist(fa_0_250,
     breaks = 100,                 # bins=100
     main   = "Fare amount (0–250) — 100 bins",
     xlab   = "USD")

# Same histogram but with log values
hist(log(fa_0_250),
     breaks = 100,                 # bins=100
     main   = "log of fare amount (0–250) — 100 bins",
     xlab   = "USD")


# Introducing "dplyr" for data pipelines
install.packages("dplyr")
library(dplyr)

# Filter to (0, 250) and drop NAs
fa_0_250_v2 <- df %>%
  filter(!is.na(fare_amount),
         fare_amount > 0,
         fare_amount < 250) %>%
  select(fare_amount) %>% unlist

# Histogram of filtered fares with 100 bins
hist(fa_0_250_v2,
     breaks = 100,                 # bins=100
     main   = "Fare amount v2 (0–250) — 100 bins",
     xlab   = "USD")


# Histograms of all numeric variables
# 1) Pick numeric columns
is_num   <- sapply(df, is.numeric)
num_vars <- names(df)[is_num]

# 2) Choose a grid that fits all panels
k  <- length(num_vars)
nc <- ceiling(sqrt(k))         # columns
nr <- ceiling(k / nc)          # rows

# 3) Draw histogram panels
old_par <- par(mfrow = c(nr, nc),  # grid layout
               mar   = c(3.5, 3.5, 3, 1))  # margins: bottom, left, top, right

for (v in num_vars) {
  x <- df[[v]]
  hist(x,
       breaks = 50,            # adjust bin count if you like
       main   = v,
       xlab   = "",
       ylab   = "Count")
}
par(old_par)                    # restore graphics settings


# Scatterplot between two variables
# keep plausible values
keep <- is.finite(df$trip_distance) & is.finite(df$fare_amount) &
  df$trip_distance > 0 & df$trip_distance <= 50 &
  df$fare_amount   > 0 & df$fare_amount   <= 250

x <- df$trip_distance[keep]
y <- df$fare_amount[keep]

# downsample to reduce overplotting (adjust N as you like)
set.seed(42)
idx <- sample.int(length(x), min(10000, length(x)))
x <- x[idx] 
y <- y[idx]

plot(x, y,
     pch = 19, cex = 0.4, col = adjustcolor("black", 0.25),
     xlab = "Trip distance (miles)", ylab = "Fare amount (USD)",
     main = "Fare vs Trip distance (sampled)")
grid()
abline(lm(y ~ x), col = "red", lwd = 2)  # quick trend line


# Introducing ggplot2
install.packages("ggplot2")
library(ggplot2)

df_plot <- subset(df,
                  is.finite(trip_distance) & is.finite(fare_amount) &
                    trip_distance > 0 & trip_distance <= 50 &
                    fare_amount   > 0 & fare_amount   <= 250)

set.seed(42)
df_plot <- df_plot[sample.int(nrow(df_plot), min(10000, nrow(df_plot))), ]

ggplot(df_plot, aes(trip_distance, fare_amount)) +
  geom_point(alpha = 0.25, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fare vs Trip distance",
       x = "Trip distance (miles)", y = "Fare amount (USD)")

# Very good resource for all kinds of plots:
# The R graph gallery
# https://r-graph-gallery.com/


################################################################################

# If you wanna practice at home, try to reproduce the plots
# (boxplot, histograms, etc.) using ggplot2. Play around with different variables,
# different colour and size options, create scatterplots between different variables.

















  


