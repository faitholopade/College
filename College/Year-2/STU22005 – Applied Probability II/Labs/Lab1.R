# Store the data values in a vector x
x <- c(4.5, 6.5, 6.4, 8.9, 4.1, 6.4, 6.3, 9.1, 12.1, 1.4, 1.4, 4.6, 1.6, 9.8, 7.2, 6.5,
       4.1, 6.5, 11.6, 2.9)

# View vector x in output window
x

#SUMMARY STATISTICS

# Find the average
mean(x)
# Find the standard deviation
sd(x)
# Find the number of observations
length(x)
# Find various summary statistics
summary(x)
# Generate a histogram of x
hist(x, main="Histogram Of X", xlab="x", ylab="Frequency")

#SIMULATING DATA

# Set a seed for reproducibility, seed makes sure you get the same numbers each time 
set.seed(6124)
# Store the value of a scalar in n
n <- 100
# Generate a vector of independent standard normal variables.
x <- rnorm(n)
# Generate a histogram of x
hist(x)

#normal distribution documentation
?rnorm

# Read the data into R
data1 <- read.csv("Lab1a.csv")
data1

# mean
mean(data1$var1)

#CODE INTO PRACTICE 

#1. 
#section 2.3 pharmaceutical example data
y <- c(1.1, 1.8, 2, 2.4, 2.5, 2.8, 2.9, 3, 3.4, 3.4, 4)
#hist of y
hist(y)
#average
mean(y)
#standard deviation
sd(y)
#number of observations
length(y)
#summary statistics
summary(y)

#2. 
#set seed 
set.seed(6498)
#store value of scalar in n 
n <- 1000
#simulate 1000 values from standard normal distribution
y <- rnorm(n)
#histogram
hist(y)

#3. 
#set seed
set.seed(7611)
#store value of scalar in n 
n <- 200
#simulate 200 values from normal distribution with mean = 30 and sd = 9
p <- rnorm(n, 30, 9)
#histogram
hist(p)

#4.
#set seed
set.seed(7611)
#store value of scalar in n 
n <- 20000
#simiulate 20,000 values from normal distribution with mean = 30 and sd = 9
q <- rnorm(n, 30, 9)
#histogram 
hist(q)

#5. 
#read data in 
data2 <- read.csv("Lab1b.csv")
data2
#view first few lines of dataset
head(data2)

#mean
mean(data2$x1)
mean(data2$x2)
mean(data2$x3)

#standard deviation
sd(data2$x1)
sd(data2$x2)
sd(data2$x3)

#number of observations
length(data2$x1)
length(data2$x2)
length(data2$x3)

#summary statistics 
summary(data2$x1)
summary(data2$x2)
summary(data2$x3)

#histograms
hist(data2$x1)
hist(data2$x2)
hist(data2$x3)

?hist
