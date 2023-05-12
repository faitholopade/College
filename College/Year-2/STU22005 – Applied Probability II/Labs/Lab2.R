#1. SAMPLING DISTRIBUTIONS
#set the seed value
set.seed(8423)
#simulate values form the normal distribution
simdata <- rnorm(n = 10000, mean = 100, sd = 4)
#create a matrix with 1000 samples in each row of 10 columns 
matrixdata <- matrix(simdata, nrow = 1000, ncol = 10)
#matrix function documentation 
?matrix
#compute the mean of each sample(each row) and print the first six means
means <- apply(matrixdata, 1, mean)
means[1:6]
#apply function documentation 
?apply
#plot the first six samples
par(mfrow=c(3, 2), mar=c(4, 4, 4, 1), oma=c(0.5, 0.5, 0.5, 0)) #c() - c defines s vector of values 
for(i in c(1:6)){
  hist(matrixdata[i,], main=paste0("Sample ", i), xlab = "Values", #matrixdata[i,] - returns the whole row of data (the sample), 
       #paste0 - allows us to change title of plot without typing individually and changing i each time 
       cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2,
       ylim = c(0, 5), xlim = c(85, 115))
}
#par documentation
?par
#reset the number of plots that appear in the window at a time
par(mfrow=c(1, 1))
#construct histogram of the sample means from each of 1000 samples 
#generate histogram(keeping the y scale the same as the earlier graphs for comparison)
hist(means, main="Histogram of the sample means", xlab= "Sample mean", xlim = c(85, 115))

#2. SAMPLES THAT ARE EXPONENTIALLY DISTRIBUTED 
# Set the seed value
set.seed(96358285)
# Simulate values from the exponential distribution
simdata <- rexp(n = 10000, rate = 0.5)
# Create a matrix with 1000 samples in each row of 10 columns
matrixdata <- matrix(simdata, nrow = 1000, ncol = 10)
#Compute the mean of each sample (each row)
means <- apply(matrixdata, 1, mean)
means[1:6]
## [1] 1.684177 2.168568 1.604019 1.704819 2.696661 1.570264
# Plot the first six samples
par(mfrow = c(3,2), mar = c(4,4,4,1), oma = c(0.5,0.5,0.5,0))
for (i in c(1:6)){
  hist(matrixdata[i,], main = paste0("Sample ", i), xlab = "Values",
       cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2,
       ylim = c(0,8), xlim = c(0,10))
}

# Reset the number of plots that appear in the window at a time
par(mfrow = c(1,1))
# Generate histogram (keeping the y scale the same as the earlier graphs for comparison)
hist(means, main = "Histogram of the sample means", xlab = "Sample mean", xlim = c(0, 10))

#3. PUTTING CODE INTO PRACTICE 
# 1. 
#set the seed value
set.seed(6594)
#simulate values from the normal distribution 
simdata <- rnorm(n = 1000, mean = 50, sd = 3)
#create a matrix with 1000 samples in each row of 10 columns
matrixdata <- matrix(simdata, nrow = 1000, ncol = 5)
#compute the mean of each sample(each row) and stor them in vector
means <- apply(matrixdata, 1, mean)
