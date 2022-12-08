#read in data set
weight <- read.csv(file = "Assignment.csv", header = TRUE)
#histogram of data
hist(weight$WEIGHT)
qqnorm(weight$WEIGHT)
qqline(weight$WEIGHT)
hist(weight$WEIGHT, freq = FALSE)
xfit <- seq(min(weight$WEIGHT), max(weight$WEIGHT), length = 40) 
yfit <- dnorm(xfit, mean = mean(weight$WEIGHT), sd = sd(weight$WEIGHT))
lines(xfit, yfit)
#size
N0 <- length(weight$WEIGHT)
#mean
mean0 <- mean(weight$WEIGHT) 
#estimated SD
sd0 <- sd(weight$WEIGHT)
#SE using estimated SD
se <- sd0/sqrt(N0) 
#95 % CV
Z1 <- qnorm(0.975)
#97.5 % CV
Z2 <- qnorm(0.9875)
#confidence intervals using sample SD (unknown population SD)
left_Z97.5 <- mean0-Z2*se
right_z97.5 <- mean0+Z2*se
#results
paste("var: weight", "mean:", mean0, "SD:", sd0, "SE:", se, "CV:", Z2)
paste("Z-distribution w/unknown pop SD:", left_Z97.5, right_z97.5)

#median
u <- median(weight$WEIGHT)
#test statistic for a Z-distribution
test_stat <- (mean0-u)/(sd0/sqrt(N0))
#p-value for this test statistic
p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) # for two tailed
#alpha
alpha <- 0.025
if(p_val < alpha){
  sig <- "significant"
} else {
  sig <- "not significant"
}
paste("At a significance level of", alpha, "the p-value of", p_val, "is", sig)
