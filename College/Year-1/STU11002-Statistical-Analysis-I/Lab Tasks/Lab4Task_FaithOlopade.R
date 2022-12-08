#Part A)
#1)
#vector of size 100 drawn from a Normal(6,4) 
sample <-rnorm(100,6,4)
#vector of size 80 drawn from a Normal(7,2) distribution
sample2 <- rnorm(80, 7, 2)
#mean for each sample
xbar <- mean(sample)
xbar2 <- mean(sample2)
#mean of populations under null hypothesis
mu <- 6
mu2 <- 7
#standard deviation of samples
sd <- sd(sample)
sd2 <- sd(sample2)
#size of samples
n <- length(sample)
n2 <- length(sample2)
#H0: means are equal
h0 <- 0
#estimate of SD using pooled sD's
esd <- sqrt((((n - 1) * (sd * sd)) + ((n2 - 1) * (sd2 * sd2)))/(n + n2 - 2))
#standard Error
se <-esd * sqrt((1/n) + (1/n2))
#test statistic 
teststat <- (xbar- xbar2 - h0)/(se) 
#p value for H0 (z test)
p_valz <-  2*pnorm(abs(teststat), lower.tail=FALSE) 
#degrees of freedom 
df <- n + n2 - 2
#p value for H0 (t test)
p_valt <- pt(abs(teststat), df, lower.tail=FALSE)

#2)
# p value of t test using t.test 
t.test(sample, sample2, mu=h0, var.equal=TRUE)

#3)
#significance level
alpha <- 0.01
if(p_valt < alpha) {
  sig <- "significant"
} else {
  sig <- "not significant"
}
#The test is not significant and therefore we fail to reject the null hypothesis and conclude that the means are equal with a significance level of 0.01

#Part B)
#1) 
#load csv file
survey <- read.csv(file="survey.csv", header=TRUE)
#contingency table for variables smoke and exer
tbl <- table(survey$Smoke, survey$Exer)
tbl
#chi square test
chisq.test(tbl)
# results: X-squared = 5.4885, df = 6, p-value = 0.4828

#2)
#a. the code gave a warning that the approximation of the test may be incorrect because the expected values in the test were
#   quite small and so the p-value is likely to be inaccurate

#b. H0: There is no relationship between the data
#   HA: There is an relationship between the data

#c. With alpha = 0.05 since p < alpha we fail to reject the null hypothesis and conclude from the test that there is
#   no relationship between the two data sets. However due to the warning message given this result may not necessarily
#   be correct as the resulting p-value and test-statistic can be incorrect.