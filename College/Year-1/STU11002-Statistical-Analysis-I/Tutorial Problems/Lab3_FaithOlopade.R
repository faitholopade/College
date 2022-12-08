#create a vector x0 of length 1000, drawn from a normal distribution with mean 3, sd 5
x0 <- rnorm(1000,3,5)

# record the size, mean and sd of x0
N0 <- length(x0)
mean0 <- mean(x0)
sd0 <- sd(x0)

#Take a random sample of size 30 from x0
x1 <- sample(x0,30)

# record the size, mean and sd of x1
n1 <- length(x1)
mean1 <- mean(x1)
sd1 <- sd(x1)

#calculate the standard error of mean1 using known and estimated sd0
se_kn <- sd0/sqrt(n1)
se_unkn <- sd1/sqrt(n1)

#Using R to get Critical Values 
Z1 <- qnorm(0.975)
Z2 <- qnorm(0.995)
Z1
Z2

?qt
#critical value below which 97.5% of values from a t distribution with 10 degrees of freedom lie
qt(0.975, 10) 
t1 <- qt(0.975, 10)
t2 <- qt(0.995, 10)
t1
t2

#Calculate Z-score for 95% CI
Z_score <- qnorm(0.975)

#Calculate critical t-value for 95% CI and x1
t_score <- qt(0.975, n1-1)

#Calculate 95% CIs for x1 using known and unknown population sd, and t and z distributions
left_Z95_kn <- mean1-Z_score*se_kn
right_z95_kn <- mean1+Z_score*se_kn
left_Z95_unkn <- mean1-Z_score*se_unkn
right_z95_unkn <- mean1+Z_score*se_unkn

left_t95_kn <- mean1-t_score*se_kn
right_t95_kn <- mean1+t_score*se_kn
left_t95_unkn <- mean1-t_score*se_unkn
right_t95_unkn <- mean1+t_score*se_unkn

#show results
paste("var: x0", "mean:", mean0, "sd:", sd0)
paste("var: x1", "mean:", mean1, "sd:", sd1, "se w/known pop sd:", se_kn, "se w/unknown pop sd:", se_unkn)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn, right_z95_kn)
paste("Z-distribution w/unknown pop sd:", left_Z95_unkn, right_z95_unkn)
paste("t-distribution w/known pop sd:", left_t95_kn, right_t95_kn)
paste("t-distribution w/unknown pop sd:", left_t95_unkn, right_t95_unkn)


#make a qqplot and a histogram with normal density curve for x0
qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit <- seq(min(x0), max(x0), length = 40) 
yfit <- dnorm(xfit, mean = mean(x0), sd = sd(x0))
lines(xfit, yfit)








