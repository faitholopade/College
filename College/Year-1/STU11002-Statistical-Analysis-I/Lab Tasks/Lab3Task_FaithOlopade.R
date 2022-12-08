#population of 4,000 with a variable from a Normal(4,5) distribution 
n4 <- rnorm(4000, 4, 5)
#population of 4,000 with a variable from an Exponential distribution with lambda = rate = 2.1
e4 <- rexp(4000, 2.1)

#sample of size 50 from n4
norm50 <- sample(n4, 50)
#sample of size 100 from n4
norma100 <- sample(n4, 100)
#sample of size 50 from e4
expo50 <- sample(e4, 50)
#sample of size 100 from e4
expo100 <- sample(e4, 100)

#z-score for 95% CI
z_score <- qnorm(0.975)

#critical t-value for 95% CI and norm4
t_scoreNorm <- qt(0.975, length(norm50)-1)
#critical t-value for 95% CI and expo4
t_scoreExp <- qt(0.975, length(expo50)-1)

#standard error of norm50 using known SD
se_kn_norm <- sd(n4)/sqrt(length(norm50))
#standard error of expo50 using known SD
se_kn_exp <- sd(e4)/sqrt(length(expo50))
#standard error of norm50 using sample SD
se_unkn_norm <- sd(norm50)/sqrt(length(norm50))
#standard error of expo50 using sample SD
se_unkn_exp <- sd(expo50)/sqrt(length(expo50))

#95% Confidence interval using zscore and known population SD
#a)
#lower bound of confidence interval for normal distribution using zscore and known population SD
left_z95_kn_norm <- mean(norm50)-z_score*se_kn_norm
#upper bound of confidence interval for normal distribution using zscore and known population SD
right_z95_kn_norm <- mean(norm50)+z_score*se_kn_norm 
#lower bound of confidence interval for exponential distribution using zscore and known population SD
left_z95_kn_exp <- mean(expo50)-z_score*se_kn_exp
#upper bound of confidence interval for exponential distribution using zscore and known population SD
right_z95_kn_exp <- mean(expo50)+z_score*se_kn_exp  

#95% confidence interval using t-distribution and the sample SD
#b)
#lower bound of confidence interval for normal distribution using tscore and sample SD
left_t95_unkn_norm <- mean(norm50)-t_scoreNorm*se_unkn_norm
#upper bound of confidence interval for normal distribution using tscore and sample SD
right_t95_unkn_norm <- mean(norm50)+t_scoreNorm*se_unkn_norm
#lower bound of confidence interval for exponential distribution using tscore and sample SD
left_t95_unkn_exp <- mean(expo50)-t_scoreExp*se_unkn_exp
#upper bound of confidence interval for exponential distribution using tscore and sample SD
right_t95_unkn_exp <- mean(expo50)+t_scoreExp*se_unkn_exp

#qqplot and a histogram to assess normality of n4 population
#normal qq plot
qqnorm(n4)
#line that passes through the quantiles
qqline(n4)
#histogram of the distribution
hist(n4, freq = FALSE)
#x value for line
xfit <- seq(min(n4), max(n4), length = 40) 
#y value for line
yfit <- dnorm(xfit, mean = mean(n4), sd = sd(n4))
lines(xfit, yfit)

#qqplot and a histogram to assess normality of e4 population
#normal qq plot
qqnorm(e4)
#line that passes through the quantiles
qqline(e4)
#histogram of the distribution
hist(e4, freq = FALSE)
#x value for line
xfit <- seq(min(e4), max(e4), length = 40) 
#y value for line
yfit <- dnorm(xfit, mean = mean(e4), sd = sd(e4))
#draw line
lines(xfit, yfit)

#qqplot and a histogram to assess normality of norm50 sample
#normal qq plot
qqnorm(norm50)
#line that passes through the quantiles
qqline(norm50)
#histogram of the distribution
hist(norm50, freq = FALSE)
#x value for line
xfit <- seq(min(norm50), max(norm50), length = 40) 
#y value for line
yfit <- dnorm(xfit, mean = mean(norm50), sd = sd(norm50))
#draw line
lines(xfit, yfit)

#qqplot and a histogram to assess normality of expo50 sample
#normal qq plot
qqnorm(expo50)
#line that passes through the quantiles
qqline(expo50)
#histogram of the distribution
hist(expo50, freq = FALSE)
#x value for line
xfit <- seq(min(expo50), max(expo50), length = 40) 
#y value for line
yfit <- dnorm(xfit, mean = mean(expo50), sd = sd(expo50))
#draw line
lines(xfit, yfit)


#I would not rely on the confidence intervals calculated in part 2 because 50 is
#a small sample of 4000 and with confidence intervals the likelihood the value 
#is accurate depends on the sample size.Larger confidence intervals increase the
#likelihood of catching the genuine percentage from the sample proportion, giving
#you more confidence that you know what it is
