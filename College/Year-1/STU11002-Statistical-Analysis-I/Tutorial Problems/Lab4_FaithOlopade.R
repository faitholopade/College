# we can extract some of these quantities from a vector like so:
Sample <-rnorm(100,0,1) #simulate some data
n <- length(Sample) #size of the sample
x_bar <- mean(Sample) #mean of the sample
sd <- sd(Sample) #this is the sample SD, in this case we know the population SD is 1 since we simulated our data
# lets define mu (the mean of the population under the null hypothesis
mu <- 0
#calculate the test statistic for a Z-distribution
test_stat <- (x_bar-mu)/(sd/sqrt(n))
#get the p-value for this test statistic
p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) # for two tailed
p_val <-  pnorm(test_stat , lower.tail=TRUE) # for left-tailed
p_val <-  pnorm(test_stat , lower.tail=FALSE) # for right tailed
Propdat <- rbinom(100, 1, 0.3) # simulate some data
n <- length(Propdat) #size of the sample
prop <- sum( ( Propdat == 1 ) / n ) #proportion of ones in the sample
prop <- mean(Propdat)  #could also do it this way since as results are either 0 or 1, the mean will be the proportion of values that are 1
#calculate sd for a z-test of proportions
sd = sqrt((prop)*(1-prop)/n)
df <- n-1 #define df
p_val <- 2*pt(abs(test_stat), df, lower.tail=FALSE) #two-tailed
p_val <- pt(test_stat , df, lower.tail=TRUE) #left-tailed
p_val <- pt(test_stat , df, lower.tail=FALSE) # right-tailed

#We could also use the t.test function in the package {stats}
t.test(Sample) #two-tailed
t.test(Sample, alternative = "less") #left-tailed
t.test(Sample, alternative = "greater") #right-tailed

dat <- iris
dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")
#Next, we can create a contingency table from size and species
Tbl <- table(dat$size, dat$Species)
Tbl #look at the table

#Finally, we can conduct a chi square test.
chisq.test(Tbl) 
##code for a one-sample hypothesis test function
OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), 
              input=paste("alpha=", alpha, "; mu=", mu, "; n=", n, "; x-bar=", x_bar, "; sd=", sd),    
              calculations=paste("se=", se, "; test statistic", test_stat),  
              conclusion=paste("At a significance level of", alpha, "the p-value of", p_val, "is", sig))
  #return the list
  return( ret )
}


