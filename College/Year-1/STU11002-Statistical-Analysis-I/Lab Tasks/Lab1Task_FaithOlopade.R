#Read in the csv file Lab1.csv using code, not the Import button
Lab1<-read.csv(file="Lab1.csv", header=TRUE)
Lab1_false<-read.csv(file="Lab1.csv", header=FALSE)
#Display summary statistics for the variable EARN
summary(Lab1$EARN)
#Display frequencies of the variable Job.class
table(Lab1$Job.class)
#Display a three-way cross-tabulation of the proportions of variables Educ, Gender and Job.Class
ftable(Lab1$EDUC, Lab1$Gender, Lab1$Job.class)
#Create a basic histogram of the variable EARN
hist(Lab1$EARN) 
#Create a basic boxplot of the variable EARN by EDUC
boxplot(Lab1$EARN~Lab1$EDUC)
#Create a new variable AGE_500 that is equal to Age divided by 500
Lab1$AGE_500 = Lab1$AGE/500
#Create a scatterplot with AGE_500 on the x axis and AGE on the Y axis
plot(Lab1$AGE_500, Lab1$AGE)


