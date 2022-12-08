#inspecting start of dataset
head(pressure)

#1)inspecting the data
#a)correlation between the variables
cor(pressure$temperature, pressure$pressure)

#b)scatter plot of the data that shows both the points and a smoothed line of the points 
scatter.smooth(x=pressure$temperature, y=pressure$pressure, main="Temperature ~ Pressure")

#c)side-by-side box-plots of each variable, including outliers, and printing the observation number of any outliers in the console
par(mfrow=c(1, 2))  #divide graph area in 2 columns
boxplot(pressure$temperature, main="Temp") #box plot for 'Temperature'
boxplot.stats(pressure$temperature)$out #display outliers
boxplot(pressure$pressure, main="Pressure")  #box plot for 'Pressure'
boxplot.stats(pressure$pressure)$out #display outliers

#d)side-by-side graphs of the densities of the variables
#density of the variables. 
plot(density(pressure$temperature), main="Density Plot: Temperature")  #density plot for 'Temperature'
plot(density(pressure$pressure), main="Density Plot: Pressure") #density plot for 'Distance'

#2)fit a model and diagnostics
#a)linear model that predicts temperature from pressure. 
pressure.lm <- lm(temperature ~ pressure, data=pressure)

#b)regression line on a scatterplot of the data
plot(pressure$temperature, pressure$pressure)
abline(pressure.lm)

#c)plot of residuals density
pressure.res <- resid(pressure.lm) #compute the residuals
plot(pressure$pressure, pressure.res, ylab="Residuals", xlab="Pressure", main="Pressure Linear Model") #residuals against the observed values
abline(0, 0) #the horizon
plot(density(pressure.res), main="Density Plot: residuals") #residuals density

#d)plot function to generate the 4 graphs of the residuals vs fitted values,
plot(pressure.lm) # each plot individually
par(mfrow=c(2,2)) # 2x2 grid of plots

#3) describing results 
#a) equation that describes the linear model you have fitted
print(pressure.lm) #coefficent for equation
#y = 132.7907 + 0.3797x

#b) why the p-values for the variable pressure and the overall F test are so similar for this model.
summary(pressure.lm) #summary of results
#The F statistic is based on the number of variables and there are two variables therefore they are very similar. The F statistic also determines the p value hence they would be similar.

#c) appropriatness of linear model 
#i believe it is appropriate as the scatterplot of residuals should showed random scatter . If i had seen a curved relationship in the residual plot, i would say the linear model is not appropriate.