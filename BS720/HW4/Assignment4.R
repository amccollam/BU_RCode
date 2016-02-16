#Homework 4
#Due February 22, 2016

library(MASS)
attach(Boston)

# 1.	Conduct the appropriate test (depending on whether or 
# not the assumptions are met) to see if the average nitrogen 
# oxide is equal to 0.5.
#nox = nitrogen oxide concentration (parts per 10 million)

#First, I'm going to take a look at the data
summary(nox)

#Summary statistics did not tell me much.
#Now generating residual plots.
par(mfrow=c(2,2))
plot(nox)
hist(nox, main= "NO2 Concentration")
qqnorm(nox, main = "QQ-plot for NO2 Concentration")
qqline(nox)
boxplot(nox, main="NO2 Concentration (parts per 10 million)")
dev.off()

#Performing a Shapiro test of normality to confirm that data do not
#form a normal distribution:
shapiro.test(nox)

#t-test assumption of normality is not met. Using Wilcoxon signed rank 
# test instead to test if the average nitrogen oxide is equal to 0.5

wilcox.test(nox, mu=0.5)
