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


 
# 2.	 Conduct the appropriate test (depending on whether or not the 
# assumptions are met) to see if the crime rate is the same for houses 
# in which the tract bounds the Charles River versus houses which do not.
#chas = Charles River dummy variable (1 if tract bounds river, 0 otherwise)
#crim = Per capita crime rate by town

#to check for normal distribution, first check a pair of histograms
par(mfrow=c(1,2))
hist(crim[which(chas=="1")],main="Bounds Charles",xlab="Crime Rate")
hist(crim[which(chas=="0")],main="Does Not Bound",xlab="Crime Rate")

#to check assumption of equal variance
var.test(crim~chas)

#Since assumptions not met, must use Wilcoxon nonparametric Rank Sum Test 
#to compare Medians
wilcox.test(crim~chas)
#Fail to reject H0 that means are the same.

#just for funsies running the t-test despite violating assumptions
t.test(crim~chas,var=T)

# Two Sample t-test
# 
# data:  crim by chas
# t = 1.2567, df = 504, p-value = 0.2094
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.066261  4.851815
# sample estimates:
#   mean in group 0 mean in group 1 
# 3.744447        1.851670 

