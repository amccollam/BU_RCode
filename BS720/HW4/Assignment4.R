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

#just for funsies running the t-test despite violating assumptions. Still fail to reject.
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



# 3.	a) Create a new variable dis.cat that reflects the distance of the 
# house from the 5 Boston employment centers as follows:
#   “very close” if the weighted mean of distances (dis) 
#     is less than 2.5 miles away
#   “somewhat nearby” if the weighted mean of distances (dis) 
#     is greater than or equal to 2.5 miles away but less than 5 miles away
#   “far” if the weighted mean of distances (dis) 
#     is greater than or equal to 5 miles way

#dis = weighted mean fo distances to five Boston employment centers

dis.label <- c("Very Close", "Somewhat Nearby", "Far")
dis.break <- c(0,2.5,5,100)
dis.cat   <- cut(dis, breaks=dis.break, labels = dis.label)

table(dis.cat)

#running data a different way to verify output
dis.cat.check <- ifelse(dis<2.5,"Very Close",
                        ifelse(dis<5,"Somewhat Nearby",
                               "Far"))

# b) Conduct the appropriate test (depending on whether or not the 
# assumptions are met) to see if the value of owner-occupied homes 
# ($1000s) is the same for each category of distance (“dis.cat”).

#medv = median value of owner-occupied homes in $1000s

#checking to make sure data structure is Factor
str(dis.cat)

#to test normality
shapiro.test(medv[which(dis.cat=="Very Close")])
shapiro.test(medv[which(dis.cat=="Somewhat Nearby")])
shapiro.test(medv[which(dis.cat=="Far")])
#all reject H0

par(mfrow=c(1,3))
hist(medv[which(dis.cat=="Very Close")],main="Close",xlab="Median Value")
hist(medv[which(dis.cat=="Somewhat Nearby")],main="Nearby",xlab="Median Value")
hist(medv[which(dis.cat=="Far")],main="Far",xlab="Median Value")



#to check for equal variance across dis factors
bartlett.test(medv~dis.cat)

#Bartlett test indicates unequal variances.  Running boxplot to look.
dev.off()
boxplot(medv~dis.cat)


#Assumptions of ANOVA are not met, must use 
#nonparametric test, Kruskal-Wallis
kruskal.test(medv~dis.cat)

#If we had run ANOVA anyways
dis.anova <- aov(medv~dis.cat)
summary(dis.anova)



