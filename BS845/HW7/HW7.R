Boston is an R dataset in MASS package recording housing values in suburbs of Boston as well as characteristics such as 
race, 
land use, 
education, 
and location. 

> library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
> Boston
> attach(Boston)
> str(Boston)
'data.frame':	506 obs. of  14 variables:
> summary(Boston)


Boston$chas <- factor(Boston$chas);
Boston$rad <- factor(Boston$rad)
str(Boston)

1.	The goal is to identify characteristics that are associated with median housing value (medv). 
Please perform necessary exploratory data analyses and then linear regression with variable selection. 
For the final model from selection, please examine model assumptions, 
report the delta R-squared of each covariate, and comment on the relative importance of these covariates. 

#tried to make a matrix of scatterplots, but this doesn't work since some variables are integers.
#chas is a dichotomous flag indicating if tract borders Charles River
#rad is categorical and indicates accessibility to radial highways
plot(Boston,panel=panel.smooth)
#too many at once, breaking up into two groups
plot(Boston[,c(1:7,14)],panel=panel.smooth)
plot(Boston[,c(8:14)],panel=panel.smooth)

#Tried all 3 stepwise. Backward and both gave exact same results, so used "both".
stepAIC(lm(medv ~ ., data=Boston),direction="forward");
stepAIC(lm(medv ~ ., data=Boston),direction="backward");
stepAIC(lm(medv ~ ., data=Boston),direction="both")

model1<-stepAIC(lm(medv ~ ., data=Boston),direction="both")
summary(model1)
getDeltaRsquare(model1)



2.	With the final model from 1, replace tax by a new variable that are the intervals defined using its quintiles. 
Perform effect estimation of all the pairwise comparisons between the quintiles intervals and hypothesis testing 
of each  effect equal zero. Perform a global test of tax quintiles.

3.	Removing two least significant covariates from final model from 1. 
Please compare the goodness of fit of new model with the final model from 1. 
Comment on impact of removing the two covariates on the goodness of fit.
