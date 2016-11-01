Please submit your R codes and output, and additional answers to the questions.
For all figures, please properly label x and y axes and provide a title to the figure.
If there are multiple plotting symbols or different colors in a figure, provide a legend to annotate the figure.
For all hypothesis testing, state null and alternative hypotheses and a 1-2 sentence conclusions based on test results. 

1.	We will use FHS teaching dataset to examine how risk factors predict 
cardiovascular disease. The outcome variable is CVD. 
Please only use period 1 data in the analysis.

The CVD risk factors under considerations are 
AGE, SEX, SYSBP, DIABP, CURSMOKE, BMI, DIABETES. 
Please fit a logistic regression model and examine the goodness of fit with appropriate 
statistics as well as hypothesis testing. Make any needed improvement. For your final model, 
please report the odds ratio (OR) and 95% CI for all covariates. 
For SYSBP, report the OR per 10 units increase in the levels. 
Please evaluate the importance of DIABETES on the predictive power of the final model. 

fram <- read.csv("~/Dropbox/2016_Fall/BS845/fram.csv")
attach(fram)
CVD_fram<-subset(fram
                 , fram$PERIOD==1
                 , select = c(AGE,SEX,SYSBP,DIABP,CURSMOKE,BMI,DIABETES))




