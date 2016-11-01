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
(e^B1 is the odds ratio comparing the increase/decrease in odds for those with a 
one-unit increase in B1 compared to the standard group)
(To adjust reference level, use relevel() or level())
For SYSBP, report the OR per 10 units increase in the levels. 
Please evaluate the importance of DIABETES on the predictive power of the final model. 

fram <- read.csv("~/Dropbox/2016_Fall/BS845/fram.csv")
attach(fram)
CVD_fram<-subset(fram
                 , fram$PERIOD==1
                 , select = c(CVD,AGE,SEX,SYSBP,DIABP,CURSMOKE,BMI,DIABETES))
detach(fram);
attach(CVD_fram);
#playing first to get the gist
summary(glm(CVD~CURSMOKE,family=binomial(link=logit)))
#to test the significance of the model
1-pchisq(5090.6-5085.5,4433-4432)

#running quick and dirty model with all variables
summary(glm(CVD~.,data=CVD_fram,family=binomial(link=logit)))

Call:
  glm(formula = CVD ~ ., family = binomial(link = logit), data = CVD_fram)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.0631  -0.7629  -0.5320   0.8118   2.5441  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -6.387366   0.411012 -15.541  < 2e-16 ***
  AGE          0.058123   0.004887  11.894  < 2e-16 ***
  SEX         -0.919749   0.078260 -11.752  < 2e-16 ***
  SYSBP        0.012979   0.002790   4.651 3.30e-06 ***
  DIABP        0.010816   0.005000   2.163 0.030542 *  
  CURSMOKE     0.379031   0.079549   4.765 1.89e-06 ***
  BMI          0.032847   0.009623   3.414 0.000641 ***
  DIABETES     1.176871   0.206259   5.706 1.16e-08 ***
  ---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 5066.6  on 4414  degrees of freedom
Residual deviance: 4427.4  on 4407  degrees of freedom
(19 observations deleted due to missingness)
AIC: 4443.4

Number of Fisher Scoring iterations: 4


CVD_allin<-glm(CVD~.,data=CVD_fram,family=binomial(link=logit))
summary(CVD_allin)
pchisq(5066.6-4427.4,df=4414-4407,lower.tail = F)
drop1(CVD_allin, test="F")
#Doesn't look like any variables should be dropped

Rsq(CVD_allin)
plot(Rsq(CVD_allin))
HLtest(Rsq(CVD_allin))

Hosmer and Lemeshow's' goodness-of-fit test for logistic regression models
with method = deciles
Observed counts:
1   2   3   4   5   6   7   8   9  10
429 401 376 381 349 337 285 270 242 194
12  41  65  61  93 104 157 171 200 247

Expected counts:
1     2     3   4     5   6     7     8     9  10
415.3 401.3 384.5 369 351.8 333 310.1 282.1 245.9 171
25.7  40.7  56.5  73  90.2 108 131.9 158.9 196.1 270

Pearson residuals:
1   2    3    4    5    6    7    8    9   10
0.7 0.0 -0.4  0.6 -0.2  0.2 -1.4 -0.7 -0.2  1.8
-2.7 0.1  1.1 -1.4  0.3 -0.4  2.2  1.0  0.3 -1.4

Chi-square statistic:  25.31851  with  8  df
P-value:  0.001372613

#p-value indicates rejected null hypothesis, i.e. the model fit is not good.  What gives?

X2GOFtest(Rsq(CVD_allin))
X2GOFtest(Rsq(CVD_allin, na.rm=T))
Rsq(CVD_allin, na.rm=T)

#k=log(df)=BIC test
stepAIC(CVD_allin, direction="both",k=log(4434))
stepAIC(CVD_allin, direction="backward")
anova(CVD_allin)



#Now let's try to get the ORs and CIs
CVD_summ<-summary(CVD_allin)
coefCI<-cbind(CVD_summ$coef
              ,CVD_summ$coef[,1]-CVD_summ$coef[,2]*1.96
              ,CVD_summ$coef[,1]+CVD_summ$coef[,2]*1.96)
coefCI
colnames(coefCI)[5:6]<-c("CI95LL","CI95UL")
coefCI

Estimate  Std. Error    z value     Pr(>|z|)       CI95LL      CI95UL
(Intercept) -6.38736559 0.411012052 -15.540580 1.843084e-54 -7.192949211 -5.58178197
AGE          0.05812301 0.004886924  11.893577 1.278135e-32  0.048544634  0.06770138
SEX         -0.91974858 0.078259831 -11.752499 6.856092e-32 -1.073137848 -0.76635931
SYSBP        0.01297855 0.002790196   4.651481 3.295593e-06  0.007509761  0.01844733
DIABP        0.01081550 0.005000262   2.162987 3.054217e-02  0.001014989  0.02061602
CURSMOKE     0.37903080 0.079549353   4.764725 1.891112e-06  0.223114072  0.53494754
BMI          0.03284720 0.009622715   3.413507 6.413262e-04  0.013986680  0.05170772
DIABETES     1.17687101 0.206258747   5.705799 1.157984e-08  0.772603867  1.58113815

exp(coefCI[2,c(1,5,6)]);
exp(coefCI[3,c(1,5,6)]);
exp(coefCI[4,c(1,5,6)]*10);
exp(coefCI[5,c(1,5,6)]);
exp(coefCI[6,c(1,5,6)]);
exp(coefCI[7,c(1,5,6)]);
exp(coefCI[8,c(1,5,6)]);

> exp(coefCI[2,c(1,5,6)]);
Estimate   CI95LL   CI95UL 
1.059845 1.049742 1.070046 
> exp(coefCI[3,c(1,5,6)]);
Estimate    CI95LL    CI95UL 
0.3986192 0.3419339 0.4647018 
> exp(coefCI[4,c(1,5,6)]*10);
Estimate   CI95LL   CI95UL 
1.138584 1.077989 1.202585 
> exp(coefCI[5,c(1,5,6)]);
Estimate   CI95LL   CI95UL 
1.010874 1.001016 1.020830 
> exp(coefCI[6,c(1,5,6)]);
Estimate   CI95LL   CI95UL 
1.460868 1.249963 1.707359 
> exp(coefCI[7,c(1,5,6)]);
Estimate   CI95LL   CI95UL 
1.033393 1.014085 1.053068 
> exp(coefCI[8,c(1,5,6)]);
Estimate   CI95LL   CI95UL 
3.244207 2.165397 4.860485 


