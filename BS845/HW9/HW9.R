







library("faraway", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
advisits<-dvisits
View(advisits)
summary(dvisits)
#creating testing data set.
advisits<-dvisits[,1:13]
advisits<-advisits[,-3]
summary(advisits)

#getting an idea of the shape of the doctorco variable
hist(advisits$doctorco)
#it looks like doctorco 0 is highly overrepresented
#according to help doc, doctorco represents number of conusltations with a dr. in the past 2 weeks.

#Count data can be logistic, poisson, or negative binomial
#I'M DOING A POISSON!!!!!!

#young and old patients are overrepresented in the data.
#52-does this mean I need an interaction plot?  If I can make it so age alone is not a factor variable, this solves that problem.
#55-when there's an interaction term in the model, you can't drop any main effects (slide 55)

table(advisits$doctorco)
   0    1    2    3    4    5    6    7    8    9 
4141  782  174   30   24    9   12   12    5    1 

#Ok, using glm to make a first run poisson model
attach(advisits);
modp1<-glm(doctorco~.,family=poisson,advisits);
summary(modp1);

Call:
  glm(formula = doctorco ~ ., family = poisson, data = advisits)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.9502  -0.6858  -0.5747  -0.4852   5.7055  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
  (Intercept) -2.097821   0.101554 -20.657  < 2e-16 ***
  sex          0.156490   0.056139   2.788  0.00531 ** 
  age          0.279123   0.165981   1.682  0.09264 .  
  income      -0.187416   0.085478  -2.193  0.02834 *  
  levyplus     0.126498   0.071552   1.768  0.07707 .  
  freepoor    -0.438462   0.179799  -2.439  0.01474 *  
  freerepa     0.083640   0.092070   0.908  0.36365    
  illness      0.186156   0.018263  10.193  < 2e-16 ***
  actdays      0.126690   0.005031  25.184  < 2e-16 ***
  hscore       0.030683   0.010074   3.046  0.00232 ** 
  chcond1      0.117300   0.066545   1.763  0.07795 .  
  chcond2      0.150717   0.082260   1.832  0.06692 .  
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

(Dispersion parameter for poisson family taken to be 1)

Null deviance: 5634.8  on 5189  degrees of freedom
Residual deviance: 4380.1  on 5178  degrees of freedom
AIC: 6735.7

Number of Fisher Scoring iterations: 6

#Deviance is pretty good at 4380.1/5178 = 0.8459
#illness and actdays look they'll be the biggest predictors
#Let's run an ANOVA
anova(modp1)

Analysis of Deviance Table

Model: poisson, link: log

Response: doctorco

Terms added sequentially (first to last)


Df Deviance Resid. Df Resid. Dev
NULL                      5189     5634.8
sex       1    68.64      5188     5566.2
age       1   118.90      5187     5447.3
income    1    12.42      5186     5434.9
levyplus  1     0.79      5185     5434.1
freepoor  1     8.96      5184     5425.1
freerepa  1     7.36      5183     5417.7
illness   1   349.07      5182     5068.7
actdays   1   674.36      5181     4394.3
hscore    1    10.10      5180     4384.2
chcond1   1     0.74      5179     4383.5
chcond2   1     3.34      5178     4380.1

#Analysis of deviance indicates that all predictive covariates improve the model over an intercept alone.
#running a drop1() function, considered using the F-test due to dispersion in age, but Poisson takes this into account.

drop1(modp1,test="Chisq")

Single term deletions

Model:
  doctorco ~ sex + age + income + levyplus + freepoor + freerepa + 
  illness + actdays + hscore + chcond1 + chcond2
           Df Deviance    AIC    LRT  Pr(>Chi)    
  <none>        4380.1 6735.7                     
  sex       1   4388.0 6741.5   7.85  0.005091 ** 
  age       1   4383.0 6736.5   2.83  0.092667 .  
  income    1   4385.0 6738.6   4.87  0.027327 *  
  levyplus  1   4383.3 6736.9   3.15  0.075885 .  
  freepoor  1   4386.8 6740.4   6.66  0.009883 ** 
  freerepa  1   4381.0 6734.5   0.83  0.362874    
  illness   1   4481.9 6835.4 101.73 < 2.2e-16 ***
  actdays   1   4917.1 7270.7 536.97 < 2.2e-16 ***
  hscore    1   4389.1 6742.7   9.02  0.002677 ** 
  chcond1   1   4383.2 6736.8   3.12  0.077534 .  
  chcond2   1   4383.5 6737.0   3.34  0.067685 .  
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
  
#test indicates that dropping freerepa will improve the fit through lower AIC.  Let's see.
  modp2<-glm(doctorco ~ sex+age+income+levyplus+freepoor+illness+actdays+hscore+chcond1+chcond2,family=poisson,advisits);
  summary(modp2);
  
  Call:
    glm(formula = doctorco ~ sex + age + income + levyplus + freepoor + 
          illness + actdays + hscore + chcond1 + chcond2, family = poisson, 
        data = advisits)
  
  Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
  -3.0004  -0.6851  -0.5761  -0.4858   5.7284  
  
  Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
    (Intercept) -2.089063   0.100811 -20.723  < 2e-16 ***
    sex          0.162000   0.055824   2.902  0.00371 ** 
    age          0.355131   0.143196   2.480  0.01314 *  
    income      -0.199806   0.084328  -2.369  0.01782 *  
    levyplus     0.083689   0.053544   1.563  0.11805    
    freepoor    -0.469596   0.176360  -2.663  0.00775 ** 
    illness      0.186101   0.018260  10.191  < 2e-16 ***
    actdays      0.126611   0.005029  25.177  < 2e-16 ***
    hscore       0.031116   0.010065   3.092  0.00199 ** 
    chcond1      0.121100   0.066389   1.824  0.06814 .  
    chcond2      0.158894   0.081762   1.943  0.05197 .  
  ---
    Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
  
  (Dispersion parameter for poisson family taken to be 1)
  
  Null deviance: 5634.8  on 5189  degrees of freedom
  Residual deviance: 4381.0  on 5179  degrees of freedom
  AIC: 6734.5
  
  Number of Fisher Scoring iterations: 6  
  
#Dispersion remains about the same at 0.8459, AIC is lower (probably mostly due to removal of covariate)
  
drop1(modp2,test="Chisq")

Single term deletions

Model:
  doctorco ~ sex + age + income + levyplus + freepoor + illness + 
  actdays + hscore + chcond1 + chcond2
           Df Deviance    AIC    LRT  Pr(>Chi)    
  <none>        4381.0 6734.5                     
  sex       1   4389.5 6741.0   8.51  0.003537 ** 
  age       1   4387.1 6738.7   6.18  0.012912 *  
  income    1   4386.7 6738.2   5.70  0.016962 *  
  levyplus  1   4383.4 6735.0   2.43  0.118706    
  freepoor  1   4389.1 6740.6   8.10  0.004428 ** 
  illness   1   4482.7 6834.2 101.70 < 2.2e-16 ***
  actdays   1   4917.6 7269.2 536.63 < 2.2e-16 ***
  hscore    1   4390.2 6741.8   9.28  0.002316 ** 
  chcond1   1   4384.3 6735.9   3.34  0.067722 .  
  chcond2   1   4384.7 6736.3   3.75  0.052681 .  
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

#removal of any covariate does not reduce AIC or Deviance.  Even levyplus contributes.
  
#Ok, Poisson model is complete I think.  
plot(predict(modp2),residuals(modp2),xlab="Fitted",ylab="Residuals")
halfnorm(residuals(modp2))

phi<-sum(resid(modp2,type="pearson")^2)/df.residual(modp2)
phi 
[1] 1.325511
#Indicates there is no major overdispersion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Now for another model - Negative Binomial
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
modnb1<-glm.nb(doctorco~.,advisits);
summary(modnb1);

Call:
  glm.nb(formula = doctorco ~ ., data = advisits, init.theta = 0.9301535861, 
         link = log)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.9630  -0.6355  -0.5279  -0.4411   4.0045  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
  (Intercept) -2.276138   0.122749 -18.543  < 2e-16 ***
  sex          0.216333   0.069681   3.105  0.00191 ** 
  age          0.331326   0.207755   1.595  0.11076    
  income      -0.156214   0.103907  -1.503  0.13273    
  levyplus     0.116379   0.085666   1.359  0.17430    
  freepoor    -0.497256   0.210696  -2.360  0.01827 *  
  freerepa     0.145683   0.115851   1.258  0.20857    
  illness      0.214937   0.023521   9.138  < 2e-16 ***
  actdays      0.143729   0.007305  19.674  < 2e-16 ***
  hscore       0.037535   0.013609   2.758  0.00581 ** 
  chcond1      0.097905   0.079153   1.237  0.21612    
  chcond2      0.183473   0.103176   1.778  0.07536 .  
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

(Dispersion parameter for Negative Binomial(0.9302) family taken to be 1)

Null deviance: 3930.4  on 5189  degrees of freedom
Residual deviance: 3029.8  on 5178  degrees of freedom
AIC: 6423.7

Number of Fisher Scoring iterations: 1


Theta:  0.9302 
Std. Err.:  0.0867 

2 x log-likelihood:  -6397.6760 
  
pchisq(3029.8,5178,lower.tail=F)
[1] 1

#Checking goodness of fit of negative binomial
phiNB<-sum(resid(modnb1,type="pearson")^2)/df.residual(modnb1)
phiNB
[1] 1.000723




~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Negative Binomial model has much lower dispersion than Poisson.  This is the model with better goodness-of-fit.
#73-Check and model non-linearity for continuous covariates.

library("gam", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("mgcv", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

gam1<-gam(doctorco~sex+age+income+levyplus+freepoor+freerepa+illness+actdays+hscore+chcond1+chcond2,data=advisits)
summary(gam1)

Family: gaussian 
Link function: identity 

Formula:
  doctorco ~ sex + age + income + levyplus + freepoor + freerepa + 
  illness + actdays + hscore + chcond1 + chcond2

Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.036123   0.035803   1.009  0.31305    
sex          0.033806   0.021602   1.565  0.11766    
age          0.148433   0.066802   2.222  0.02633 *  
  income      -0.055814   0.031154  -1.792  0.07326 .  
levyplus     0.035238   0.024876   1.417  0.15668    
freepoor    -0.103180   0.052457  -1.967  0.04924 *  
  freerepa     0.032900   0.038071   0.864  0.38752    
illness      0.059872   0.008339   7.180 7.96e-13 ***
  actdays      0.103181   0.003656  28.221  < 2e-16 ***
  hscore       0.017018   0.005180   3.285  0.00103 ** 
  chcond1      0.004522   0.023716   0.191  0.84880    
chcond2      0.042277   0.035528   1.190  0.23411    
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1


R-sq.(adj) =    0.2   Deviance explained = 20.2%
GCV = 0.51071  Scale est. = 0.50953   n = 5190

gam2<-gam(doctorco~sex+age+income+levyplus+freepoor+freerepa+illness+actdays+hscore+chcond1+chcond2,data=advisits,family = nb())
summary(gam2)

Family: Negative Binomial(0.913) 
Link function: log 

Formula:
  doctorco ~ sex + age + income + levyplus + freepoor + freerepa + 
  illness + actdays + hscore + chcond1 + chcond2

Parametric coefficients:
              Estimate Std. Error z value Pr(>|z|)    
  (Intercept) -2.27801    0.12305 -18.513  < 2e-16 ***
  sex          0.21671    0.06987   3.102  0.00192 ** 
  age          0.33170    0.20833   1.592  0.11135    
  income      -0.15612    0.10417  -1.499  0.13396    
  levyplus     0.11664    0.08587   1.358  0.17438    
  freepoor    -0.49793    0.21116  -2.358  0.01837 *  
  freerepa     0.14662    0.11618   1.262  0.20694    
  illness      0.21524    0.02360   9.122  < 2e-16 ***
  actdays      0.14395    0.00734  19.610  < 2e-16 ***
  hscore       0.03759    0.01366   2.752  0.00592 ** 
  chcond1      0.09788    0.07934   1.234  0.21732    
  chcond2      0.18388    0.10348   1.777  0.07556 .  
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1


R-sq.(adj) =  0.0509   Deviance explained =   23%
-REML = 3222.6  Scale est. = 1         n = 5190


gam3<-gam(doctorco~sex+age+income+levyplus+freepoor+freerepa+illness+actdays+hscore+chcond1+chcond2,data=advisits,family = negbin(0.9302))
summary(gam3)

Family: Negative Binomial(0.93) 
Link function: log 

Formula:
  doctorco ~ sex + age + income + levyplus + freepoor + freerepa + 
  illness + actdays + hscore + chcond1 + chcond2

Parametric coefficients:
          Estimate Std. Error z value Pr(>|z|)    
  (Intercept) -2.276125   0.122748 -18.543  < 2e-16 ***
  sex          0.216331   0.069681   3.105  0.00191 ** 
  age          0.331322   0.207754   1.595  0.11076    
  income      -0.156215   0.103906  -1.503  0.13273    
  levyplus     0.116378   0.085665   1.359  0.17430    
  freepoor    -0.497252   0.210694  -2.360  0.01827 *  
  freerepa     0.145677   0.115850   1.257  0.20859    
  illness      0.214935   0.023521   9.138  < 2e-16 ***
  actdays      0.143727   0.007305  19.674  < 2e-16 ***
  hscore       0.037535   0.013609   2.758  0.00581 ** 
  chcond1      0.097907   0.079153   1.237  0.21611    
  chcond2      0.183469   0.103176   1.778  0.07537 .  
---
  Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1


R-sq.(adj) =  0.0529   Deviance explained = 22.9%
UBRE = -0.41159  Scale est. = 1         n = 5190




anova(modnb1,gam3)














lm1<-lm(doctorco~.,data=advisits)


anova(lm1,gam1)






