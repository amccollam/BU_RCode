







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
  


  