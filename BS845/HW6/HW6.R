attitude is from a survey of the clerical employees of a large financial organization, 
the data are aggregated from the questionnaires of the approximately 35 employees for 
each of 30 (randomly selected) departments. The numbers give the percent proportion of 
favourable responses to seven questions in each department. Please use help(attitude) 
to view the manual of the dataset.

The overall purpose is to fit a model to examine the association of rating with all the 
other variables in the dataset. Please carry out exploratory data analyses include 
normality check, correlation and t-tests to examine crude association. Fit a linear 
model with model diagnosis and evaluate goodness of fit the model. Improve the model 
based on diagnostic results. 



attitude;
str(attitude);
summary(attitude);

#comparing all variables to all other variables using scatterplots
plot(attitude,panel=panel.smooth);

#Evaluating rating as a response varaible
par(mfrow=c(1,2));
hist(attitude$rating);
qqnorm(attitude$rating);
qqline(attitude$rating)
#The response variable does not appear to be perfectly normally distributed

#generating more graphs about response variable
par(mfrow=c(2,2))
plot(attitude$rating)
hist(attitude$rating, main= "Rating")
qqnorm(attitude$rating, main = "QQ-plot for Rating")
qqline(attitude$rating)
boxplot(attitude$rating, main="Rating")

#checking normality of response formally using Shapiro test of normality
shapiro.test(attitude$rating)


#doing quick simple linear regressions to identify important predictor variables
attach(attitude)
LMComplaints<-lm(rating~complaints);
LMPrivileges<-lm(rating~privileges);
LMLearning<-lm(rating~learning);
LMRaises<-lm(rating~raises);
LMCritical<-lm(rating~critical);
LMAdvance<-lm(rating~advance);

plot(LMComplaints,main="Complaints");
plot(LMPrivileges,main="Privileges");
plot(LMLearning,main="Learning");
plot(LMRaises,main="Raises");
plot(LMCritical,main="Critical");
plot(LMAdvance,main="Advance");

summary(LMComplaints);
summary(LMPrivileges);
summary(LMLearning);
summary(LMRaises);
summary(LMCritical);
summary(LMAdvance);



#Forward stepwise regression
comp_learn<-lm(rating~complaints+learning);
comp_raises<-lm(rating~complaints+raises);
comp_priv<-lm(rating~complaints+privileges);




#testing to see which transformation Critical might benefit from:
par(mfrow=c(2,2))
plot(attitude$critical)
hist(attitude$critical, main= "Critical")
qqnorm(attitude$critical, main = "QQ-plot for Critical")
qqline(attitude$critical)
boxplot(attitude$critical, main="Critical")
#critical is right skew.  Not bad otherwise.

test<-glm(rating~complaints+learning+raises+privileges+critical+advance)

comp_crit<-lm(rating~complaints+critical)


install.packages("sandwich");library(sandwich)
coeftest(comp_learn, vcov= vcovHC(m1))
#didn't work with sandwich library
install.packages("moments")
library(moments)
install.packages("lmtest")
library(lmtest);
library(MASS)



