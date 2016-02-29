#The following homework uses the Outbreak dataset from the “epicalc” 
#library that we used in class. 

> out <- read.delim("~/Dropbox/_git/BU_RCode/BS720/HW6/out.txt")
>   View(out)

# 1. Using the Outbreak dataset from the “epicalc” library, test the 
# hypothesis that there is no association between the odds of developing 
# gastrointestinal illness and eating a salted egg. Report the odds ratio, 
# 95% CI for the odds ratio, chi square statistic, p-value and interpret 
# the results (make sure you do not include observations with missing 
# values).

#first, need to create a new column for GI illness:
case <- ifelse(nausea==1|vomiting==1|abdpain==1|diarrhea==1,1,0)
outbreak <- data.frame(out,case)

#Now removing rows with nullvalues for saltegg
saltegg.eat <- ifelse(saltegg==1,1,ifelse(saltegg==0,0,NA))
table(saltegg.eat,case)

#calculating odds ratio using epitools
library(epitools)
oddsratio(saltegg.eat,case)


# Test the null hypothesis that the proportion of individuals who ate a 
# salted egg is equal to 10%.
table(saltegg.eat)
prop.test(saltegg.eat==1,length(which(saltegg.eat==0 | saltegg.eat==1)),p=0.1)

table(saltegg.eat==1)
length(saltegg.eat)


# Test the null hypothesis the proportion of individuals who had 
# gastrointestinal illness among those who ate a salted egg is the same as 
# the proportion of individuals having gastrointestinal illness among those 
# who did not eat a salted egg.

#First, need to set up a 2x2 table of eggs and outcomes
yesegg.cases = length(which(case ==1 & saltegg ==1))
noegg.cases = length(which(case == 1 & saltegg==0))
egg.cases = c(yesegg.cases, noegg.cases)
yesegg.total = length(which(saltegg==1))
noegg.total = length(which(saltegg==0))
egg.total = c(yesegg.total, noegg.total)
#Then, run ChiSq test of independence
prop.test(egg.cases,egg.total)

# a) Create a new variable called age.cat which is defined as follows: 
#   1 if the age is less than 15, 
# 2 if the age is greater than or equal to 15 and less than 25, and 
# 3 if the age is greater than or equal to 25.


age_cat<-99
age_cat[age<15]<-1
age_cat[age>=15 & age<25]<-2
age_cat[age>=25]<-3

table(age_cat)


#Making a table displaying vomiting by age.cat

table(vomiting)
age.vomiting<-ftable(vomiting~age_cat)
prop.test(age.vomiting)

# d) Test the null hypothesis that there is a linear 
# increase/decrease (a trend) in the proportion of individuals who 
# vomited as you increase/decrease age category.

vomit.vector = tapply(vomiting, age_cat, sum)
total.vector = tapply(vomiting, age_cat, length)

prop.trend.test(vomit.vector,total.vector)

# 5.a.a) Create a variable called symptom.sum that is defined as follows:
# 0 if an individual only had none of the following symptoms: nausea, diarrhea, abdominal pain or vomiting
# 1 if an individual only had one of the following symptoms: nausea, diarrhea, abdominal pain or vomiting
# 2 if an individual had two of the following symptoms: nausea, diarrhea, abdominal pain or vomiting
# 3 if an individual had three of the following symptoms: nausea, diarrhea, abdominal pain or vomiting
# 4 if an individual had all four symptoms

















