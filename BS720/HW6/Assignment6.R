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



prop.test()









