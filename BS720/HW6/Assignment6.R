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







