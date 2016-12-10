#Needed to import csv in format ASCII
Chronic_Conditions_PUF_2010 <- read.csv("~/Dropbox/2016_Fall/BS845/Project/2010_ChronicConditions_PUF/Chronic_Conditions_PUF_2010.csv", encoding="ASCII")
attach(Chronic_Conditions_PUF_2010)
#Creating a subset of the original Medicare data to only show demographics, comorbidities, and full year counts.
WholeYearCC_2010<-Chronic_Conditions_PUF_2010[,c(1:15,24:30,39:45,48,53:55)]
detach(Chronic_Conditions_PUF_2010);
attach(WholeYearCC_2010)
str(WholeYearCC_2010)
summary(WholeYearCC_2010)
#BENE_COUNT_PD_EQ_12, AVE_PDE_CST_PD_EQ_12, AVE_PDE_PD_EQ_12 all have 4537 NA's.
#What are these rows like?  Can they be omitted?
PDNA<-subset(WholeYearCC_2010,is.na(AVE_PDE_PD_EQ_12))
summary(PDNA)

#NA rows are for people who don't have Part D at all it appears (or Part C as it happens)
#This hits both genders, and all age categories
#Need to remove NA from WholeYearCC_2010
WholeYearCC_2010_WD<-subset(WholeYearCC_2010,AVE_PDE_PD_EQ_12>0)
summary(WholeYearCC_2010_WD)

#Several comorbidities have 767 NA's out of 17466.  Checking to see if these are all suppressed in the same rows
ComorbidNA<-subset(WholeYearCC_2010_WD,is.na(CC_ALZHDMTA))
summary(ComorbidNA)

#Yep - all 767 NA/suppressed CC_ALZHDMTA are also NA for CC_COPD, CC_DEPRESSN, CC_OSTEOPRS, CC_STRKETIA
#They're also NA for BENE_COUNT_PC_EQ_12, but I think this is unrelated.
#Removing these rows.  Conveniently, this gets rid of all NA comorbidity values.
WholeYearCC_2010_WD2<-subset(WholeYearCC_2010_WD,!is.na(CC_ALZHDMTA))
summary(WholeYearCC_2010_WD2)

#Think data's clean now!  That was surprisingly painless (I hope).
#Giving working data set an easier name and cleaning up a little.
PDP_2010<-WholeYearCC_2010_WD2
summary(PDP_2010)
rm(ComorbidNA,PDNA,WholeYearCC_2010_WD2,WholeYearCC_2010_WD)
detach()

hist(PDP_2010$AVE_PDE_PD_EQ_12, main="Average Prescriptions Filled per Beneficiary, 2010\nFull Year Part D Only", xlab="Average Prescriptions by Category")


#let's run a quick and dirty poisson to see what happens:
str(PDP_2010)
attach(PDP_2010)
modp1<-glm(AVE_PDE_PD_EQ_12~., family=poisson, PDP_2010)
#Hm.  glm isn't working with poisson because AVE_PDE_PD_EQ_12 is an average, not an integer.
