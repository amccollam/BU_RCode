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
