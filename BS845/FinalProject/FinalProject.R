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
#What are these rows like?  Can/should they be omitted?
PDNA<-subset(WholeYearCC_2010,is.na(AVE_PDE_PD_EQ_12))
summary(PDNA)

#These NA rows are for people who don't have Part D at all it appears (or Part C as it happens)
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

#Saving this file to my project folder to keep a version untouched.
str(PDP_2010)
attach(PDP_2010)
write.table(PDP_2010, file="PDP_2010.csv",quote=F, sep=",", na="", row.names=F)

hist(PDP_2010$AVE_PDE_PD_EQ_12, main="Average Prescriptions Filled per Beneficiary, 2010\nFull Year Part D Only", xlab="Average Prescriptions by Category")


#let's run a quick and dirty poisson to see what happens:

modp1<-glm(AVE_PDE_PD_EQ_12~., family=poisson, data=PDP_2010)
summary(modp1)
#Hm.  glm isn't working with poisson because AVE_PDE_PD_EQ_12 is an average, not an integer.

#Taking a moment to understand the number of full year beneficiaries
#To get the mean and sd for the poisson, I'll need to take a weighted average.
#First, creating a new column with total RX = full year PDP beneficiaries * avg Rx filled for year

PDP_2010_WD<-PDP_2010
PDP_2010_WD$TotRx<- (BENE_COUNT_PD_EQ_12 * AVE_PDE_PD_EQ_12)

#Now dividing by the total number of beneficiaries to get pop mean of 39.55605
PopAvgPDE<-sum(PDP_2010_WD$TotRx)/sum(PDP_2010_WD$BENE_COUNT_PD_EQ_12)


#Ok...  now what?
#Should I do something with the equation p(Y=k)=(e^-u*u^k)/k! where k=BENE_COUNT_PD_EQ_12?
#Should I check predictor varaibles for colinearity?
#Copying my 11/9 HW - got a 9.9/10 for the poisson I built there.  Output in plots.docx

anova(modp1)
drop1(modp1,test="Chisq")

#The output is telling me to stop using benefit payment amounts for Part A, Part B svcs, and I agree this is a good idea.
#All of these have Likelihood Ratio Test values of 0, which is I think what happens when R rounds from a really really low number (aka bad)
#Building a second model excluding these.
modp2<-glm(AVE_PDE_PD_EQ_12~BENE_SEX_IDENT_CD+BENE_AGE_CAT_CD
           +CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA
           +CC_2_OR_MORE+DUAL_STUS+BENE_COUNT_PA_EQ_12+AVE_IP_ADM_PA_EQ_12+AVE_SNF_DAYS_PA_EQ_12
           +BENE_COUNT_PB_EQ_12+AVE_CA_VST_PB_EQ_12+AVE_OP_VST_PB_EQ_12+BENE_COUNT_PC_EQ_12+BENE_COUNT_PD_EQ_12+AVE_PDE_CST_PD_EQ_12
           , family=poisson
           , data=PDP_2010);
summary(modp2)

drop1(modp2,test="Chisq")

#This is still a terrible model.  Can I do forward stepwise?
#She used stepAIC in class, part of MASS package.

stepAIC(glm(AVE_PDE_PD_EQ_12~AVE_PDE_PD_EQ_12~BENE_SEX_IDENT_CD+BENE_AGE_CAT_CD
            +CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA
            +CC_2_OR_MORE+DUAL_STUS+BENE_COUNT_PA_EQ_12+AVE_IP_ADM_PA_EQ_12+AVE_SNF_DAYS_PA_EQ_12
            +BENE_COUNT_PB_EQ_12+AVE_CA_VST_PB_EQ_12+AVE_OP_VST_PB_EQ_12+BENE_COUNT_PC_EQ_12+BENE_COUNT_PD_EQ_12+AVE_PDE_CST_PD_EQ_12
            , family=poisson, data=PDP_2010),direction = "both")


#AIC=Inf happens if using Poisson with non-integer values.

#Does Binomial work?
modp1<-glm(AVE_PDE_PD_EQ_12~., family=binomial, data=PDP_2010);
summary(modp1)
#No, because output needs to be between 0-1

#making matrix of scatter plots to bring to meeting (this will be ugly)
plot(PDP_2010,panel=panel.smooth)
#data too big - this would need to be broken out.  Not even sure if it's necessary...

#~~~~~~~~~~~~~~~~~Meeting with Prof~~~~~~~~~~~~~
#Rounded total RX col added
attach(PDP_2010)
PDP_2010_WD$TotRx<- (BENE_COUNT_PD_EQ_12 * AVE_PDE_PD_EQ_12)
PDP_2010_WD$TotRxRound<- round(PDP_2010_WD$TotRx, digits=0)
detach()

#model built with prof
mod_prof<-glm(TotRxRound~BENE_COUNT_PD_EQ_12+CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_COPD, family=poisson, data=PDP_2010_WD)
#~~~~~~~~~~~~~~~~Complete~~~~~~~~~~~~~~~~~~~~~~~~~

#Trying with comorbidities only (with bene count)
mod_comorbid<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA
              , family=poisson, data=PDP_2010_WD)
#using the log() of Bene count gives a better model than not
#Comorbidities all highly significant with and wihtout log(bene), but comorbid coefficients go in the right direction (i.e. positively corrleate) once the log is taken.
#Not great models yet, but at least they're working now.  

mod_offsettest<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                    CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA
                    ,family=poisson,data=PDP_2010_WD)
#This worked really well!!!!  Coefficients in the right directions!!!!

#Ok, making a model with all covariates and seeing what happens.
#prof says continuous variables need log transformation with poisson.
#I'm not sure about adding bene counts for Parts A, B, and C, no matter how predictive.
#I'm going to leave those out.  They correlate with Part D bene count which is definitely going in there already.

mod_poisson1<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                  BENE_SEX_IDENT_CD+log(BENE_AGE_CAT_CD)+
                  CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                  CC_2_OR_MORE+DUAL_STUS+
                  log(AVE_PA_PAY_PA_EQ_12)+log(AVE_IP_PAY_PA_EQ_12)+log(AVE_SNF_PAY_PA_EQ_12)+log(AVE_OTH_PAY_PA_EQ_12)+log(AVE_IP_ADM_PA_EQ_12)+log(AVE_SNF_DAYS_PA_EQ_12)+
                  log(AVE_PB_PAY_PB_EQ_12)+log(AVE_CA_PAY_PB_EQ_12)+log(AVE_OP_PAY_PB_EQ_12)+log(AVE_OTH_PAY_PB_EQ_12)+log(AVE_CA_VST_PB_EQ_12)+log(AVE_OP_VST_PB_EQ_12)+
                  log(AVE_PDE_CST_PD_EQ_12)
                  ,family=poisson,data=PDP_2010_WD)

#Ok, now let's see about using drop1() to make this better.
drop1(mod_poisson1,test="Chisq")

#This didn't drop anything.
#Yeah...  I'm getting rid of cost of drugs - this corrlates *too* well.
mod_poisson2<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                    BENE_SEX_IDENT_CD+log(BENE_AGE_CAT_CD)+
                    CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                    CC_2_OR_MORE+DUAL_STUS+
                    log(AVE_PA_PAY_PA_EQ_12)+log(AVE_IP_PAY_PA_EQ_12)+log(AVE_SNF_PAY_PA_EQ_12)+log(AVE_OTH_PAY_PA_EQ_12)+log(AVE_IP_ADM_PA_EQ_12)+log(AVE_SNF_DAYS_PA_EQ_12)+
                    log(AVE_PB_PAY_PB_EQ_12)+log(AVE_CA_PAY_PB_EQ_12)+log(AVE_OP_PAY_PB_EQ_12)+log(AVE_OTH_PAY_PB_EQ_12)+log(AVE_CA_VST_PB_EQ_12)+log(AVE_OP_VST_PB_EQ_12)
                  ,family=poisson,data=PDP_2010_WD)

#This made fit worse, not sure if that's a bad thing.
#trying drop1() again
#All variables still *highly* correlated.  Deviance even worse than before, no drops improve AIC.
#I want to compare pay vs. visit/days/admission.  Should not have both of these I don't think.
#Going to try the model with just pay with model with just visits.
mod_poisson_justpay<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                  log(AVE_PA_PAY_PA_EQ_12)+log(AVE_IP_PAY_PA_EQ_12)+log(AVE_SNF_PAY_PA_EQ_12)+log(AVE_OTH_PAY_PA_EQ_12)+
                  log(AVE_PB_PAY_PB_EQ_12)+log(AVE_CA_PAY_PB_EQ_12)+log(AVE_OP_PAY_PB_EQ_12)+log(AVE_OTH_PAY_PB_EQ_12)
                  ,family=poisson,data=PDP_2010_WD);

mod_poisson_justvisit<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                  log(AVE_IP_ADM_PA_EQ_12)+log(AVE_SNF_DAYS_PA_EQ_12)+
                  log(AVE_CA_VST_PB_EQ_12)+log(AVE_OP_VST_PB_EQ_12)
                  ,family=poisson,data=PDP_2010_WD)

#mod_poisson_justpay has lower deviance and lower AIC than mod_poisson_justvisit, although it also has more variables.
#mod_comorbid looks very similar, with quality somewhere in between (although all 3 models are terrible fits)

mod_poisson_justdemographics_nolog<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                   BENE_SEX_IDENT_CD+BENE_AGE_CAT_CD
                   ,family=poisson,data=PDP_2010_WD)

#still no good.
#Let's check out this overdispersion thing graphically
scatter.smooth(log(fitted(mod_poisson2)),log((PDP_2010_WD$TotRxRound-fitted(mod_poisson2))^2),xlab=expression(hat(mu))
               , ylab=expression(sigma^2==(y-hat(mu))^2))
abline(0,1,lty=2)

#No luck, running a pearson statistic to measure overdisperison.
phi<- sum(resid(mod_poisson2, type = "pearson")^2)/df.residual(mod_poisson2);
phi_justdemo<- sum(resid(mod_poisson_justdemographics_nolog, type = "pearson")^2)/df.residual(mod_poisson_justdemographics_nolog);
phi_justpay<- sum(resid(mod_poisson_justpay, type = "pearson")^2)/df.residual(mod_poisson_justpay);
phi_justvisit<- sum(resid(mod_poisson_justvisit, type = "pearson")^2)/df.residual(mod_poisson_justvisit)
phi_justvisit<- sum(resid(mod_poisson_justvisit, type = "pearson")^2)/df.residual(mod_poisson)

#Why isn't this working?  Oh, fitted values for glm exclude NA by default.  This causes error in TotRxRound-fitted()
#length(PDP_2010_WD$TotRxRound)=16699, length(fitted(mod_poisson2)) = 12043

#Right!  I can use StepAIC now that I'm getting AICs!
#then also consider doing add1() to complement drop1()
mod_poisson3<-stepAIC(glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                            BENE_SEX_IDENT_CD+log(BENE_AGE_CAT_CD)+
                            CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                            CC_2_OR_MORE+DUAL_STUS+
                            log(AVE_PA_PAY_PA_EQ_12)+log(AVE_IP_PAY_PA_EQ_12)+log(AVE_SNF_PAY_PA_EQ_12)+log(AVE_OTH_PAY_PA_EQ_12)+log(AVE_IP_ADM_PA_EQ_12)+log(AVE_SNF_DAYS_PA_EQ_12)+
                            log(AVE_PB_PAY_PB_EQ_12)+log(AVE_CA_PAY_PB_EQ_12)+log(AVE_OP_PAY_PB_EQ_12)+log(AVE_OTH_PAY_PB_EQ_12)+log(AVE_CA_VST_PB_EQ_12)+log(AVE_OP_VST_PB_EQ_12)
                          ,family=poisson,data=PDP_2010_WD),direction="forward")

#Maybe I should create a column with the sum of comorbidities...
attach(PDP_2010_WD);
PDP_2010_WD$TotComorbid<- (CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                             CC_2_OR_MORE+DUAL_STUS);
detach()

mod_lm_totcomorbid<-lm((AVE_PDE_PD_EQ_12~TotComorbid), data=PDP_2010_WD)
#Oh god, liner model with tot number of comorbidities done on a larf is *much* better.  Much much much.
#Let's try it with the individual comorbidities
mod_lm_comorbid<-lm(AVE_PDE_PD_EQ_12~CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA
                    ,data=PDP_2010_WD)

#Cancer is negatively correlated, the others are positively correlated.
#The individual comorbidities which seem to lead to the most prescriptions are Depression, Diabetes, CHF, COPD, and Alzheimers.
#The fewest prescriptions are Cancer (-), Osteoporosos, and Strketia


summary(lm(AVE_PDE_PD_EQ_12~CC_ALZHDMTA+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA
                    ,data=PDP_2010_WD))



#I do like the data/coefficients I get from having the comorbidities broken out like this.

#~~~~~~~~~~~~~~~~
#Ok, should I do anything else with poisson before I forget what I was doing?
#While looking for how to do poisson transformations...
#Found some info about quasi poisson
mod_quasipoisson2<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                    BENE_SEX_IDENT_CD+log(BENE_AGE_CAT_CD)+
                    CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                    CC_2_OR_MORE+DUAL_STUS+
                    log(AVE_PA_PAY_PA_EQ_12)+log(AVE_IP_PAY_PA_EQ_12)+log(AVE_SNF_PAY_PA_EQ_12)+log(AVE_OTH_PAY_PA_EQ_12)+log(AVE_IP_ADM_PA_EQ_12)+log(AVE_SNF_DAYS_PA_EQ_12)+
                    log(AVE_PB_PAY_PB_EQ_12)+log(AVE_CA_PAY_PB_EQ_12)+log(AVE_OP_PAY_PB_EQ_12)+log(AVE_OTH_PAY_PB_EQ_12)+log(AVE_CA_VST_PB_EQ_12)+log(AVE_OP_VST_PB_EQ_12)
                  ,family=quasipoisson(link="log"),data=PDP_2010_WD)

mod_quasipoisson_justpay<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                           log(AVE_PA_PAY_PA_EQ_12)+log(AVE_IP_PAY_PA_EQ_12)+log(AVE_SNF_PAY_PA_EQ_12)+log(AVE_OTH_PAY_PA_EQ_12)+
                           log(AVE_PB_PAY_PB_EQ_12)+log(AVE_CA_PAY_PB_EQ_12)+log(AVE_OP_PAY_PB_EQ_12)+log(AVE_OTH_PAY_PB_EQ_12)
                         ,family=quasipoisson,data=PDP_2010_WD);

mod_quasipoisson_justvisit<-glm(TotRxRound~offset(log(BENE_COUNT_PD_EQ_12))+
                             log(AVE_IP_ADM_PA_EQ_12)+log(AVE_SNF_DAYS_PA_EQ_12)+
                             log(AVE_CA_VST_PB_EQ_12)+log(AVE_OP_VST_PB_EQ_12)
                           ,family=quasipoisson,data=PDP_2010_WD)



#Yeah, this is totally linear - giving up on poisson and finishing the analysis.
#Running scatterplots against AVE_PDE_PD_EQ_12

#demographics - being young and female leads to more Rx
plot(PDP_2010_WD[c(1:2,33)],panel=panel.smooth)

#comorbidities - all increase Rx except for Cancer and Stroke. Osteoporosis only a little bit
#Cancer negatively correlates with alzheimers, stroke, and depression (surprising); CHF positively correlates wiht IschmCHT.
plot(PDP_2010_WD[c(3:13,33)],panel=panel.smooth)

#Other Medicare charge amounts- All positively correlate with Rx, some more htan others. 
#Also, they all strongly correlate with each other, some more than others.
plot(PDP_2010_WD[c("AVE_PA_PAY_PA_EQ_12","AVE_IP_PAY_PA_EQ_12","AVE_SNF_PAY_PA_EQ_12","AVE_OTH_PAY_PA_EQ_12"
,"AVE_PB_PAY_PB_EQ_12","AVE_CA_PAY_PB_EQ_12","AVE_OP_PAY_PB_EQ_12","AVE_OTH_PAY_PB_EQ_12","AVE_PDE_PD_EQ_12")],panel=panel.smooth) 

#Other Medicare visits - Average number of carrier/physician visits doesn't correlate with anything, really, including Rx count.
#Everything else highly correlates.  Especially IP admission with SNF days, but that makes sense.
plot(PDP_2010_WD[c("AVE_IP_ADM_PA_EQ_12","AVE_SNF_DAYS_PA_EQ_12","AVE_CA_VST_PB_EQ_12","AVE_OP_VST_PB_EQ_12","AVE_PDE_PD_EQ_12")],panel=panel.smooth)

plot(PDP_2010_WD[c("CC_2_OR_MORE","DUAL_STUS","AVE_PDE_PD_EQ_12")],panel=panel.smooth)


#Linear model all of the everything
mod_linear2<-lm(AVE_PDE_PD_EQ_12~BENE_SEX_IDENT_CD+BENE_AGE_CAT_CD+
                    CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                    CC_2_OR_MORE+DUAL_STUS+
                    AVE_PA_PAY_PA_EQ_12+AVE_IP_PAY_PA_EQ_12+AVE_SNF_PAY_PA_EQ_12+AVE_OTH_PAY_PA_EQ_12+AVE_IP_ADM_PA_EQ_12+AVE_SNF_DAYS_PA_EQ_12+
                    AVE_PB_PAY_PB_EQ_12+AVE_CA_PAY_PB_EQ_12+AVE_OP_PAY_PB_EQ_12+AVE_OTH_PAY_PB_EQ_12+AVE_CA_VST_PB_EQ_12+AVE_OP_VST_PB_EQ_12,
                  data=PDP_2010_WD)

#Running stepAIC on it

mod_linear2_step<-stepAIC(lm(AVE_PDE_PD_EQ_12~.-AVE_PDE_CST_PD_EQ_12-BENE_COUNT_PD_EQ_12-BENE_COUNT_PC_EQ_12-BENE_COUNT_PB_EQ_12-BENE_COUNT_PA_EQ_12, data=PDP_2010),direction="both",na.rm=T)
mod_linear2_step$anova
summary(mod_linear2_step)

mod_linear2_step2<-stepAIC(lm(AVE_PDE_PD_EQ_12~BENE_SEX_IDENT_CD+BENE_AGE_CAT_CD+
                  CC_ALZHDMTA+CC_CANCER+CC_CHF+CC_CHRNKIDN+CC_COPD+CC_DEPRESSN+CC_DIABETES+CC_ISCHMCHT+CC_OSTEOPRS+CC_RA_OA+CC_STRKETIA+
                  CC_2_OR_MORE+DUAL_STUS+
                  AVE_PA_PAY_PA_EQ_12+AVE_IP_PAY_PA_EQ_12+AVE_SNF_PAY_PA_EQ_12+AVE_OTH_PAY_PA_EQ_12+AVE_IP_ADM_PA_EQ_12+AVE_SNF_DAYS_PA_EQ_12+
                  AVE_PB_PAY_PB_EQ_12+AVE_CA_PAY_PB_EQ_12+AVE_OP_PAY_PB_EQ_12+AVE_OTH_PAY_PB_EQ_12+AVE_CA_VST_PB_EQ_12+AVE_OP_VST_PB_EQ_12,
                data=PDP_2010),direction = "both")
summary(mod_linear2_step2)

mod_linear2_step2$anova

par(mfrow=c(2,2));
plot(mod_linear2_step2)

#apparently there's a command called getDeltaRsquare() which I forgot about in rockchalk packate
getDeltaRsquare(mod_linear2_step2)
getDeltaRsquare(mod_lm_comorbid)
#everything is more influential on R2 in comorbid model.  I think linear2_step2 has too many predictors.

bptest(mod_linear2_step2)

#Getting rid of unwanted predictors.  
#Unfortunately, -BENE_COUNT_PC_EQ_12 has null values and is breaking things when I try to take it out.
mod_linear2_step3<-stepAIC(lm(AVE_PDE_PD_EQ_12~. -AVE_PDE_CST_PD_EQ_12 -BENE_COUNT_PD_EQ_12 -BENE_COUNT_PB_EQ_12 -BENE_COUNT_PA_EQ_12,
                                data=PDP_2010, na.action=na.exclude),direction = "both")



#Did my own forward stepwise, like this best of them due to R2=.8812 and Intercept error = .2344
mod_lm_construct_best<-lm(AVE_PDE_PD_EQ_12~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_CHRNKIDN+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD)
summary(mod_lm_construct_best)
getDeltaRsquare(mod_lm_construct_best)
plot(mod_lm_construct_best)
#Testing heteroscedasticity
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
bptest(mod_lm_construct_best)

plot(lm((AVE_PDE_PD_EQ_12)^2~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))
#this one throws everything out of whack

plot(lm(sqrt(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))
bptest(lm(sqrt(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))
#this ain't bad.  Still heteroscedastic

plot(lm(log(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))
bptest(lm(log(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))
#not bad eihter.  Still heteroscedastic.

plot(lm(exp(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))
bptest(lm(exp(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD))

#Now this is intesting...  took the exponential of the outcome and there are some strange effects
#especially due to obs 3666 (173.5) and 8104 (174.7)
#this following code is from Avery's lecture 5
mod_lm_construct_exp<-lm(exp(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD)
mod_lm_construct_exp.dffits<-dffits(mod_lm_construct_exp)
mod_lm_construct_exp.hat<-hatvalues(mod_lm_construct_exp)
id.mod_lm_construct_exp.dffits<-which(mod_lm_construct_exp.dffits>(2*sqrt((9+1)/16699)))
influence.measures(mod_lm_construct_best)

#Removed influential observations in Excel just to see if this is worth pursuing
#it isn't
mod_lm_construct_exp_nooutliers<-lm(exp(AVE_PDE_PD_EQ_12)~CC_DEPRESSN+CC_DIABETES+CC_CHF+CC_ALZHDMTA+CC_COPD+CC_RA_OA+CC_ISCHMCHT+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_NoOutliers)
mod_lm_construct_exp_nooutliers.dffits<-dffits(mod_lm_construct_exp_nooutliers)
mod_lm_construct_exp_nooutliers.hat<-hatvalues(mod_lm_construct_exp_nooutliers)
id.mod_lm_construct_exp_nooutliers.dffits<-which(mod_lm_construct_exp_nooutliers.dffits>(2*sqrt((9+1)/16699)))
influence.measures(mod_lm_construct_best)


#Output variable is moderately skewed (0.5-1.0) (Class 6)
> skewness(AVE_PDE_PD_EQ_12)
[1] 0.5777349
> kurtosis(AVE_PDE_PD_EQ_12)
[1] -0.1198344

#Found a better way to test correlation than the graphs
All_Cor<-cor(PDP_2010_WD, use="complete.obs")
View(All_Cor)
write.table(All_Cor, file="All_Cor.csv",quote=F, sep=",", na="", row.names=T)


summary(mod_lm_construct_exp)
#This is not a good model.  R2=0.001823
mod_lm_construct_exp_Totcomorbid<-lm(exp(AVE_PDE_PD_EQ_12)~TotComorbid+sqrt(AVE_IP_ADM_PA_EQ_12)+DUAL_STUS, data=PDP_2010_WD)
summary(mod_lm_construct_exp_Totcomorbid)

#trying sandwich test with previous constructed 10-factor model.
coeftest(mod_lm_construct_best)
summary(mod_lm_construct_best)
