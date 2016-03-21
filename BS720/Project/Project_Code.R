# Um…  gonna run an ANOVA across all 50 states to test for equivalence
# Gonna maybe do the same thing by region – are all states in a region equivalent?
# Gonna then see what sort of ANOVA follow up tests I can do to see which one of these things is kinda different.
# Identify outlier states based on national averages (p.47 Tufte)

# What graphs….
# •	Box and whisker for all states, maybe grouped by region?
# •	Want to do at least one map graph, that would be fun
# •	Do a graph like that one homework where each state was listed along the side (I had done it by using state names as pips incorrectly) (also see p.94 Tufte)
# •	Histogram of all state means?


#Average.Covered.Charges is factor data, need to 
#coerce into numeric so I can do things to it.
#Ugh, all of the columns are factors.... Almost
#I probably want a matrix or a data frame

#wait, it's in a data frame.
#Ok, going to try to strip the leading $ and then conver to numeric string.

IPPS$CovChg<-(substring(Average.Covered.Charges,2))
IPPS$CovChg<-as.numeric(IPPS$CovChg)

> str(CovChg)
num [1:4553] 40237 49636 47469 44164 17021 ...

> head(CovChg)
[1] 40237.34 49635.90 47468.55 44164.29 17021.15 40433.93


> tapply(CovChg,Provider.State,mean)
AK       AL       AR       AZ       CA       CO       CT       DC       DE       FL       GA       HI       IA 
48563.07 33942.96 23463.36 43449.79 65986.26 40569.28 28278.79 41854.51 25598.66 44378.64 30156.06 28958.04 24229.97 
ID       IL       IN       KS       KY       LA       MA       MD       ME       MI       MN       MO       MS 
23931.96 36091.94 27689.28 32139.55 24858.63 35294.04 18290.48 11816.78 17549.79 21824.56 25075.62 30983.54 31847.48 
MT       NC       ND       NE       NH       NJ       NM       NV       NY       OH       OK       OR       PA 
22913.96 23280.84 18975.18 30815.09 24579.86 64308.23 34142.37 66986.40 30514.99 29516.14 31406.22 24170.24 38561.72 
RI       SC       SD       TN       TX       UT       VA       VT       WA       WI       WV       WY 
27157.34 33796.01 24267.95 29494.73 44351.72 24301.84 27467.22 16061.84 31308.52 25054.19 18417.50 29264.07 

CovChg_means<-tapply(CovChg,Provider.State,mean)
CovChg_means<-sort(CovChg_means,decreasing=TRUE)
barplot(CovChg_means,horiz=TRUE,cex.names=.4,las=2,tck=1,main="Mean Charge Amount for Inpatient AMI Treatment by State",ylab="Facility State",xlab="Average Charge")

#For all DRGs
> mean(IPPS$CovChg)
[1] 35260.82
> sd(IPPS$CovChg)
[1] 23240.81

#Made subset of MCC only for cleaner analysis
> IPPS_MCC<-subset(IPPS,DRG.Definition=="280 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W MCC")

> detach(IPPS)
> attach(IPPS_MCC)

#include this table as an appendix.
> MCC_barchart<-tapply(IPPS_MCC$CovChg,IPPS_MCC$Provider.State,mean)
AK       AL       AR       AZ       CA       CO       CT       DC 
79674.45 42942.19 30499.54 53782.40 82123.22 56753.31 34285.45 59618.15 
DE       FL       GA       HI       IA       ID       IL       IN 
32491.66 58014.44 40009.14 40402.02 31095.33 27664.27 45789.06 34039.81 
KS       KY       LA       MA       MD       ME       MI       MN 
38761.49 32200.10 44942.87 23194.85 17692.27 23577.87 27855.81 32367.31 
MO       MS       MT       NC       ND       NE       NH       NJ 
40042.74 41994.60 27671.08 28746.18 23550.85 37428.97 30492.46 89859.87 
NM       NV       NY       OH       OK       OR       PA       RI 
47661.49 89514.62 39620.23 36821.64 41030.49 32733.51 48935.62 31690.53 
SC       SD       TN       TX       UT       VA       VT       WA 
42626.73 29524.27 38480.40 56750.65 29385.35 35343.33 24005.94 39658.97 
WI       WV       WY 
28618.87 25022.98 35610.38 

MCC_barchart<-sort(MCC_barchart, decreasing=TRUE)
> barplot(CovChg_means_MCC,horiz=TRUE,cex.names=.4,las=2,tck=1,main="Mean Charges for Inpatient AMI with MCC by State",ylab="Facility State",xlab="Average Charge")

#This sets the location of the column value labels
Plotbars<-barplot(MCC_barchart,horiz=TRUE,cex.names=.6,cex.axis=.5,las=1,tck=1,xlim=c(0,100000),col=rainbow(51),main="Mean Charges for Inpatient AMI with MCC by State",ylab="Facility State",xlab="Average Charge")
#This is the main plot
barplot(MCC_barchart,horiz=TRUE,cex.names=.6,cex.axis=.6,las=1,tck=0.01,xlim=c(0,100000),col=rainbow(51),main="Mean Charges for Inpatient AMI with MCC by State",ylab="Facility State",xlab="Average Charge")
#Places a mean value line
abline(v=40443.13,par(lwd=2,lty=2))
text(47000,60,"<- National Mean",cex=.6)
#got rid of scientific notation on x-axis
options(scipen=5)
#displays values for each row
text(y=Plotbars,x=MCC_barchart,label=round(MCC_barchart,digits=2), pos=4, cex=.6)


> mean(CovChg_means_MCC)
[1] 40443.13
> sd(CovChg_means_MCC)
[1] 16353.51


#Trying to make awesome map
> library(ggplot2)
> install.packages("maps")
> library(maps)
> install.packages("RcolorBrewer")
> library(RColorBrewer)


ggplot(data,aes(fill=murder))+geom_map(aes(map_id=state),map=map)+expand_limits(x=map$long, y=map$lat);

#To get this to work, I need to transform CovChg_means_MCC
> CovChg_means_MCC<-as.data.frame(CovChg_means_MCC)

#Now need to create a new row with state names since can't map off of row names, only values.
> CovChg_means_MCC$state<-row.names(CovChg_means_MCC)
> CovChg_means_MCC$charges<-CovChg_means_MCC$V1
#need to use lowercase state names rather than abbreviations for 
#mapping to work.
#note - DC was not considered a state
>CovChg_means_MCC$statename<-tolower(state.name[match(CovChg_means_MCC$state,state.abb)])

#YESSSSSSS!!!!!!!
> ggplot(CovChg_means_MCC,aes(fill=CovChg_means_MCC$charges))+geom_map(aes(map_id=CovChg_means_MCC$statename),map=map)+expand_limits(x=map$long, y=map$lat);

#Prettier
ggplot(CovChg_means_MCC,aes(fill=CovChg_means_MCC$charges))+geom_map(aes(map_id=CovChg_means_MCC$statename),map=map)+expand_limits(x=map$long, y=map$lat)+ggtitle("Average Covered Inpatient Charges for AMI by State")+theme(plot.title=element_text(size=20),axis.text.y=element_blank(),axis.text.x=element_blank(),legend.title=element_blank());


#Now I'm going to play with an ANOVA.  

#First, checking for equivalence across 3 DRGs
#No, wait. Remember I'm ONLY doing MCC now.

#Running for all 50 states 
#first, check the assumptions (which won't be met)
#Independent observations - SURE
#Dependent variable is normal in each group....


#Equal variance of dependent variable across groups


#Ok, having trouble checking the assumptions
IPPS_MCC$State.Vector<-as.vector(IPPS_MCC$Provider.State)

ANOVA_state_MCC <- aov(IPPS_MCC$CovChg~IPPS_MCC$State.Vector)

> ANOVA_state_MCC
Call:
  aov(formula = IPPS_MCC$CovChg ~ IPPS_MCC$Provider.State)

Terms:
  IPPS_MCC$Provider.State    Residuals
Sum of Squares             597484611506 814420277664
Deg. of Freedom                      50         1830

Residual standard error: 21095.93
Estimated effects may be unbalanced


summary(ANOVA_state_MCC)

Df    Sum Sq   Mean Sq F value Pr(>F)    
IPPS_MCC$Provider.State   50 5.975e+11 1.195e+10   26.85 <2e-16 ***
  Residuals               1830 8.144e+11 4.450e+08                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#They're DEFINITELY different

#Ok, going to also see about comparing all states to each other
#Since there are so many observations, I need to make a correction
#in order to minimize Type1 errors (~2.5 will occur if I compare
#means without correcting).
#Using Tukey (lecture 4, p.21)

TukeyOutput<-TukeyHSD(ANOVA_state_MCC)
as.data.frame(TukeyOutput)
#*laughs*  Well, that table's sure unreadable. 50! rows.
#Bonferroni has better output layout at least, althogh it's more
#conservative than Tukey

pairwise.t.test(IPPS_MCC$CovChg, IPPS_MCC$State.Vector, p.adj="bonferroni")


#Ok, this isn't working.  Crosswalking everything into regions and working from there.
#state.name function has region ability

IPPS_MCC$Prov.Region <- state.region[match(IPPS_MCC$Provider.State,state.abb)]

#Need to manually assign DC to "South"
#Setting region info to Vector so region names preserved for next steps
IPPS_MCC$Prov.Region<-as.vector(IPPS_MCC$Prov.Region)
#Creating a column of region names 
DCMatrix<-ifelse(IPPS_MCC$State.Vector=="DC","South",IPPS_MCC$Prov.Region)
#Replacing IPPS_MCC$Prov.Region with DCMatrix
IPPS_MCC$Prov.Region<-DCMatrix

#Used table to confirm that region counts still the same and visually checked data for accuracy.


#Now that I have region data, running ANOVAs, etc.
#It's probably manageable enough to test the assumptions now.

#Running ANOVA first because I have the code
ANOVA_region_MCC <- aov(IPPS_MCC$CovChg~IPPS_MCC$Prov.Region)
ANOVA_region_MCC
summary(ANOVA_region_MCC)

Df    Sum Sq   Mean Sq F value Pr(>F)    
IPPS_MCC$Prov.Region    3 1.827e+11 6.089e+10   92.97 <2e-16 ***
  Residuals            1877 1.229e+12 6.549e+08                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Reject H0 that all regions have equal prices.

#Ok, which regions are different from each other?
#Making sure to use a correction to minimize Type1 errors
#Using Tukey (lecture 4, p.21)

TukeyOutput<-TukeyHSD(ANOVA_region_MCC)
TukeyOutput

Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = IPPS_MCC$CovChg ~ IPPS_MCC$Prov.Region)

$`IPPS_MCC$Prov.Region`
diff       lwr       upr     p adj
Northeast-North Central 10342.205  5814.545 14869.864 0.0000000
South-North Central      7299.611  3366.530 11232.692 0.0000117
West-North Central      30897.767 26011.515 35784.020 0.0000000
South-Northeast         -3042.594 -7152.791  1067.603 0.2268715
West-Northeast          20555.563 15525.646 25585.479 0.0000000
West-South              23598.157 19095.992 28100.321 0.0000000

#The only regions which might even be the same are South and Northeast

> tapply(IPPS_MCC$CovChg,IPPS_MCC$Prov.Region,mean)
North Central     Northeast         South          West 
35958.84      46301.04      43258.45      66856.60 
> tapply(IPPS_MCC$CovChg,IPPS_MCC$Prov.Region,sd)
North Central     Northeast         South          West 
15605.85      33019.57      21470.67      34259.52 


#Now that I've run the test, checking the assumptions of ANOVA.  This data is probably skew and not normally distributed.
#Independent observations - SURE
#Dependent variable is normal in each group....
#Equal variance of dependent variable across groups
#Running residual plots for each.
#First, it's easier if I break out the data into the 4 regional categories.


Northeast_MMC<-subset(IPPS_MCC,Prov.Region=="Northeast");
Northcentral_MMC<-subset(IPPS_MCC,Prov.Region=="North Central");
South_MMC<-subset(IPPS_MCC,Prov.Region=="South");
West_MMC<-subset(IPPS_MCC,Prov.Region=="West");

detach(South_MMC)
attach(West_MMC)
par(mfrow=c(2,2))
plot(CovChg)
hist(CovChg, main= "Number of rooms")
qqnorm(CovChg, main = "QQ-plot for Number of Rooms")
qqline(CovChg)
boxplot(CovChg, main="Boxplot of Number of Rooms per dwelling")
#Northeast, definitely left skew, not normal
#Northcentral, less skew, but still left skew, not normal
#South resembles Northcentral. Left skew, not normal
#West is most normal looking, but still not.

#Doing formal Shapiro tests, none are normally distributed at a high level of significance.
> shapiro.test(Northeast_MMC$CovChg)

Shapiro-Wilk normality test

data:  Northeast_MMC$CovChg
W = 0.83319, p-value < 2.2e-16

> shapiro.test(Northcentral_MMC$CovChg)

Shapiro-Wilk normality test

data:  Northcentral_MMC$CovChg
W = 0.92673, p-value = 4.258e-14

> shapiro.test(South_MMC$CovChg)

Shapiro-Wilk normality test

data:  South_MMC$CovChg
W = 0.92484, p-value < 2.2e-16

> shapiro.test(West_MMC$CovChg)

Shapiro-Wilk normality test

data:  West_MMC$CovChg
W = 0.9223, p-value = 1.999e-11

#Checking for equal variance using Bartlett test
bartlett.test(IPPS_MCC$CovChg~IPPS_MCC$Prov.Region)

Bartlett test of homogeneity of variances

data:  IPPS_MCC$CovChg by IPPS_MCC$Prov.Region
Bartlett's K-squared = 327.29, df = 3, p-value < 2.2e-16

#Reject H0 that the variability in charge amount is equal for all region categories.

#In this case, we should really use a nonparametric alternative to ANOVA.
#Running a Kruskal-Wallis Test
kruskal.test(IPPS_MCC$CovChg~IPPS_MCC$Prov.Region)
Error in kruskal.test.default(c(40237.34, 49635.9, 47468.55, 44164.29,  : 
  all group levels must be finite

#Test doesn't like the format of Prov.Region, so adding a new
#column with Prov.Region as a factor:
IPPS_MCC$Prov.Region.factor<-as.factor(Prov.Region)
kruskal.test(IPPS_MCC$CovChg~IPPS_MCC$Prov.Region.factor)

Kruskal-Wallis rank sum test

data:  CovChg and Prov.Region.factor
Kruskal-Wallis chi-squared = 206.62, df = 3, p-value <
  2.2e-16

#Prices are still not equal across regions.
#There is no readily available non-parametric Tukey/Bonferroni alternative
#Can repeat Wilcoxon tests to test equivalence between nonparametric means 
#There are few enough to brute force - should I be fancy and use a loop?

Regions<-c("Northeast_MMC$CovChg","Northcentral_MMC$CovChg","South_MMC$CovChg","West_MMC$CovChg")
wilcox.test(South_MMC$CovChg,West_MMC$CovChg)

i<-1
j<-2
repeat(
  i < nrow(Regions)){
  wilcox.test(Regions[i],Regions[j]);
  i<-i+1;
};

#ok, loop not working. Brute forcing, looping if I have time.

wilcox.test(Northeast_MMC$CovChg,Northcentral_MMC$CovChg)
#p-value = 0.03255
wilcox.test(Northeast_MMC$CovChg,South_MMC$CovChg)
#p-value = 0.1168
wilcox.test(Northeast_MMC$CovChg,West_MMC$CovChg)
#p-value < 2.2e-16
wilcox.test(Northcentral_MMC$CovChg,South_MMC$CovChg)
#p-value = 4.734e-08
wilcox.test(Northcentral_MMC$CovChg,West_MMC$CovChg)
#p-value < 2.2e-16
wilcox.test(South_MMC$CovChg,West_MMC$CovChg)
#p-value < 2.2e-16


