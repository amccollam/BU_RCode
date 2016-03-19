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

