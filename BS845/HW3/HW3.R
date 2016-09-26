Homework 3 
Please submit your R codes and output, and additional answers to the questions.
For all figures, please properly label x and y axes and provide a title to the figure.
If there are multiple plotting symbols or different colors in a figure, provide a legend to annotate the figure.
Using the Framingham Heart Study teaching dataset,

1.	Using R internal dataset, state.x77, generate scatter plots with smoothed lines to examine the relationship between 
Income and Illiterary, 
and between Income and Life Exp respectively. 
Place the two plots side by side in one figure. Comment on the linearity of the relationship between the plotted variables.

 stateframe<-as.data.frame(state.x77)
 attach(stateframe)
 scatter.smooth(Income,Illiteracy)
 scatter.smooth(Income,Illiteracy,main="State Income v. Illiteracy",xlab="Per Capita Income (1974)", ylab="Illiteracy (1970, percent of population)")
 par(mfrow=c(1,2))
 scatter.smooth(Income,Illiteracy,main="State Income v. Illiteracy",xlab="Per Capita Income (1974)", ylab="Illiteracy (1970, percent of population)")
 scatter.smooth(Income,`Life Exp`,main="State Income v. Life Expectancy",xlab="Per Capita Income (1974)", ylab="Life Expectancy in Years (1969-71)")


2.	Check if the distribution of TOTCHOL at PERIOD equal to 1 is normal distributed. Please comment on the normality of TOTCHOL 
a.	If you feel the distribution if not normal, apply an appropriate transformation on TOTALCHOL to make it approximately normal. 
b.	Show that the transformed variable is approximately normal using normal QQ plot and histogram with normal density curve and a kernel density curve in the same plot. Please plot the two figures in a 1x2 array.

attach(fram)
Period1fram<-fram[PERIOD==1,]
hist(Period1fram$TOTCHOL,xlab="Total Cholesterol", main="Total Cholesterol Frequency Distribution")
#Distribution appears to be mostly normal but has large outliers.
summary(Period1fram$TOTCHOL)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#107     206     234     237     264     696      52 
#Mean is 237 and Median is 234. These are "close" and support hypothesis of normality.
par(mfrow=c(1,2))
hist(Period1fram$TOTCHOL,main="Total Cholesterol Frequency Distribution", xlab="Total Cholesterol")
qqnorm(Period1fram$TOTCHOL)
qqline(Period1fram$TOTCHOL)





#The QQ Plot does not support normality. The data points deviate from the normal line at the tails.
#Maybe squaring the data will help?
qqnorm((Period1fram$TOTCHOL)^2)
qqline((Period1fram$TOTCHOL)^2)
#This made it worse, trying SQRT
qqnorm(sqrt(Period1fram$TOTCHOL))
qqline(sqrt(Period1fram$TOTCHOL))
#SQRT helped a little
qqnorm(log(Period1fram$TOTCHOL))
qqline(log(Period1fram$TOTCHOL))
#Log works pretty well.

#Setting variables with transformed values
x<-log(Period1fram$TOTCHOL)
m<-mean(x,na.rm=TRUE)
s<-sd(x,na.rm=TRUE)

#enabling multiple charts at one time
par(mfrow=c(1,2))

hist(x,prob=T,main="Frequncy of Log Total Cholesterol",xlab="Log Total Cholesterol")
curve(dnorm(x,mean=m,sd=s),col="red",add=TRUE)
lines(density(na.omit(x)),col="blue")
legend("topright",c("Normal Density","Kernel Density"),fill=c("red","blue"))

qqnorm(x)
qqline(x)
dev.off()





3.	Titanic is an R internal dataset. Use barplot to examine the difference in survival rate for different group of people.
a.	Reduce the Titanic into a two-way table with Class as row variable, and other factors and outcome combinations as column variable.
b.	Compute survival rate for each Age and Sex combination within each Class
c.	Draw a bar plot with bars grouped by Class
d.	Comment on if and how each factor affects survival rate for different factors.

a.
#Converting Titanic table into data frame to allow for easier manipulation.
TitanicDF<-data.frame(Titanic)
head(TitanicDF)

#Class    Sex   Age Survived Freq
#1   1st   Male Child       No    0
#2   2nd   Male Child       No    0
#3   3rd   Male Child       No   35
#4  Crew   Male Child       No    0
#5   1st Female Child       No    0
#6   2nd Female Child       No    0

#This creates a Matrix with Class as the row variable (byrow=F)
#ncol=8 was used because the other columns in the data frame had 2^3=8 combinations.
TitanicMatrix<-matrix(TitanicDF$Freq, ncol=8, byrow=F)
TitanicMatrix

#[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#[1,]    0    0  118    4    5    1   57  140
#[2,]    0    0  154   13   11   13   14   80
#[3,]   35   17  387   89   13   14   75   76
#[4,]    0    0  670    3    0    0  192   20


#Naming rows and columns to avoid confusion
rownames(TitanicMatrix) <- unique(TitanicDF$Class)
colnames(TitanicMatrix) <- unique(paste(TitanicDF$Sex,TitanicDF$Age,TitanicDF$Survived))
TitanicMatrix

#Male Child No Female Child No Male Adult No Female Adult No Male Child Yes Female Child Yes Male Adult Yes
#1st              0               0           118               4              5                1             57
#2nd              0               0           154              13             11               13             14
#3rd             35              17           387              89             13               14             75
#Crew             0               0           670               3              0                0            192
#Female Adult Yes
#1st               140
#2nd                80
#3rd                76
#Crew               20


b. 
#Calculating element wise survival rates on matrix cells
TitSurvivalRates <- TitanicMatrix[,c(5,6,7,8)]/(TitanicMatrix[,c(5,6,7,8)]+TitanicMatrix[,c(1,2,3,4)])
TitSurvivalRates
#setting to more meaningful names
colnames(TitSurvivalRates)<-unique(paste(TitanicDF$Sex,TitanicDF$Age))

#Male Child Female Child Male Adult Female Adult
#1st   1.0000000    1.0000000 0.32571429    0.9722222
#2nd   1.0000000    1.0000000 0.08333333    0.8602151
#3rd   0.2708333    0.4516129 0.16233766    0.4606061
#Crew        NaN          NaN 0.22273782    0.8695652

#Crew col 1 and 2 are NaN because there were no children serving on crew. 0/0=NaN.


c. 


barplot(TitSurvivalRates
        ,col=rainbow(4)
        ,beside=T
        ,ylab="Survival Rate"
        ,main="Titanic Survival Rate by Class, Gender, and Age"
        ,xlab="Gender and Age Category")
legend("top",rownames(TitSurvivalRates),fill=c(rainbow(4)))


