#HW 2, 2/8/2016
#Use the help() function to get a handle on the built-in R 
# dataset USArrests. Give it a new name (so as to redefine it as 
# a new object, for example, use command arrests<-USArrests).

?USArrests
head(USArrests)
arrests<-USArrests
apply(arrests[,1:4],2,mean,na.rm=TRUE)
total_crime<-apply(arrests[,c(1,2,4)],1,sum,na.rm=TRUE)
arrests$total_crime<-total_crime
quantile(arrests$total_crime,probs=c(0,.2,.4,.6,.8,1))

#Makes a scatterplot and saves it as a png to wd
png("TotalCrime.png")
plot(arrests$total_crime,ylab="Total Crimes",xlab="State Name",main="Total Crime by State",type="n") 
text(total_crime,labels=row.names(arrests),cex=0.6)
dev.off()

# Use the swiss dataset (the dataset is within R) to create/output the following plots: 
# a.	Create a histogram of all of the continuous variables (there should be 6) in one *.tiff 
# figure that has two rows and three columns. Label the horizontal axis and give each plot 
# its own main title. 
tiff(filename="SwissData.tiff")
attach(swiss)
par(mfrow=c(2,3))
hist(Fertility,main="Standardized Fertility Measure",xlab="Fertility")
hist(Agriculture,main="% of Maiels Involved in Agriculture",xlab="Agriculture")
hist(Examination,main="% Draftees with Highest Mark",xlab="Examination")
hist(Education,main="% Post Primary Education",xlab="Education")
hist(Catholic,main="% Catholic",xlab="Catholic")
hist(Infant.Mortality,main="Live Births Living Less than 1 Year",xlab="Infant Mortality")
detach(swiss)
dev.off()

# Create 5 scatterplots. Plot each of the continuous variables versus Infant Mortality in 
# one *.tiff figure that has 5 rows and 1 column. Label the axes and give each plot its own main title. 
tiff(filename="SwissPlot.tiff")
attach(swiss)
par(mfrow=c(5,1))
plot(Infant.Mortality,Fertility,main="Fertility vs. Infant Mortality",ylab="Fertility",xlab="Infant Mortality")
plot(Infant.Mortality,Agriculture,main="Agriculture vs. Infant Mortality",ylab="Agriculture",xlab="Infant Mortality")
plot(Infant.Mortality,Examination,main="Examination vs. Infant Mortality",ylab="Examination",xlab="Infant Mortality")
plot(Infant.Mortality,Education,main="Education vs. Infant Mortality",ylab="Education",xlab="Infant Mortality")
plot(Infant.Mortality,Catholic,main="Catholic vs. Infant Mortality",ylab="Catholic",xlab="Infant Mortality")
detach(swiss)
dev.off()


# Use the ChickWeight dataset (the dataset is within R) to do the following:
#   a. Create a qqplot of the Weight variable. Label the main title and store the file as a *.png
# file.

png(filename="ChickWeightQQ.png")
qqnorm(ChickWeight$weight,main="Weight Q-Q Plot")
qqline(ChickWeight$weight)
dev.off()


# b. Create a *.tiff file of side-by-side boxplots of Weight for each type of chick diet (there                                                                                        should be one figure with four side by side boxplots). Label the axes and the main title.
tiff(filename="ChickWeightBox.tiff")
boxplot(weight ~ Diet, data=ChickWeight,main="Chick Weight by Diet", xlab="Diet", ylab="Weight (gm)")
dev.off()

tapply(conc,Type,mean)
tapply(conc,Type,sd)
tapply(uptake,Type,mean)
tapply(uptake,Type,sd)


