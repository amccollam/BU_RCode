#Andi McCollam
#February 1, 2016
#BS720
#HW 1

#Problem 1
help("Seatbelts")
#Seatbelts is a dataset which seems to be in R by default.  
#It's a data set comparing UK automobile driver death rates before and after the introduction of mandatory seatbelt legislation.
#The data set has 8 variables (columns)

help("OrchardSprays")
#OrchardSprays is another dataset available in R by default.
#This data set analyzes an experiment run to determine honeybee repellant potency of different components of orchard sprays.
#There are 64 observatinos across 4 variables.  This is stored as a data frame.

#Problem 2
#Searching for continuous variables in Seatbelts
str(Seatbelts[,1])
str(Seatbelts[,2])
str(Seatbelts[,3])
str(Seatbelts[,4])
str(Seatbelts[,5])
str(Seatbelts[,6])
str(Seatbelts[,7])
str(Seatbelts[,8])
#All return data type of Time-Series

Seatbelts
#With the exception of categorical column law [,8], all colums appear to be continuous.

#mean(Seatbelts[,1:7]) gave output of 2576.329 and appears to have given the mean of all values in all columns.
#summary gives Mean, Median, Min, Max
summary(Seatbelts[,1:7])

#calculates standard deviations
sd(Seatbelts[,1])
sd(Seatbelts[,2])
sd(Seatbelts[,3])
sd(Seatbelts[,4])
sd(Seatbelts[,5])
sd(Seatbelts[,6])
sd(Seatbelts[,7])

#calculates variance
var(Seatbelts[,1])
var(Seatbelts[,2])
var(Seatbelts[,3])
var(Seatbelts[,4])
var(Seatbelts[,5])
var(Seatbelts[,6])
var(Seatbelts[,7])

#calculates range. range() function just gives min and max values
max(Seatbelts[,1])-min(Seatbelts[,1])
max(Seatbelts[,2])-min(Seatbelts[,2])
max(Seatbelts[,3])-min(Seatbelts[,3])
max(Seatbelts[,4])-min(Seatbelts[,4])
max(Seatbelts[,5])-min(Seatbelts[,5])
max(Seatbelts[,6])-min(Seatbelts[,6])
max(Seatbelts[,7])-min(Seatbelts[,7])

#Searching for continuous variables in OrchardSprays
str(OrchardSprays[,1:4])
OrchardSprays
#decrease [,1] is the only continuous variable.  rowpos and colpos have data type num, but are categorical.
mean(OrchardSprays$decrease)
sd(OrchardSprays$decrease)
var(OrchardSprays$decrease)
min(OrchardSprays$decrease)
max(OrchardSprays$decrease)
median(OrchardSprays$decrease)
max(OrchardSprays$decrease)-min(OrchardSprays$decrease)

#Problem 3
#create matricies
a<-matrix(c(1,3,2,4),ncol=2)
b<-matrix(c(1,-1),ncol=1)
c<-matrix(c(1,5,3,7),ncol=2)

#perform calculations
(a+c)*4
#     [,1] [,2]
#[1,]    8   20
#[2,]   32   44

2*b
#[,1]
#[1,]    2
#[2,]   -2

#Problem 4
#a) Attach 3rd row to a
a<-rbind(a,5:6)

#b) Attach a 3rd col to c
c<-cbind(c,c(4,8))

#c) Mean of 1st col of a
mean(a[,1])
#[1] 3

#Problem 5
#Selecting csv from gui file structure prompt
pulse<-read.csv(file.choose())
#imported pulsedata.csv from downloads folder

#Problem 6
#a)Create pulse_diff vector
pulse_diff<-pulse$Pulse2 - pulse$Pulse1

#b)create bmi variable
bmi<- (pulse$Weight)/((pulse$Height/100)^2)

#c)attach pulse_diff and bmi to pulse
pulse<-cbind(pulse,pulse_diff,bmi)

#d)output as a text file
write.table(pulse, file = "pulse_new.txt", quote = FALSE, row.names=FALSE)
#pulse_new.txt was exported into wd

#Problem 7
#Create boxplots of Pulse1 and Pulse2 data
help(boxplot)
boxplot(pulse$Pulse1)
boxplot(pulse$Pulse2)

#Problem 8
#Install epitools
install.packages("epitools")

#% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
#Dload  Upload   Total   Spent    Left  Speed
#0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0 65  223k   65  146k    0     0   286k      0 --:--:-- --:--:-- --:--:--  285k100  223k  100  223k    0     0   345k      0 --:--:-- --:--:-- --:--:--  345k
#
#The downloaded binary packages are in
#/var/folders/zf/s66prmks063_j9xm3cm_8cg80000gn/T//Rtmpyy3yjo/downloaded_packages

#load epitools library
library(epitools)

#Problem 9
#lookup ratetable example code
help("ratetable")

#Method 1, numbers
#ratetable(bc0, py0, bc1, py1)

method1<-ratetable(5, 7, 4, 55, 67, 21)
method1
# Outcome
# Predictor  Cases Person-time
# Exposed1     5          55
# Exposed2     7          67
# Exposed3     4          21

#Method2, vector
# dat <- c(bc0, py0, bc1, py1)
# ratetable(dat)

dat <- c(5,7,4,55,67,21)
method2<-ratetable(dat)
method2
# Outcome
# Predictor  Cases Person-time
# Exposed1     5          55
# Exposed2     7          67
# Exposed3     4          21

#Method3, vector
# cases <- c(bc0, bc1)
# pyears <- c(py0, py1)
# ratetable(bc.cases = cases, person.years = pyears)

cases <-c(5,7,4)
pyears <- c(55,67,21)
method3<-ratetable ("Number of Cases" = cases, Person_years=pyears)
method3
# Number of Cases Person_years
# Exposed1               5           55
# Exposed2               7           67
# Exposed3               4           21

#Method4, matrix
##1 matrix
# r238 <- matrix(c(41, 28010, 15, 19017), 2, 2)
# dimnames(r238) <- list(c("BC cases", "Person-years"),
#                        "Radiation" = c("Yes", "No"))
# r238
# r238b <- t(r238)
# r238b
# ratetable(r238b, rev = "r")

exp_group <- matrix(c(5,7,4,55,67,21),nrow=3,ncol=2,dimnames = list("Exposed Group"=c("Unexposed","Minimal","Full"),c("Number of Cases","Person_years")))
exp_group
method4<-ratetable(exp_group)
method4
# Exposed Group Number of Cases Person_years
# Unexposed               5           55
# Minimal                 7           67
# Full                    4           21
