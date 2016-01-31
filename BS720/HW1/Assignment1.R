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



#Intro to R class 1 code
#note that the hashtag/pound symbol starts a line of comments
#that will not run if you execute those lines


2+2

# to quit, type q()



#exercise 1

# case 1 : 1+2
2+2; 2*3; 2/5
# case 2
8/2-2*(2-3)
# case 3: 
3*5  * 4   /2





#A note on machine precision
1+1.110223e-16





#exercise 2 
x <- !(5>=3)
y <- ((2^4) > (2*3))
z<- x|y
w <- x&y






#Vectors
x <- c(1.2, 2.3, 0.2, 1.1)
length(x)


x[c(2,3,4)]






#exercise 3
x <- c(1.2, 2.3, 0.2, 1.1)

x[-1]
x[2:4] 







#Vector arithmetic 
2*x+3
x[c(FALSE, TRUE, TRUE, FALSE)]
x[x>1]





#Matrices
x <- matrix(c(1,2,3,4,5,6), ncol=2)
p<- matrix(c(1,2,3,4,5,6), nrow=2)


cbind(c(1,2,3), c(4:6))
rbind(c(1,4), c(2,5), c(3,6))




#exercise 4
x <- c(-3:3)
y <- c(2, 5, -6, 3, -2, 10, -4)


mean(z[1,])




#Data Types
k<-3
k
str(k)

w <-"Homer" 
w
str(w)




#Matrices vs. Data Frames
matrix.1<- matrix(1:16,4,4)

str(matrix.1)
is.matrix(matrix.1)
is.data.frame(matrix.1)


data.1 <- as.data.frame(matrix.1)
data.1
str(data.1)



object.size(matrix.1)
object.size(data.1)




airqual <- read.table("/Users/Avery/Desktop/airquality.txt")

airqual <- read.csv("/Users/Avery/Desktop/airquality.csv")

path<-file.choose()
airqual2<-read.table(path)


bod <- read.table("/Users/Avery/Desktop/BOD.txt", header=F)
colnames(bod) <- c("Time","demand")



cars1<-read.table("/Users/Avery/Desktop/cars.txt", header=FALSE)
colnames(cars1) <- c("speed","dist")




getwd() #the default working directory where files are read/written from/to

setwd("/Users/Avery/Desktop")
write.table(cars1, file="cars1.txt", quote=F, row.names=F)




#datasets
data()
CO2

CO2[,"Type"]
CO2$Type
CO2[,2]

#all these three above commands give the same results


#exercise

mean(CO2$conc)
mean(CO2[,4])

mean(CO2$uptake)
mean(CO2[,5])

range(CO2$conc)
mean(CO2[,4])

range(CO2$uptake)
range(CO2[,5])




#Getting help
?plot
help.search("sort")
??sort

help(+) #this won't work!
help("+")



