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



