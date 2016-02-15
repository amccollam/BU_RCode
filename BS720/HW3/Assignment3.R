#HW3 BS720
#a)	Create a data frame called “ageweight” in which the 
#first column is age and the second column is weight. (20pts)

#read in data
age = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
weight = c(30, 55, 105, 110, 115, 140, 190, 170, 120, 128, 165, 132, 174, 201, 133, 164)

#confirmed both variables have 16 values
length(age)
length(weight)

#converted into data frame
ageweight<-data.frame(age,weight)



# b)	Create a new function called “newage” that creates 
#a new variable called age_cat variable that is defined as 
#follows:
#   1 if age is 20 or less
# 2 if 20<age≤50
# 3 otherwise

#this  will involve figuring out a nested ifelse, 
#and then creating a new function
#remember to use <=, >=
#can use age<=20 in first ifelse, 
#then stack a age <=50 on top
#including a print() step at the end to check output.

newage <- function(age) {
        age_cat<- ifelse(age<=20,"1",
                          ifelse(age<=50,"2","3"
                                 )
                         )
        print(age_cat)
}



# c)	Use the “newage” function on the age variable in the 
# ageweight dataset to create the age_cat variable. (15pts)

#"ordinary assignments done within the function are local 
#and temporary and are lost after exit from the function" 
#(Venables,Smith,2015) 
#That is, age_cat must be assigned outside of the function 
#to become part of the working environment.

age_cat<-newage(age=age)

 
# 2.
# a. Read in the fram_heart dataset. (10pts)

fram_heart<-read.csv(file.choose())

#b Use conditional indexing to replace the missing values 
# (999,-99,9) for the weight, height, diabetes and myocardial 
# infarction variables with NA.  (Hint: this technique was done 
# for the months in the airquality dataset in the inclasscode_3.R 
# script. Replace missing values (99,-99,999) with NA.) (20pts)

#First, ran descriptive statistics to get the feel of the data and 
#double check the null values.
summary(fram_heart[,1:13])
stem(fram_heart$height)
stem(fram_heart$weight)
stem(fram_heart$mi)
stem(fram_heart$dia)

#Discovered that height null value is actually -9, not 9
#$height null = -9
#$weight null = -99
#$mi null = 999
#$dia null = 999
fram_heart_copy<-fram_heart

#this indexes all null height values
null_height_index<-which(fram_heart$height==-9)
#this moves the null height rows into a new table
null_height_fram_heart<-fram_heart[null_height_index,]

for(i in ncol(null_height_index)){
  fram_heart$height<-NA
}


#this isn't working
fram_heart_testing<-ifelse(fram_heart[null_height_index,],NA,fram_heart$height)




#3.	Create a dataset called “cardio” which only contains the 
# subset of participant from the fram_heart dataset who had any 
# of the cardiovascular risk factors (smoke, hypertension, and 
# hypercholesterolemia).  (Hint: You can either use an ifelse 
# statement with multiple conditions OR you can first use the 
# apply statement to sum the smoke, hypertension and 
# hypercholesterolemia variables and then take only those where 
# the sum is greater than 1). (20pts)

#Relevant columns (4, 5, 6):
fram_heart$smoke
fram_heart$htn
fram_heart$hca

#verified fram_heart data structure
str(fram_heart)

cv_risk<-sum(fram_heart$smoke,
             fram_heart$htn,
             fram_heart$hca)

cv_risk<-apply(fram_heart[,4:6],sum)

#this returned a single value 0
cardio_risk<-ifelse(fram_heart$smoke==0 && fram_heart$htn==0 && fram_heart$hca==0,"0","1")
#this returned an array. Adding as.integer got rid of ""
cardio_risk_ifelse<-as.integer(ifelse(fram_heart$smoke==0 & fram_heart$htn==0 & fram_heart$hca==0,"0","1"))



#this one worked
cardio_risk<-apply(fram_heart[,c(4:6)],1,sum)
has_cardio_risk<-which(cardio_risk>0)
cardio<-fram_heart[has_cardio_risk,]

