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



# a)	Create a new function called “newage” that creates 
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





