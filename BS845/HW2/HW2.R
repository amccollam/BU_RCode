Please submit your R codes and output to the questions.
1.	Create a function that has following descriptive statistics as output for a variable:
  
???	N (Number of non-missing observations)
???	Nmiss (Number of  missing observations)
???	Mean
???	SD (standard deviation)
???	Min (Minimum value)
???	Max (Maximum value)

descriptive <- function(x){
  #assigning output values to variables
  n_nonmissing <- length(na.omit(x))
  nmiss <- length(x) - n_nonmissing
  mean_descriptive <- mean(x, na.rm=T)
  sd_descriptive <- sd (x, na.rm=T)
  min_descriptive <- min(x, na.rm=T)
  max_descriptive <- max (x, na.rm=T)
  
  #assigning output and order of values
  out <- c(n_nonmissing, nmiss, mean_descriptive, sd_descriptive, min_descriptive, max_descriptive)
  #naming output columns
  names(out) <- c("N","Nmiss","Mean","SD","Min","Max")
  #outputting values and names
  out
}


2.	Using the function created in 1., compute the descriptive statistics for TOTCHOL from the Framingham Heart Study (FHS) teaching dataset. 

descriptive(fram$TOTCHOL)

N       Nmiss        Mean          SD         Min         Max 
11218.00000   409.00000   241.16242    45.36803   107.00000   696.00000 


3.	Create an age group variable for FHS teaching data with the following categories: 30-40 (i.e. AGE>=30 and AGE<40),  40-60,  >=60.  Report the frequencies of different age categories in FHS teaching dataset.

attach(fram)
age_cat<-ifelse(AGE>=30 & AGE<40,"30-40",ifelse(AGE>=60,"60+",ifelse(AGE<30,"30-","40-60")))

ftable(age_cat)

>age_cat 30-40 40-60  60+
          566  7290 3771





Problem 4. is optional and not required to be included in your homework solutions. It is covered by the advanced materials in the class notes.
4.	Apply the function created in 1. to following variables using lapply or apply:
  AGE, SYSBP, DIABP, TOTCHOL, HDL, LDL, BMI, GLUCOSE
Reformat the results into a matrix output, and assign column names to indicate the type of descriptive statistics and row names to indicate variables.

