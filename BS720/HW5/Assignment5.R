#1.	Use the R dataset airquality to assess the relationship between 
#the Ozone and the Temp variables. 

# a) Use a histogram to assess normality of the Ozone and the Temp 
# variables. Are the two variables normally distributed? Output the 
# histogram as a .png file and include it in your homework. You do 
# not need to perform a formal test of normality for this question. 

head(airquality)
str(airquality)
attach(airquality)

png(filename="OzoneTemp.png")
par(mfrow=c(2,1));
hist(Ozone, main="Mean Ozone at Roosevelt Island", xlab="Ozone ppb");
hist(Temp, main="Maximum Daily Temperature at La Guardia Airpot", xlab="Temp F")
dev.off()

# b) Use a scatterplot to assess the linearity assumption between the 
# Ozone and the Temp variables. Do the two variables appear to have a 
# linear trend? Output the scatterplot as a .png file and include it 
# in your homework. 


dev.off()
png(filename="OzoneTemp_scatter.png")
plot(Ozone~Temp, main="Ozone Against Temp",ylab="Ozone (ppb)",xlab="Temp (F)")


# c) Compute the correlation between Ozone and Temp using both the 
# Pearson and Spearman methods. Based on parts a and b, which measurement 
# of correlation would you choose? Write a brief report of the results 
# based on the method of correlation you feel is most appropriate. 

cor.test(Temp, Ozone)
cor.test(Temp, Ozone, method="spearman")


# 1.	Thus far we have used the cor() and cor.test() functions to compute 
# correlations in R; however, these functions will only compute pairwise 
# correlations. In order to compute a correlation matrix for all specified 
# variables in a dataset, one can use the rcorr function in the Hmisc package 
# in R. 
# 	a) Install the Hmisc package in R. 

install.packages("Hmisc")

#b) Call the Hmisc package in R using the library() command. 

library(Hmisc)

# c) Use the help function on the rcorr command to generate a correlation 
# matrix for all variables in the airquality dataset (for the purposes of 
# this question, just use the Pearson method). **Hint: you must coerce your 
# data frame into a matrix for the function to work. 

?rcorr
airqual_matrix<-as.matrix(airquality)
rcorr(airqual_matrix, type=c("pearson"))

# d) Summarize which variables have a significant correlation and whether 
# they are positively or negatively correlated with each other. 




# 1.	Create a linear regression model in R using the airquality dataset 
# based on the following model: 
#   𝑂𝑧𝑜𝑛𝑒! = 𝛽! + 𝛽! ∗𝑆𝑜𝑙𝑎𝑟! + 𝛽! ∗𝑊𝑖𝑛𝑑! + 𝛽! ∗𝑇𝑒𝑚𝑝! + 𝛽! ∗𝑀𝑜𝑛𝑡h! + 𝛽! ∗𝐷𝑎𝑦! + 𝜖! 

attach(airquality)
Ozone_model<-lm(Ozone~Solar.R+Wind+Temp+Month+Day, data=airquality)
summary(Ozone_model)


# b) Test the assumptions of linearity, homoscedasticity and normality 
# using the plots in R. Are all of the assumptions met? Please provide a 
# full explanation for each assumption tested. 

#Running residual plots
par(mfrow=c(2,2))
plot(Day)
hist(Day, main= "Day of Month")
qqnorm(Day, main = "QQ-plot for Day of Month")
qqline(Day)
boxplot(Day, main="Boxplot of Day of Month")








