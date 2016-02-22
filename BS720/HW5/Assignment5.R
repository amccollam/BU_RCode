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
