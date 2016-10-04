Homework 4
Please submit your R codes and output, and additional answers to the questions.
For all figures, please properly label x and y axes and provide a title to the figure.
If there are multiple plotting symbols or different colors in a figure, provide a legend to 
annotate the figure.

Using the Framingham Heart Study teaching dataset,
1.	Generate a boxplot to compare the TOTALCHOL between hypertensive (HYPERTEN equal 1) 
and normal individuals. Make the box size proportional to sample size of each group. 
Compare the TOTALCHOL distributions between the two groups and comment on any similarity and difference 
between the two groups.

boxplot(TOTCHOL~HYPERTEN 
        #if varwidth is TRUE, the boxes are drawn with widths proportional to the square-roots 
        #of the number of observations in the groups.
        ,varwidth=TRUE
        ,ylab="Total Cholesterol"
        ,xlab="Hypertension No/Yes"
        ,main="Cholesterol Levels by Hypertensive Status"
        )


2.	CO2 is an R internal dataset with the following variables:
  Plant: an ordered factor with levels Qn1 < Qn2 < Qn3 < ... < Mc1 giving a unique identifier for each plant. 
Type: a factor with levels Quebec Mississippi giving the origin of the plant 
Treatment: a factor with levels nonchilled chilled 
conc:a numeric vector of ambient carbon dioxide concentrations (mL/L). 
uptake:a numeric vector of carbon dioxide uptake rates (umol/m^2 sec). 

Please generate plant specific curves to display the relationship between  
conc (response) and uptake (predictor) in four panels by Type and Treatment combinations 
using ggplot2 package. 
