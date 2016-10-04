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

#loaded ggplot2 through Packages interface of R Studio
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
attach(CO2)

Miss_Chilled<-CO2[CO2$Type==c("Mississippi") & CO2$Treatment==c("chilled"),c("conc","uptake", "Plant")]
Quebec_Non<-CO2[Type==c("Quebec") & Treatment==c("nonchilled"),c("conc","uptake","Plant")]
Miss_Non<-CO2[Type==c("Mississippi") & Treatment==c("nonchilled"),c("conc","uptake", "Plant")]
Quebec_Chilled<-CO2[Type==c("Quebec") & Treatment==c("chilled"),c("conc","uptake", "Plant")]

p_Miss_Chilled <- qplot(Miss_Chilled$uptake,Miss_Chilled$conc
                        ,xlab="CO2 Uptake Rate (umol/m^2 sec)"
                        ,ylab="CO2 Concentration (mL/L)"
                        ,main="Conc as measured by Plant CO2 Uptake for Chilled Miss.") +
  geom_smooth(method="lm",se=F)
p_Miss_Non <- qplot(Miss_Non$uptake,Miss_Non$conc
                        ,xlab="CO2 Uptake Rate (umol/m^2 sec)"
                        ,ylab="CO2 Concentration (mL/L)"
                        ,main="Conc as measured by Plant CO2 Uptake for Nonchilled Miss.") +
  geom_smooth(method="lm",se=F)
p_Quebec_Chilled <- qplot(Quebec_Chilled$uptake,Quebec_Chilled$conc
                        ,xlab="CO2 Uptake Rate (umol/m^2 sec)"
                        ,ylab="CO2 Concentration (mL/L)"
                        ,main="Conc as measured by Plant CO2 Uptake for Chilled Quebec") +
  geom_smooth(method="lm",se=F)
p_Quebec_Non <- qplot(Quebec_Non$uptake,Quebec_Non$conc
                          ,xlab="CO2 Uptake Rate (umol/m^2 sec)"
                          ,ylab="CO2 Concentration (mL/L)"
                          ,main="Conc as measured by Plant CO2 Uptake for Nonchilled Quebec") +
  geom_smooth(method="lm",se=F)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p_Miss_Chilled, p_Miss_Non, p_Quebec_Chilled, p_Quebec_Non, cols=2)



