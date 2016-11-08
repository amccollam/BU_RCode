







library("faraway", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
advisits<-dvisits
View(advisits)
summary(dvisits)
#creating testing data set.  Left in agesq in addition to requested predictor variables.
advisits<-dvisits[,1:13]
summary(advisits)

#getting an idea of the shape of the doctorco variable
hist(advisits$doctorco)
#it looks like doctorco 0 is highly overrepresented