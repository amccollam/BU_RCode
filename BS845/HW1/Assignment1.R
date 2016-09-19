# Please submit R codes and outputs for each problem.

# 1.	Read the Framingham Heart Study Teaching Dataset (fram.csv) into R. 
# a.	What is the total number of rows and of columns of this dataset?

attach(fram)
dim(fram)
>[1] 11627    38
#The Framingham Heart Study Teaching Dataset has 11,627 rows and 38 columns.
#This information is also given in R Studio in the Environment window.

# b.	Count the total number of non-missing values in total cholesterol (TOTCHOL).

NoNA_TOTCHOL<-fram[which(!is.na(TOTCHOL)),"TOTCHOL"]
length(NoNA_TOTCHOL)
>[1] 11218
#There are 11,218 rows in the dataset that have non-missing values in the TOTCHOL column.

# c.	Compute the mean, minimum, maximum and standard deviation of TOTCHOL.

summary(TOTCHOL)
>Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
>107.0   210.0   238.0   241.2   268.0   696.0     409 

sd(NoNA_TOTCHOL)
[1] 45.36803

#TOTCHOL mean is 241.2, minimum is 107.0, maximum is 696.0, and standard deviation is 45.37.        

# d.	Participants are identified by their randomized ID numbers (RANDID). What is the total number of participants in this dataset? What is the average number of observations for a subject?

length(unique(RANDID))
>[1] 4434
length(RANDID)/length(unique(RANDID))
>[1] 2.622237

#There are 4,434 unique participants in this dataset.  On average, each participant has been observed 2.62 times. 


# e.	Select participants who were diabetic (DIABETES), hypertensive (HYPERTEN), had CVD (CVD), with BMI 27 or higher and aged 50 or younger;  export data for these participants with following variables to a coma (or tab) delimited plain text file, with variable names in the first line in such order:
  # RANDID, TOTCHOL, AGE, SEX, BMI, GLUCOSE, DIABETES, SYSBP, DIABP, HYPERTEN, CVD. 

eParticipants<-fram[DIABETES==1 & HYPERTEN==1 & CVD==1 & BMI>=27 & AGE<=50,]
write.table(eParticipants[,c("RANDID","TOTCHOL","AGE","SEX","BMI","GLUCOSE","DIABETES","SYSBP","DIABP","HYPERTEN","CVD")], file="framHW1.txt",quote=F, sep=",", na = "", row.names=F)

<framHW1.txt>
RANDID,TOTCHOL,AGE,SEX,BMI,GLUCOSE,DIABETES,SYSBP,DIABP,HYPERTEN,CVD
3830539,231,43,2,34.95,274,1,155.5,99.5,1,1
3830539,,49,2,36.52,,1,175,105,1,1
4327409,290,45,2,38.42,69,1,165,96,1,1
5444736,301,49,2,41.51,180,1,155,100,1,1
5856634,233,41,1,32.56,87,1,126,79,1,1
5856634,310,47,1,35.52,74,1,123,76,1,1
6764667,172,48,1,35.12,108,1,131,79,1,1
7307274,210,47,1,28.24,183,1,163.5,97,1,1
7356485,284,46,2,33.81,,1,137,88,1,1
8538782,189,45,1,28.4,177,1,132,78,1,1
8962433,186,47,2,27.9,125,1,139,85,1,1
9036299,296,47,1,28.5,332,1,141,93,1,1
9485907,208,44,1,27.93,193,1,175,101,1,1
9964282,236,49,1,33.78,144,1,129,91,1,1
9969773,260,50,2,43.67,260,1,190,130,1,1


# f.	Select observations for participants with RANDID being the following 
# 9771959, 6461117, 5422551, 3026761, 4696586, 7414089, 2024096, 7561936, 3541454, 4409179

group<-c(9771959, 6461117, 5422551, 3026761, 4696586, 7414089, 2024096, 7561936, 3541454, 4409179)
fram[RANDID %in% group,c(1:5)]

      SEX  RANDID TOTCHOL AGE SYSBP
2406    2 2024096     200  57 108.0
2407    2 2024096     245  63 113.0
3497    2 3026761     225  53 125.0
3498    2 3026761     276  59 150.0
3499    2 3026761     223  65 150.0
4147    2 3541454     220  36 125.0
4148    2 3541454     290  42 127.0
4149    2 3541454     278  48 127.0
5146    2 4409179     290  40 125.0
5147    2 4409179     287  46 134.0
5148    2 4409179     266  52 152.5
5469    1 4696586      NA  51 112.5
5470    1 4696586     274  57 106.0
5471    1 4696586      NA  63 112.0
6277    1 5422551     336  48 183.0
6278    1 5422551     328  54 193.0
6279    1 5422551     241  60 181.0
7436    1 6461117     215  38 110.0
7437    1 6461117     219  43 107.5
8645    2 7414089     316  60 212.0
8646    2 7414089     253  66 188.0
8647    2 7414089     227  71 180.0
8811    2 7561936     260  61 137.0
8812    2 7561936     274  67 130.0
8813    2 7561936     249  73 166.5
11387   2 9771959     257  36 103.0
11388   2 9771959     268  42 111.5
11389   2 9771959     260  48 133.0

