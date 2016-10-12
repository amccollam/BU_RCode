BS845                                                Homework 5
Please submit your R codes and output, and additional answers to the questions.
For all figures, please properly label x and y axes and provide a title to the figure.
If there are multiple plotting symbols or different colors in a figure, provide a legend to annotate the figure.
For all hypothesis testing, state null and alternative hypotheses and a 1-2 sentence conclusions based on test results. 

1.	UCBAdmission is a three-way table from cross-tabulating three variables.
The three variables are listed with dimnames(UCBAdmissions) 
Using the follow command, we can produce a two-way table based on the three way table for admission and gender by 
combining admission data of all the departments:  
  margin.table(UCBAdmissions,c(1,2))
Gender
Admit      Male Female
Admitted 1198    557
Rejected 1493   1278

a.	Report proportions of admission in male and female applicants repectively and their respective 95% confidence interval
#creating margin table
AdmitGender<-margin.table(UCBAdmissions,c(1,2))

#Creating a prop.table by column so each gender sums to 1.
prop.table(AdmitGender,2)

Gender
Admit           Male    Female
Admitted 0.4451877 0.3035422
Rejected 0.5548123 0.6964578

#measuring proportion of accepted female applicants.
prop.test(557,(557+1278),conf.level = .95)

1-sample proportions test with continuity correction

data:  557 out of (557 + 1278), null probability 0.5
X-squared = 282.51, df = 1, p-value < 2.2e-16
alternative hypothesis: true p is not equal to 0.5
95 percent confidence interval:
  0.2826669 0.3252490
sample estimates:
  p 
0.3035422 

#measuring proportion of accepted male applicants.
prop.test(1198,(1198+1493),conf.level = .95)

1-sample proportions test with continuity correction

data:  1198 out of (1198 + 1493), null probability 0.5
X-squared = 32.12, df = 1, p-value = 1.449e-08
alternative hypothesis: true p is not equal to 0.5
95 percent confidence interval:
  0.4263168 0.4642163
sample estimates:
  p 
0.4451877 


b.	Perform a hypothesis testing to examine whether there is gender difference in admission rates.

#H0: The proportion of admitted female students and male students is equal
#H1: The proportion of admitted female students and male students is not equal
#First, we can reject H0 at 95% confidence level because the 95% CIs don't overlap. 
prop.test(557,(557+1278),p=(1198/(1198+1493)))

1-sample proportions test with continuity correction

data:  557 out of (557 + 1278), null probability (1198/(1198 + 1493))
X-squared = 148.48, df = 1, p-value < 2.2e-16
alternative hypothesis: true p is not equal to 0.4451877
95 percent confidence interval:
  0.2826669 0.3252490
sample estimates:
  p 
0.3035422 



2.	Using R internal dataset infert, 
a.	Produce a marginal 2-way table of education vs. case (case=1, infertility; case=0, otherwise)

#exploring the data
infert
str(infert)
'data.frame':	248 obs. of  8 variables:

#converting to a table
InfertTable<-table(infert$case,infert$education)
InfertTable

0-5yrs 6-11yrs 12+ yrs
0      8      80      77
1      4      40      39

#verified data adds to 248

i.	Compute row proportions, and comment on how percent of cases differs across different education group.

#computed column proportions to check differences across education groups
prop.table(InfertTable,2)

0-5yrs   6-11yrs   12+ yrs
0 0.6666667 0.6666667 0.6637931
1 0.3333333 0.3333333 0.3362069

#across all education levels, approximately 1/3 of people are identified as having infertility.

ii.	Perform a test of whether infertility depends on education 

The odds ratio below displays that we are over 95% confident that there is no relationship between infertility and education.

iii.	Report odds ratio (OR) as a measure of the association and provide an interpretation of the OR.

library("epitools", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
#oddsratio function requires r x 2 dimension table. Rotating the table
InfertTableOR<-table(infert$education,infert$case)

oddsratio(InfertTableOR)

$data

0  1 Total
0-5yrs    8  4    12
6-11yrs  80 40   120
12+ yrs  77 39   116
Total   165 83   248

$measure
odds ratio with 95% C.I.
estimate     lower    upper
0-5yrs  1.0000000        NA       NA
6-11yrs 0.9829708 0.2844553 4.010957
12+ yrs 0.9956791 0.2875998 4.068890

$p.value
two-sided
midp.exact fisher.exact chi.square
0-5yrs          NA           NA         NA
6-11yrs  0.9791352            1  1.0000000
12+ yrs  0.9947485            1  0.9839929

$correction
[1] FALSE

attr(,"method")
[1] "median-unbiased estimate & mid-p exact CI"


#using the 0-5 yrs group as the reference group, people with 6-11 yrs of education have .983 times the risk of infertility compared with 
#people with 0-5 years of education with a 95% confidence interval of (0.284,4.011).  People with 12+ years of education have .996 times the 
#risk of infertility than people with 0-5 years of education with a 95% confidence interval of (0.288,4.069).  Since both of these confidence
#intervals contain the null value of 1.0, we fail to reject the null hypothesis that there is no difference in odds of infertility across
#these groups.
#Since one group has fewer than 5 observed values (education=1-5yrs, case=1), the Fisher exact values should be considered as well.

fisher.test(InfertTableOR)

Fisher's Exact Test for Count Data

data:  InfertTableOR
p-value = 1
alternative hypothesis: two.sided

#This test also fails to reject the null hypothesis that there is no difference in odds of infertility across education levels.


b.	Repeat all the analyses in a. to study the relationship between induced and case.

InducedCaseOR<-table(infert$induced,infert$case)
InducedCaseOR

  0  1
0 96 47
1 45 23
2 24 13

prop.table(InducedCaseOR,1)

          0         1
0 0.6713287 0.3286713
1 0.6617647 0.3382353
2 0.6486486 0.3513514

oddsratio(InducedCaseOR)

$data

0  1 Total
0      96 47   143
1      45 23    68
2      24 13    37
Total 165 83   248

$measure
odds ratio with 95% C.I.
estimate     lower    upper
0 1.000000        NA       NA
1 1.045600 0.5603142 1.923131
2 1.110326 0.5047799 2.358710

$p.value
two-sided
midp.exact fisher.exact chi.square
0         NA           NA         NA
1  0.8870238    1.0000000  0.8903248
2  0.7894580    0.8457246  0.7942121

$correction
[1] FALSE

attr(,"method")
[1] "median-unbiased estimate & mid-p exact CI"
