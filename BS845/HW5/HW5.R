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
i.	Compute row proportions, and comment on how percent of cases differs across different education group.
ii.	Perform a test of whether infertility depends on education 
iii.	Report odds ratio (OR) as a measure of the association and provide an interpretation of the OR.
b.	Repeat all the analyses in a. to study the relationship between induced and case.
