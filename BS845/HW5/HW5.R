BS845                                                Homework 5
Please submit your R codes and output, and additional answers to the questions.
For all figures, please properly label x and y axes and provide a title to the figure.
If there are multiple plotting symbols or different colors in a figure, provide a legend to annotate the figure.
For all hypothesis testing, state null and alternative hypotheses and a 1-2 sentence conclusions based on test results. 

1.	UCBAdmission is a three-way table from cross-tabulating three variables.
The three variables are listed with dimnames(UCBAdmissions) 
Using the follow command, we can produce a two-way table based on the three way table for admission and gender by combining admission data of all the departments:  
  margin.table(UCBAdmissions,c(1,2))
Gender
Admit      Male Female
Admitted 1198    557
Rejected 1493   1278

a.	Report proportions of admission in male and female applicants repectively and their respective 95% confidence interval
b.	Perform a hypothesis testing to examine whether there is gender difference in admission rates.
2.	Using R internal dataset infert, 
a.	Produce a marginal 2-way table of education vs. case (case=1, infertility; case=0, otherwise)
i.	Compute row proportions, and comment on how percent of cases differs across different education group.
ii.	Perform a test of whether infertility depends on education 
iii.	Report odds ratio (OR) as a measure of the association and provide an interpretation of the OR.
b.	Repeat all the analyses in a. to study the relationship between induced and case.
