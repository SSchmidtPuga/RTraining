
# First import the dataset
library(readxl)
X3_discriminant_blank <- read_excel("3_discriminant_blank.xlsx", 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric"))
# View the Database

View(X3_discriminant_blank)
library(tidyverse)

# Split the database in 2 -> 0 = Risky Companies, 1 = Good Companies 

db_1 <-  X3_discriminant_blank[-c(25:38 ), ]
View(db_1)

db_0 <-  X3_discriminant_blank[c(25:38 ), ]
View(db_0)

# Discriminant Analysis, we want to know how much each of the variables affect to the Credit risk
# That we we cant in the future know if it is recommended give a credit to another company.

#How we construct the credit score?
#X$A and X$B vectors containing the mean values of the n independent variables for the group of healthy and abnormal companies, respectively, and Σ the matrix of variances and covariances
#1) We need the mean of each column, in this example we have  vector Xa and the Vector Xb
Mean_x1_db1 <- mean(db_1$`x1: interest expenses over turnover`)
Mean_x2_db1 <- mean(db_1$`x2: unauthorised overdrafts over total credit exposure`)

Mean_x1_db0 <- mean(db_0$`x1: interest expenses over turnover`)
Mean_x2_db0 <- mean(db_0$`x2: unauthorised overdrafts over total credit exposure`)


vector_db1 <- c(Mean_x1_db1,Mean_x2_db1)
vector_db0 <- c(Mean_x1_db0,Mean_x2_db0)


#2) Need the variance and co variance of each colum
VarianceX1_db1 <- var(db_1$`x1: interest expenses over turnover`)
VarianceX2_db1 =  var(db_1$`x2: unauthorised overdrafts over total credit exposure`)

VarianceX1_db0 <- var(db_0$`x1: interest expenses over turnover`)
VarianceX2_db0 =  var(db_0$`x2: unauthorised overdrafts over total credit exposure`)

Covar_db1 <- cov(db_1$`x1: interest expenses over turnover`, db_1$`x2: unauthorised overdrafts over total credit exposure`)
Covar_db0 <- cov(db_0$`x1: interest expenses over turnover`, db_0$`x2: unauthorised overdrafts over total credit exposure`)

#Create a Matrix for each column
Matrix_Variance_db1 <- matrix(c(VarianceX1_db1,Covar_db1,Covar_db1,VarianceX2_db1),nrow = 2, ncol= 2, byrow = TRUE)
Matrix_Variance_db1


Matrix_Variance_db0 <- matrix(c(VarianceX1_db0,Covar_db0,Covar_db0,VarianceX2_db0),nrow = 2, ncol= 2, byrow = TRUE)
Matrix_Variance_db0

#with 𝑛#, 𝑛$ the number of companies in the two groups and Σ#, Σ$ the matrices of variances and covariances for each group of companies

N_db1 <- (24-1)/(24+14-2)

N_db0 <- (14-1)/(24+14-2)

#COMBINE EVERYTHING TO HAVE THE VARIANCE 

Varianza_total = N_db1*Matrix_Variance_db1 + N_db0*Matrix_Variance_db0
Varianza_total

Varianza_total_inversa = solve(Varianza_total)
Varianza_total_inversa


#calculate the credit score
Y = Varianza_total_inversa%*%(vector_db0 - vector_db1 )
Y
New_db <- X3_discriminant_blank
New_db$Score <- X3_discriminant_blank$`x1: interest expenses over turnover` * Y[c(1 ), ] + X3_discriminant_blank$`x2: unauthorised overdrafts over total credit exposure`*Y[c(2 ), ]
view(New_db)


#Know we have to calculate which it the cut-off point where we decide if a company is risky
# We want a cut-off point 𝛼 below which a company is rejected because it is too risky:
# We could use the point halfway between the two vectors X#a,X$b:

Alpha  <- (t(vector_db1 +vector_db0 )%*% Y) * 0.5
Alpha[c(1)]
New_db$CutOff1_Alpha_1 <- Alpha[c(1)]



#Probability that a company i is abnormal
Probability_past_default <- 0.37
#(1-Probability_past_default/Probability_past_default) +1
Calculation <- (1-Probability_past_default)/Probability_past_default
Calc_2 <- Calculation*exp(New_db$Score - Alpha[c(1)])
Calc_3 <- 1+Calc_2


New_db$Probability_of_not_paying <- 1/Calc_3
View(New_db)

CutOff2 <- Alpha[c(1)] +log(Probability_past_default/(1-Probability_past_default))
CutOff2
New_db$CutOff2_Alpha_2  <- CutOff2
View(New_db)



#The cost of errors
#Type I error: classifying an insolvent company as healthy
#Type II error: classifying a healthy company as insolvent
# Let C(𝐴/𝐵)  be the cost of type I error and C(𝐵/𝐴) the cost of type IIerror


#To account for the cost of errors, a bank could decide to refuse a loan to a customer whenever
#the expected cost of a type I error (cost of error weighted by its probability)
#exceeds the expected cost of a type II error

Probability_type_I <- 0.7 
Probability_type_2 <- 0.02

error_type_cut_off <- CutOff2 + log(Probability_type_I/Probability_type_2)

New_db$CutOff3_Alpha_3 <- error_type_cut_off
view(New_db)


