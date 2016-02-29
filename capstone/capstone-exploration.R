#R-Scripts exploring Capstone Dataset

############################################################
# install packages
############################################################
library("ggplot2", lib.loc="~/R/win-library/3.2")
install.packages("ggplot2")

library("gridExtra", lib.loc="~/R/win-library/3.2")
install.packages("gridExtra")

library("dplyr", lib.loc="~/R/win-library/3.2")
install.packages("dplyr")

library("tidyr", lib.loc="~/R/win-library/3.2")
install.packages("tidyr")

library("stringr", lib.loc="~/R/win-library/3.2")
install.packages("stringr")

############################################################
# 1. set working directory and load data
############################################################
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/capstone')

# create the loan dataframe from the loan.csv data file
loan  <- read.csv ("loan.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)

str (loan)

# Description of Variables:
# Source: https://www.lendingclub.com/info/download-data.action
###########################################################################################################
# bad_loan     
# loan_amnt	  The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
# term	      The number of payments on the loan. Values are in months and can be either 36 or 60.
# int_rate
# emp_length	Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years. 
# home_ownership	The home ownership status provided by the borrower during registration. Our values are: RENT, OWN, MORTGAGE, OTHER.
# annual_inc	The self-reported annual income provided by the borrower during registration.
# purpose	    A category provided by the borrower for the loan request. 
# addr_state	The state provided by the borrower in the loan application
# dti	        A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income.
# delinq_2yrs	The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years
# revol_util	Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
# longest_credit_length

#############################################################################################
# 2. Wrangle: Convert data to numeric or factors where needed
#############################################################################################

# variables need to be numeric (e.g. "36 months" to 36)
loan$term <- as.vector(loan$term)
loan$term <- regmatches(loan$term, regexpr("[[:digit:]]+", loan$term))
loan$term <- as.numeric(loan$term)


# bad_loan needs to be a factor for CART
loan$bad_loan <- as.factor(loan$bad_loan)


#############################################################################################
# 3. Split the dataset into training and the test sets.
#############################################################################################

## 75% of the sample size
smp_size <- floor(0.75 * nrow(loan))

## set the seed to make partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(loan)), size = smp_size)

loan_train <- loan [train_ind, ]
loan_test <- loan [-train_ind, ]



#############################################################################################
# 4. Graphic Exploration
#############################################################################################


qplot(x=loan_amnt,y=annual_inc,data=loan)

# sample: how do higher interest rates vs. loan amount affect bad loans?
ggplot (aes(x=int_rate,y=loan_amnt),data=loan) +
  geom_jitter(alpha = 1/10, aes(color = loan$bad_loan)) +
  coord_cartesian(ylim = c(0, 40000))



#################################################################################################
# 5. Models: Model 1 - Logistic Regression Model
#################################################################################################


################################################################# 
## since we are trying to detemine either loan or no loan, which
## we will use a logistic regression model
#################################################################

# 5a) Creatig the Bivariate Models
# Build Bivariate models to identify which of our variables are useful in predicting a 
# particular outcome (e.g. models that predict the outcome using a single independent variable.) 
# An independent variable shall be considered significant if there is at least one star at 
# the end of the coefficients row for that variable (e.g. the probability column having a value smaller than 0.05)

LoanModBi01 = glm (bad_loan ~ loan_amnt, data=loan_train, family="binomial")
LoanModBi02 = glm (bad_loan ~ term, data=loan_train, family="binomial")
LoanModBi03 = glm (bad_loan ~ int_rate, data=loan_train, family="binomial")
LoanModBi04 = glm (bad_loan ~ emp_length, data=loan_train, family="binomial")
LoanModBi05 = glm (bad_loan ~ home_ownership, data=loan_train, family="binomial")
LoanModBi06 = glm (bad_loan ~ annual_inc, data=loan_train, family="binomial")
LoanModBi07 = glm (bad_loan ~ purpose, data=loan_train, family="binomial")
LoanModBi08 = glm (bad_loan ~ addr_state, data=loan_train, family="binomial")
LoanModBi09 = glm (bad_loan ~ dti, data=loan_train, family="binomial")
LoanModBi10 = glm (bad_loan ~ delinq_2yrs, data=loan_train, family="binomial")
LoanModBi11 = glm (bad_loan ~ revol_util, data=loan_train, family="binomial")
LoanModBi12 = glm (bad_loan ~ total_acc, data=loan_train, family="binomial")
LoanModBi13 = glm (bad_loan ~ longest_credit_length, data=loan_train, family="binomial")
LoanModBi14 = glm (bad_loan ~ verification_status, data=loan_train, family="binomial")

summary(LoanModBi02)

# Create a matrix containing only the Variable, its P-Value, and the "significance"
# for the numeric variables

Bino_Results_numeric <- matrix (nrow=11, ncol =3)
colnames(Bino_Results_numeric) <- c("Variable","P-Value", "significance")
ranges <- c(0, 0.001, 0.01, 0.05, 0.1)
codes <- c("***" , "**","*", ".", " ")

Bino_Results_numeric [1,] <- c("Loan_amount", coef(summary(LoanModBi01))[2,"Pr(>|z|)"],"")
Bino_Results_numeric [1,3] <- codes[findInterval(Bino_Results_numeric[1,2], ranges)]

Bino_Results_numeric [2,] <- c("term", coef(summary(LoanModBi02))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [2,3] <- codes[findInterval(Bino_Results_numeric[2,2], ranges)]

Bino_Results_numeric [3,] <- c("int_rate", coef(summary(LoanModBi03))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [3,3] <- codes[findInterval(Bino_Results_numeric[3,2], ranges)]

Bino_Results_numeric [4,] <- c("emp_length", coef(summary(LoanModBi04))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [4,3] <- codes[findInterval(Bino_Results_numeric[4,2], ranges)]

Bino_Results_numeric [5,] <- c("home_ownership", coef(summary(LoanModBi05))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [5,3] <- codes[findInterval(Bino_Results_numeric[5,2], ranges)]

Bino_Results_numeric [6,] <- c("annual_inc", coef(summary(LoanModBi06))[2,"Pr(>|z|)"],"")
Bino_Results_numeric [6,3] <- codes[findInterval(Bino_Results_numeric[6,2], ranges)]

Bino_Results_numeric [7,] <- c("dti", coef(summary(LoanModBi09))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [7,3] <- codes[findInterval(Bino_Results_numeric[7,2], ranges)]

Bino_Results_numeric [8,] <- c("delinq_2yrs", coef(summary(LoanModBi10))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [8,3] <- codes[findInterval(Bino_Results_numeric[8,2], ranges)]

Bino_Results_numeric [9,] <- c("revol_util", coef(summary(LoanModBi11))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [9,3] <- codes[findInterval(Bino_Results_numeric[9,2], ranges)]

Bino_Results_numeric [10,] <- c("total_acc", coef(summary(LoanModBi12))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [10,3] <- codes[findInterval(Bino_Results_numeric[10,2], ranges)]

Bino_Results_numeric [11,] <- c("longest_credit_length", coef(summary(LoanModBi13))[2,"Pr(>|z|)"], "")
Bino_Results_numeric [11,3] <- codes[findInterval(Bino_Results_numeric[11,2], ranges)]

# Non-Numeric Variables
#Bino_Results_numeric [2,] <- c("verification_status", coef(summary(LoanModBi14))[2,"Pr(>|z|)"], ")
#Bino_Results_numeric [2,] <- c("purpose", coef(summary(LoanModBi07))[2,"Pr(>|z|)"],"")
#Bino_Results_numeric [2,] <- c("addr_state", coef(summary(LoanModBi08))[2,"Pr(>|z|)"],"")

# Display complete matrix including Significance labels
Bino_Results_numeric


# Conclusion: from the above, the following variables appear significant to predicting 
# a bad_loan: loan_amnt, term60 months, int_rate, annual_inc, "some of" purpose, "some of" state, 
#   dti, delinq_2yrs, revol_util, longest_credit_length, verification_status, total_acc
# 
#  the following two are not significant: emp_length, home_ownership


# New model with only signficant variables

LoanModBi00 = glm (bad_loan ~ loan_amnt + term + int_rate + annual_inc +
                   dti + delinq_2yrs + revol_util 
                   + total_acc + longest_credit_length, 
                   data=loan_train, family="binomial")

summary(LoanModBi00)

# the model coefficients in more detail
coef(summary(LoanModBi00))[2,"Pr(>|z|)"]


###########################################################################################
# 5 b) Multi-variate Models for bad loans

# Often, variables that were significant in bivariate models are no longer significant in 
# multivariate analysis due to correlation between the variables.

# Evaluating the variable pairs to determine if any have a high degree of correlation 
# (a correlation greater than 0.8 or less than -0.8):

# compute all pair-wise correlations between numeric variables with:

cor(loan_train[c("loan_amnt", "term", "int_rate", "annual_inc", 
           "dti", "delinq_2yrs", "revol_util", "total_acc", "longest_credit_length")])

# Question 1 for Arinban : why do I get so many NAs?
# Question 2 for Arinban: Interest rate and Term seems correlated, but not so much that it would 
# be a problem. Yes?

# note, the following variables have been removed and will be analyzed separately below: 
# "purpose", "addr_state", "verification_status"

##################################################################################
# 5 c): chi-squared tests information:
##################################################################################
#
# http://stackoverflow.com/questions/3571909/calculate-correlation-cor-for-only-a-subset-of-columns
# http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
# http://stackoverflow.com/questions/26728197/error-creating-chisq-test-in-r-invalid-type-character-of-argument

# chi squared tests for the character variables
chisq.test(loan_train$bad_loan,loan_train$purpose)$p.value
chisq.test(loan_train$bad_loan,loan_train$addr_state)$p.value
chisq.test(loan_train$bad_loan,loan_train$verification_status)$p.value

# results: none of these three variables have P > 0.8

# Question 3 for Arinban : how would I incorporate them into the model?

######################################################################
### 5 d) Binary Classification Confusion Matrix
### Shows us the number of correct and incorrect predictions
### make by the classification moeel compared to the 
### actual outcomes (e.g. test dataset)
######################################################################

# TP = True Positive    : predicted positive, test positive
# FP = False Positive   : predicted positive, test negative
# TN = True Negative    : predicted negative, test positive
# FN = False Negative   : predicted negative, test negative 

# For example, the false positives, or FP, are the number of data points for which we 
# predict bad loans, but they're actually good candidates. 
# And the false negatives, or FN, are the number of data points for which we predict good candidates, 
# but they're actually poor loan candidates.


# accuracy on Postiive Class (true positive rate)
# TP_Rate = TP/(TP+FN)
# 1 - accuracy on Positive Class
# TN_Rate = FP/(FP+TN) = 1 - TP_Rate

predictTest = predict(LoanModBi00, type="response", newdata=loan_test)
summary(predictTest)

# Since this customer is a bank, we'll assume the prefernce is to minimize the number of 
# bad loans, so we pick a large threshold value t, which will predict bad loan case 
# rarely, since the probability of bad loan has to be really large to be greater 
# than the threshold.

table(loan_test$bad_loan,predictTest>.6)

# results with t > .8:
#     FALSE  TRUE
# 0   33347    11
# 1   7579    13


# If there's no preference between the errors, the right threshold to 
# select is t = 0.5, since it just predicts the most likely outcome.

table(loan_test$bad_loan,predictTest>.5)

# results with t > .5:
#     FALSE  TRUE
# 0   33100   258
# 1   7368    224

TP_Rate = 33100/(33100+224)
TN_Rate = 7368 / (7368 + 258)
  
# Selecting Thresholds
install.packages("ROCR")
library (ROCR)

###############################################################################################
# 5 e) A Receiver Operator Characteristic (ROC) curve for selecting the appropriate t threshold
# The ROC chart shows the False Positive Rate on the X-Axis,
# against the True Positive Rate (sensistivity) on the Y-Axis
###############################################################################################

# http://thestatsgeek.com/2014/05/05/area-under-the-roc-curve-assessing-discrimination-in-logistic-regression/

ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot (ROCRperf, colorize = TRUE)
plot (ROCRperf, colorize = TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# You can compute the test set Area Under the Curve (AUC) by running the following two commands in R:
# The area under the ROC curve is often used as a measure of quality of
# the classification model - random model has an area under the curve
# equal to .5, a perfect classifier is equal to 1

ROCRpredTest = prediction(predictTest, loan_test$bad_loan)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc



######################################################################
### Threshold values, Sensitivity and Specificity
### AKA Type 1 and 2 errors
######################################################################

# Convert the probabilities to predictions using a threshold value, t. If the probability of bad 
# loan is greater than this threshold value, t, we predict a bad loan.

# But if the probability of a bad loan is less than the threshold value,t, 
# then we predict that we have a good loan candidate.

# The threshold value, t, is often selected based on which errors are 
# better. There are two types of errors that a model can make --
# ones where you predict 1, or bad loan, but the actual outcome is 0, 
# and ones where you predict 0, or a good loan, but the actual outcome is 1 (bad loan).

# Since this customer is a bank, we'll assume the prefernce is to minimize the number of 
# bad loans, so we pick a large threshold value t, which will predict bad loan case 
# rarely, since the probability of bad loan has to be really large to be greater 
# than the threshold.

# This means that we will make more errors where we say good loan, but it's 
# actually a bad loan. On the other hand, if the threshold value, t, is 
# small, we predict bad loans frequently, and we predict good loans rarely.

# This means that we will make more errors where we say bad loan, but 
# it's actually a good loan candiate.

# If there's no preference between the errors, the right threshold to 
# select is t = 0.5, since it just predicts the most likely outcome.

# A confusion matrix or classification matrix. This compares the actual 
# outcomes to the predicted outcomes.

# The rows are labeled with the actual outcome, and the columns are labeled with the predicted outcome. Each entry of the table gives 
# the number of data observations that fall into that category.

# So the number of true negatives, or TN, is the number of observations 
# that are actually good loan candidates and for which we predict they 
# are good loan candidates.

# The true positives, or TP, is the number of observations that are actually poor loan candidates and for which we predict they are poor.

# These are the two types that we get correct.







#################################################################################################
# Model 2  CART - Classification and Regression Trees
#################################################################################################

# logistic regression models are not easily interpretable.
# The model coefficients in logistic regression indicate the importance and relative effect of variables,
# but do not give a simple explanation of how a decision is made.


# This method builds what is called a tree by splitting on the values of the independent variables.
# To predict the outcome for a new observation or case, you can follow the splits in the tree and at the end,
# you predict the most frequent outcome in the training set that followed the same path.

# Some advantages of CART are that it does not assume a linear model, like logistic regression
# or linear regression, and it's a very interpretable model.

# install additional required packages/libraries
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library (rpart.plot)

# Build Classification Tree (CART)
# The first one is method = "class". This tells rpart to build a classification tree, instead of
# a regression tree. The last argument we'll give is minbucket = 25. This limits the tree so that it doesn't
# overfit to our training set.

# http://trevorstephens.com/post/72923766261/titanic-getting-started-with-r-part-3-decision

?rpart 

LoanTree = rpart (bad_loan~loan_amnt+term+int_rate+emp_length+home_ownership+annual_inc+purpose+
                    addr_state+dti+delinq_2yrs+revol_util+total_acc+longest_credit_length+
                    verification_status, data=loan_train, method = "class", minbucket = 25)

# without non-numeric values
LoanTree = rpart (bad_loan~loan_amnt+term+int_rate+emp_length+annual_inc+
                    dti+delinq_2yrs+revol_util+total_acc+longest_credit_length,
                    data=loan_train, method = "class")

prp(LoanTree)


cor(loan_train[c("loan_amnt", "term", "int_rate", "annual_inc", 
                 "dti", "delinq_2yrs", "revol_util", "total_acc", "longest_credit_length")])


# Logistic Regression, CART, and Random Forest are all designed to be used 
# to predict whether or not someone has a heart attack, since this is a 
# classification problem. Linear regression would be appropriate for a problem 
# with a continuous outcome, such as the amount of time until someone has a 
# heart attack. In this lecture, we'll use random forest, but the other 
# methods could be used too.

# k-means clustering.
# The broad description of the algorithm is as follows.
# 1. We first specify the number of clusters k.
# 2. Then we randomly assign each data point to a cluster.
# 3. We then compute the cluster centroids.
# 4. We re-assign each point to the closest cluster centroid.
# 5. We then re-compute the cluster centroids,
# 6. repeat steps 4 and 5 until no improvement is made.

