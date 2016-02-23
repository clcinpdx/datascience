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
# set working directory and load data
############################################################
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/capstone')

loan  <- read.csv ("loan.csv", sep = ',', header = TRUE)

str(loan)

#############################################################################################
# 1. Split the dataset into training and the test sets.
#############################################################################################

## 75% of the sample size
smp_size <- floor(0.75 * nrow(loan))

## set the seed to make partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(loan)), size = smp_size)

loan_train <- loan [train_ind, ]
loan_test <- loan [-train_ind, ]



#############################################################################################
# 2. Exploration
#############################################################################################


# various views of the loan data
dim(loan)
View(loan)
summary(loan)

qplot(x=loan_amnt,y=annual_inc,data=loan)

ggplot (aes(x=loan_amnt,y=annual_inc),data=loan) +
  geom_point(alpha= 0.1) +
  geom_line(linetype = 1, stat = 'summary', fun.y = mean, color = 'blue') +
  coord_cartesian(xlim = c(0, 35000), ylim = c(0,185000)) +
  facet_grid(~bad_loan)

ggplot (aes(x=loan_amnt,y=purpose),data=loan) +
  geom_point(alpha= 0.1) +
  coord_cartesian(xlim = c(0, 35000)) +
  facet_grid(~bad_loan)

ggplot (aes(x=loan_amnt),data=loan) +
  geom_histogram () +
  coord_cartesian(xlim = c(0, 35000)) +
  facet_grid(~bad_loan)



#################################################################################################
# Model 1 Logistic Regression Model
#################################################################################################


################################################################# 
## since we are trying to detemine either loan or no loan, which
## we will use a logistic regression model
#################################################################

# Bivariate Model
# Build Bivariate models to identify which of our variables are useful in predicting a 
# particular outcome (e.g. models that predict the outcome using a single independent variable.) 
# An independent variable shall be considered significant if there is at least one star at 
# the end of the coefficients row for that variable (e.g. the probability column having a value smaller than 0.05)

LoanModBi = glm (bad_loan ~ loan_amnt + term + int_rate + emp_length + home_ownership + annual_inc +
                   purpose + addr_state + dti + delinq_2yrs + revol_util + total_acc + longest_credit_length+
                   verification_status, data=loan_train, family="binomial")
summary(LoanModBi)

LoanModBi01 = glm (bad_loan ~ loan_amnt, data=loan_train, family="binomial")
summary(LoanModBi01)

LoanModBi02 = glm (bad_loan ~ term, data=loan_train, family="binomial")
summary(LoanModBi02)

LoanModBi03 =  glm (bad_loan ~ int_rate, data=loan_train, family="binomial")
summary(LoanModBi03)

LoanModBi04 = glm (bad_loan ~ emp_length, data=loan_train, family="binomial")
summary(LoanModBi04)

LoanModBi05 = glm (bad_loan ~ home_ownership, data=loan_train, family="binomial")
summary(LoanModBi05)

LoanModBi06 = glm (bad_loan ~ annual_inc, data=loan_train, family="binomial")
summary(LoanModBi06)

LoanModBi07 = glm (bad_loan ~ annual_inc, data=loan_train, family="binomial")
summary(LoanModBi07)

LoanModBi08 = glm (bad_loan ~ purpose, data=loan_train, family="binomial")
summary(LoanModBi08)

LoanModBi09 = glm (bad_loan ~ addr_state, data=loan_train, family="binomial")
summary(LoanModBi09)

LoanModBi10 = glm (bad_loan ~ dti, data=loan_train, family="binomial")
summary(LoanModBi10)

LoanModBi11 = glm (bad_loan ~ delinq_2yrs, data=loan_train, family="binomial")
summary(LoanModBi11)

LoanModBi12 = glm (bad_loan ~ revol_util, data=loan_train, family="binomial")
summary(LoanModBi12)

LoanModBi13 = glm (bad_loan ~ total_acc, data=loan_train, family="binomial")
summary(LoanModBi13)

LoanModBi14 = glm (bad_loan ~ longest_credit_length, data=loan_train, family="binomial")
summary(LoanModBi14)

LoanModBi15 = glm (bad_loan ~ verification_status, data=loan_train, family="binomial")
summary(LoanModBi15)


# from these summaries we can see that the following variables appear significant to predicting 
# a bad_loan: loan_amnt, term60 months, int_rate, annual_inc, "some of" purpose, "some of" state, 
#   dti, delinq_2yrs, revol_util, longest_credit_length, verification_status, total_acc
# 
#   not significant: emp_length, home_ownership


# New model with only signficant variables

LoanModBi00 = glm (bad_loan ~ loan_amnt + term + int_rate + annual_inc +
                   purpose + addr_state + dti + delinq_2yrs + revol_util 
                   + total_acc + longest_credit_length + verification_status, 
                   data=loan_train, family="binomial")

summary(LoanModBi00)


# Multivariate Models for bad loans

# Often, variables that were significant in bivariate models are no longer significant in 
# multivariate analysis due to correlation between the variables.

# Evaluating the variable pairs to determine if any have a high degree of correlation 
# (a correlation greater than 0.8 or less than -0.8):

# compute all pair-wise correlations between variables with:

cor(loan_train[c("loan_amnt", "term", "int_rate", "annual_inc", "purpose", "addr_state",
           "dti", "delinq_2yrs", "revol_util", "total_acc", "longest_credit_length",
           "verification_status")])

cor(loan_train$loan_amnt,loan_train$term)
cor(loan_train$loan_amnt,loan_train$int_rate)
cor(loan_train$loan_amnt,loan_train$annual_inc)
cor(loan_train$loan_amnt,loan_train$purpose)
cor(loan_train$loan_amnt,loan_train$addr_state)
cor(loan_train$loan_amnt,loan_train$dti)
cor(loan_train$loan_amnt,loan_train$delinq_2yrs)
cor(loan_train$loan_amnt,loan_train$revol_util)
cor(loan_train$loan_amnt,loan_train$total_acc)
cor(loan_train$loan_amnt,loan_train$longest_credit_length)
cor(loan_train$loan_amnt,loan_train$verification_status)
cor(loan_train$term,loan_train$int_rate)
cor(loan_train$term,loan_train$annual_inc)
cor(loan_train$term,loan_train$purpose)
cor(loan_train$term,loan_train$addr_state)
cor(loan_train$term,loan_train$dti)
cor(loan_train$term,loan_train$delinq_2yrs)
cor(loan_train$term,loan_train$revol_util)
cor(loan_train$term,loan_train$total_acc)
cor(loan_train$term,loan_train$longest_credit_length)
cor(loan_train$term,loan_train$verification_status)
cor(loan_train$int_rate,loan_train$annual_inc)
cor(loan_train$int_rate,loan_train$purpose)
cor(loan_train$int_rate,loan_train$addr_state)
cor(loan_train$int_rate,loan_train$dti)
cor(loan_train$int_rate,loan_train$delinq_2yrs)
cor(loan_train$int_rate,loan_train$revol_util)
cor(loan_train$int_rate,loan_train$total_acc)
cor(loan_train$int_rate,loan_train$longest_credit_length)
cor(loan_train$int_rate,loan_train$verification_status)
cor(loan_train$annual_inc,loan_train$purpose)
cor(loan_train$annual_inc,loan_train$addr_state)
cor(loan_train$annual_inc,loan_train$dti)
cor(loan_train$annual_inc,loan_train$delinq_2yrs)
cor(loan_train$annual_inc,loan_train$revol_util)
cor(loan_train$annual_inc,loan_train$total_acc)
cor(loan_train$annual_inc,loan_train$longest_credit_length)
cor(loan_train$annual_inc,loan_train$verification_status)
cor(loan_train$purpose,loan_train$addr_state)
cor(loan_train$purpose,loan_train$dti)
cor(loan_train$purpose,loan_train$delinq_2yrs)
cor(loan_train$purpose,loan_train$revol_util)
cor(loan_train$purpose,loan_train$total_acc)
cor(loan_train$purpose,loan_train$longest_credit_length)
cor(loan_train$purpose,loan_train$verification_status)
cor(loan_train$addr_state,loan_train$dti)
cor(loan_train$addr_state,loan_train$delinq_2yrs)
cor(loan_train$addr_state,loan_train$revol_util)
cor(loan_train$addr_state,loan_train$total_acc)
cor(loan_train$addr_state,loan_train$longest_credit_length)
cor(loan_train$addr_state,loan_train$verification_status)
cor(loan_train$dti,loan_train$delinq_2yrs)
cor(loan_train$dti,loan_train$revol_util)
cor(loan_train$dti,loan_train$total_acc)
cor(loan_train$dti,loan_train$longest_credit_length)
cor(loan_train$dti,loan_train$verification_status)
cor(loan_train$delinq_2yrs,loan_train$revol_util)
cor(loan_train$delinq_2yrs,loan_train$total_acc)
cor(loan_train$delinq_2yrs,loan_train$longest_credit_length)
cor(loan_train$delinq_2yrs,loan_train$verification_status)
cor(loan_train$revol_util,loan_train$total_acc)
cor(loan_train$revol_util,loan_train$longest_credit_length)
cor(loan_train$revol_util,loan_train$verification_status)
cor(loan_train$total_acc,loan_train$longest_credit_length)
cor(loan_train$total_acc,loan_train$verification_status)
cor(loan_train$longest_credit_length,loan_train$verification_status)


# Question for Anirban: How do you deal with non-numeric values like term, purpose, addr_state, verification_status?
# Error in cor(loan_train[c("loan_amnt", "term", "int_rate", "annual_inc",  : 
# 'x' must be numeric

cor(loan_train[c("loan_amnt", "int_rate", "annual_inc", 
                 "dti", "delinq_2yrs", "revol_util", "total_acc", "longest_credit_length")])











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

# The false positives, or FP, are the number of data points for which we 
# predict bad loans, but they're actually good candidates. And the false negatives, or FN, are the number of data points for which we predict good candidates, but they're actually poor loan candidates.

# We can compute two outcome measures that help us determine what types 
# of errors we are making. They're called sensitivity and specificity.

# Sensitivity is equal to the true positives divided by 
# the true positives plus the false negatives, and measures the 
# percentage of actual bad loan cases that we classify correctly. 
# This is often called the true positive rate.

# Specificity is equal to the true negatives divided by 
# the true negatives plus the false positives, and measures the 
# percentage of actual good loan cases that we classify correctly. 
# This is often called the true negative rate.

# A model with a higher threshold will have a lower sensitivity
# and a higher specificity. A model with a lower threshold will have a higher sensitivity
# and a lower specificity.

predictTrain = predict(LoanModBi,type="response")
summary(predictTrain)

table(loan_train$bad_loan,predictTrain>0.7)

# Selecting Thresholds
install.packages("ROCR")
library (ROCR)

# A Receiver Operator Characteristic curve
ROCRpred = prediction(predictTrain, qualityTrain$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot (ROCRperf)
plot (ROCRperf, colorize = TRUE)
plot (ROCRperf, colorize = TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))





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
prp(LoanTree)





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

