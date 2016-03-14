#################################################################################################
# Model 2  CART - Classification and Regression Trees
#################################################################################################

# Exploration by Craig Calder

############################################################
# install packages
############################################################
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("gridExtra", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library("tidyr", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")

install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages('randomForest')


# Description of Variables:
# Source: https://www.lendingclub.com/info/download-data.action
###########################################################################################################
#
### Target Value (AKA Class) - Binary Classification 
#
# bad_loan    Numeric: 1 = Bad Loan, 0 = Good loan. Captures if the consumer was either a good or bad loan. 
#             This data is unevenly split between good and bad loans: Good Loans (0): 133,971 observations
#             Bad Loans (1): 30,016 observations
#
### Features (AKA predictors or attributes or features )
#
# loan_amnt	  The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
#             Loan amount ranges from 500 to 35,000.
#
# term	      (chr) The number of payments on the loan. Values are in months and can be either 36 or 60.
#
# int_rate    Ranging from 5.42 the best candidate (e.g. lowest risk) to 26.06 for high risk customers. Not part of the model since.
#
# emp_length	Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years. 
#
# home_ownership	(chr) The home ownership status provided by the borrower during registration. Our values are: RENT, OWN, MORTGAGE, OTHER.
#
# annual_inc	The self-reported annual income provided by the borrower during registration.
#
# purpose	    (chr) A category provided by the borrower for the loan request. 
#
# addr_state	The state provided by the borrower in the loan application
#
# dti	        A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income.
#
# delinq_2yrs	The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years
#
# revol_util	Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
#
# longest_credit_length
#
# verification_status   (chr)

############################################################
# 1. set working directory and load data
############################################################

getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/capstone')

# create the loan dataframe from the loan.csv data file

loan  <- read.csv ("loan.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)


#####################################################################
## 2) Impute missing values to eliminate NA's later
## http://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#####################################################################

library(mice)

# Determine which variables are missing data and what percent?
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(loan,2,pMiss)

# indicates the following variables are misssing values:
# emp_length (580 or 3.5%), annual_inc (4 or 0.002%), and delinq_2yrs (29), 
# total_acc (2), revol_util (193), longest_credit_length (29 or 0.017%)
# A "safe" maximum threshold is 5% of the total for large datasets. 

tempData <- mice(loan,m=1,maxit=5,meth='pmm',seed=500)

loan <- complete(tempData,1)
apply(loan,2,pMiss)

# confirm new loan dataset has no N/A's
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(loan,2,pMiss)

remove (pMiss)
remove (tempData)

#############################################################################################
# 2. Wrangle: Convert data to numeric or factors where needed
#############################################################################################
#
# When to use numeric versus factor values? Factor values should be used for categorical data 
# (e.g. discrete units that are not in any specific order), numeric should be used for continuous, 
#  ratio, or (some) interval level data.
# http://www.stat.berkeley.edu/~s133/factors.html


# 4a) Convert bad_loan char (dependent variable) to factors 
loan$bad_loan <- as.factor(loan$bad_loan)


# 4b) variables need to be factors (e.g. "36 months" to "36") 
loan$term <- as.vector(loan$term)
loan$term <- regmatches(loan$term, regexpr("[[:digit:]]+", loan$term))
loan$term <- as.factor(loan$term)


# 4c) add_state needs to factored and grouped 

# Bankruptcy Filings by State: http://www.valuepenguin.com/bankruptcy-filings-state
loan$addr_state <- as.factor(loan$addr_state)
levels(loan$addr_state) <- c(
  #   AK    AL     AR    AZ        CA     CO        CT      DC         DE    FL         GA     HI  #
  "Low","High","Med","Med-High","Med","Med-High","Med","Med-High", "Med","Med-High","High", "Low", 
  #   IA     ID         IL     IN       KS       KY      LA        MA    MD        ME      MI        MN   #
  "High","Med-High","High","High","Med-High","High","Med-High","Low","Med-High","Low","Med-High","Medium",  
  #   MO         MS     MT    NC    NE   NH        NJ     NM     NV     NY    OH     OK #
  "Med-High","High","Low","Med","Med","Low","Med-High","Med","High","Low","High","Med", 
  #   OR         PA    RI    SC    SD    TN     TX    UT      VA       VT     WA         WI     WV    WY #
  "Med-High","Med","Med","Low","Low","High","Low","High","Med-High","Low","Med-High","High","Med","Low")

loan$bankrpc_state_low <-  ifelse(loan$addr_state=="Low", 1, 0)
loan$bankrpc_state_med <- ifelse(loan$addr_state=="Med", 1, 0)
loan$bankrpc_state_medhigh <- ifelse(loan$addr_state=="Med-High", 1, 0)
loan$bankrpc_state_high  <- ifelse(loan$addr_state=="High", 1, 0)


# 4d) simplify purpose to a single binary factor
# hard = 1 = something tangable, like cars, house, major purchase, renewable energy 
# soft = 0 = something intangible, like debts, medical, vacations, wedding, etc.

loan$purpose <- as.factor (loan$purpose)
levels(loan$purpose) <- c (
  #   car  credit_card debt_consolidation educational home_improvement house   major_purchase   medical #
  "hard",  "soft",       "soft",          "soft" ,     "hard",       "hard",     "hard",      "soft",
  # moving   other  renewable_energy  small_business vacation   wedding
  "soft",   "soft",    "hard",          "soft",     "soft",    "soft")

loan$purpose <-  as.factor(ifelse(loan$purpose=="hard", 1, 0))


# 4e) factor verification_status to a numeric 
# 1 = verified
# 0 = unverified

loan$verification_status <- as.factor(loan$verification_status)
levels(loan$verification_status) <- c ("verified","not verified")
loan$vstatus_verified <- as.factor (ifelse(loan$verification_status=="verified", 1, 0))


# 4f) factor home_ownership
loan$home_ownership <- as.factor(loan$home_ownership)
levels(loan$home_ownership) <- c ("other","mortgage","other","other","other","rent")

loan$homeown_other <-  ifelse(loan$home_ownership=="other", 1, 0)
loan$homeown_mort <- ifelse(loan$home_ownership=="mortgage", 1, 0)
loan$homeown_rent <- ifelse(loan$home_ownership=="rent", 1, 0)


# 4g) Remove outliers from the dataset

# note: with outliers, annual_inc results in a glm.fit warning for fitted probabilites numerical 0 or 1 occured
# http://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/complete_separation_logit_models.htm


#      i) look at summary, look for variables where the mean and median are significantly different
summary(loan)
#     ii) specifically in the annual income data, with a Max value of $7,142,000  
summary (loan$annual_inc)
big_income <- subset(loan,loan$annual_inc >= 1000000)
#     iii) 27 observatiosn have over $1,000,000 in income. We will exclude them from this set since they would  
#          likely need to be considered separately for a loan anyhow
loan <- subset(loan,loan$annual_inc < 1000000)
#      iv) remove temp matrix big_income          
remove (big_income)

# remove interest rate as it is not an independent variable
loan <- subset( loan, select = -int_rate)



#############################################################################################
# 5a. balance the training set so that it is more equally aligns good and bad loan cases.
#############################################################################################

table (loan$bad_loan)
#   0      1 
# 133971  30016 

loan_bad <- subset (loan, loan$bad_loan == '1' )
loan_good <- subset (loan,loan$bad_loan == '0')

## 75% of the bad sample size to use to equalize good examples
smp_size <- floor(0.75 * nrow(loan_bad))


#############################################################################################
# 5b. Split the equalized dataset into training and the test sets.
#############################################################################################

## set the seed to make partition reproductible and select the same training set sizes

set.seed(123)

train_ind_bad <- sample(seq_len(nrow(loan_bad)), size = smp_size)
train_ind_good <- sample(seq_len(nrow(loan_good)), size = smp_size)

loan_train_bad <- loan_bad [train_ind_bad, ]
loan_train_good <- loan_good [train_ind_good, ]
loan_train <- rbind (loan_train_good,loan_train_bad)

loan_test_bad <- loan_bad [-train_ind_bad, ]
loan_test_good <- loan_good [-train_ind_good, ]
loan_test <- rbind (loan_test_good,loan_test_bad)

# cleanup
remove (loan_test_bad)
remove(loan_test_good)
remove (loan_train_bad)
remove (loan_train_good)
remove (loan_bad)
remove (loan_good)
remove (train_ind_bad)
remove (train_ind_good)
remove (smp_size)

table (loan_train$bad_loan)
table (loan_test$bad_loan)


# logistic regression models are not easily interpretable.
# The model coefficients in logistic regression indicate the importance and relative effect of variables,
# but do not give a simple explanation of how a decision is made.


# This method builds what is called a tree by splitting on the values of the independent variables.
# To predict the outcome for a new observation or case, you can follow the splits in the tree and at the end,
# you predict the most frequent outcome in the training set that followed the same path.

# Some advantages of CART are that it does not assume a linear model, like logistic regression
# or linear regression, and it's a very interpretable model.

# Build Classification Tree (CART)

# http://www.r-bloggers.com/classification-tree-models/
# http://trevorstephens.com/post/72923766261/titanic-getting-started-with-r-part-3-decision
# http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/

# TREE package
library(tree)
?tree
tr <- tree(bad_loan ~ ., data=loan_train)
summary(tr)
plot(tr); text(tr)
remove (tr)

# RPART package
library (rpart.plot)
library(rpart)
# only numeric values
LoanTree = rpart (bad_loan~., control=rpart.control(minsplit=20),data=loan_train)
prp(LoanTree)

# remove term, 
LoanTree2 = rpart (bad_loan~loan_amnt + annual_inc + dti + delinq_2yrs + 
                     total_acc + longest_credit_length + bankrpc_state_low +   
                     bankrpc_state_high  + homeown_mort + homeown_rent + 
                     purpose + vstatus_verified,control=rpart.control(minsplit=10),data=loan_train)
prp(LoanTree2)

remove(LoanTree)
remove(LoanTree2)

# Random Forest package
# http://trevorstephens.com/post/73770963794/titanic-getting-started-with-r-part-5-random
# https://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html
#http://www.inside-r.org/packages/cran/randomforest/docs/randomforest

library(randomForest)
set.seed(415)
memory.limit()
memory.size(max = TRUE)

fit <- randomForest(bad_loan ~ ., data=loan_train, importance=TRUE, ntree=100, do.trace=TRUE)
plot(fit)
print(fit)

varImpPlot(fit, sort = TRUE, type = 1, pch = 19, col = 1, cex = 1,
           main = "Relative Importance of Loan Variables \nin Random Forest Predictive Model \n MeanDecreaseAccuracy")

# MeanDecreaseGini is a measure of variable importance based on the Gini impurity index used for the 
# calculation of splits during training. A low Gini (i.e. higher descrease in Gini) means that a 
# particular predictor variable plays a greater role in partitioning the data into the defined classes.

varImpPlot(fit,sort = TRUE, type = 2, pch = 19, col = 1, cex = 1,
           main = "Relative Importance of Loan Variables \nin Random Forest Predictive Model 
           \n MeanDecreaseGini")

# http://stackoverflow.com/questions/14996619/random-forest-output-interpretation
# http://discuss.analyticsvidhya.com/t/how-to-extract-important-variables-from-random-forest-model-using-varimpplot-in-r/1325



