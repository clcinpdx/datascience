#R-Scripts exploring Capstone Dataset

# Exploration by Craig Calder

############################################################
# install packages
############################################################
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("gridExtra", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library("tidyr", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")

install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")

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

tempData <- mice(loan,m=1,maxit=25,meth='pmm',seed=500)

loan <- complete(tempData,1)
apply(loan,2,pMiss)

remove (pMiss)

# confirm new loan dataset has no N/A's
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(loan,2,pMiss)


#############################################################################################
# 3. Wrangle: Convert data to numeric or factors where needed
#############################################################################################
#
# When to use numeric versus factor values? Factor values should be used for categorical data 
# (e.g. discrete units that are not in any specific order), numeric should be used for continuous, 
#  ratio, or (some) interval level data.
# http://www.stat.berkeley.edu/~s133/factors.html


# 3a) Convert bad_loan char (dependent variable) to factors 
loan$bad_loan <- as.factor(loan$bad_loan)


# 3b) variables need to be factors (e.g. "36 months" to "36") 
loan$term <- as.vector(loan$term)
loan$term <- regmatches(loan$term, regexpr("[[:digit:]]+", loan$term))
loan$term <- as.factor(loan$term)


# 3c) add_state needs to factored and grouped 

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


# 3d) simplify purpose to a single binary factor
# hard = 1 = something tangable, like cars, house, major purchase, renewable energy 
# soft = 0 = something intangible, like debts, medical, vacations, wedding, etc.

loan$purpose <- as.factor (loan$purpose)
levels(loan$purpose) <- c (
  #   car  credit_card debt_consolidation educational home_improvement house   major_purchase   medical #
  "hard",  "soft",       "soft",          "soft" ,     "hard",       "hard",     "hard",      "soft",
  # moving   other  renewable_energy  small_business vacation   wedding
  "soft",   "soft",    "hard",          "soft",     "soft",    "soft")

loan$purpose <-  as.factor(ifelse(loan$purpose=="hard", 1, 0))


# 3e) factor verification_status to a numeric 
# 1 = verified
# 0 = unverified

loan$verification_status <- as.factor(loan$verification_status)
levels(loan$verification_status) <- c ("verified","not verified")
loan$vstatus_verified <- as.factor (ifelse(loan$verification_status=="verified", 1, 0))


# 3f) factor home_ownership
loan$home_ownership <- as.factor(loan$home_ownership)
levels(loan$home_ownership) <- c ("other","mortgage","other","other","other","rent")

loan$homeown_other <-  ifelse(loan$home_ownership=="other", 1, 0)
loan$homeown_mort <- ifelse(loan$home_ownership=="mortgage", 1, 0)
loan$homeown_rent <- ifelse(loan$home_ownership=="rent", 1, 0)


# 3g) Remove outliers from the dataset

# note: with outliers, annual_inc results in a glm.fit warning for fitted probabilites numerical 0 or 1 occured
# http://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/complete_separation_logit_models.htm


#      i) look at summary, look for variables where the mean and median are significantly different
summary(loan)
#     ii) specifically in the annual income data, with a Max value of $7,142,000  
summary (loan$annual_inc)
big_income <- subset(loan,loan$annual_inc >= 750000)
#     iii) 27 observatiosn have over $1,000,000 in income. We will exclude them from this set since they would  
#          likely need to be considered separately for a loan anyhow
loan <- subset(loan,loan$annual_inc < 750000)
#      iv) remove temp matrix big_income          
remove (big_income)

# 4h) exclude interest rate as it is not an independent variable

loan <- subset(loan, select = -int_rate)


#############################################################################################
# 4a. balance the training set so that it is more equally aligns good and bad loan cases.
#############################################################################################

table (loan$bad_loan)
#   0      1 
# 133896  30011 

loan_bad <- subset (loan, loan$bad_loan == '1' )
loan_good <- subset (loan,loan$bad_loan == '0')

## 75% of the bad sample size to use to equalize good examples
smp_size <- floor(0.75 * nrow(loan_bad))


#############################################################################################
# 4b. Split the equalized dataset into training and the test sets.
#############################################################################################

## set the seed to make partition reproductible and select the same training set sizes

set.seed(123)

train_ind_bad <- sample(seq_len(nrow(loan_bad)), size = smp_size)
train_ind_good <- sample(seq_len(nrow(loan_good)), size = smp_size)

loan_train_bad <- loan_bad [train_ind_bad, ]
loan_train_good <- loan_good [train_ind_good, ]
loan_train <- rbind (loan_train_good,loan_train_bad)

# create an "unbalanced" test set with good and bad loans
loan_test_bad <- loan_bad [-train_ind_bad, ]
loan_test_good <- loan_good [-train_ind_good, ]
loan_test <- rbind (loan_test_good,loan_test_bad)

## create an "balanced" test set between good and bad loans ##
# loan_test_bad <- loan_bad [-train_ind_bad, ]
# balanced_smp_size <- nrow(loan_test_bad)
# test_good_balanced_ind <- sample(seq_len(nrow(loan_good)), size = balanced_smp_size)
# loan_test_good_balanced <- loan_good [test_good_balanced_ind, ]
# loan_test <- rbind (loan_test_good_balanced,loan_test_bad)
# remove (balanced_smp_size,test_good_balanced_ind,loan_test_good_balanced)


table (loan_train$bad_loan)
#   0     1 
# 22508 22508

table (loan_test$bad_loan)
#   0      1 
# 111459   7503

# cleanup
remove (loan_test_bad, loan_test_good,loan_train_bad,loan_train_good)
remove (loan_bad, loan_good,train_ind_bad, train_ind_good, smp_size)



#################################################################################################
# 5. Model 1 - Logistic Regression Model
#################################################################################################


################################################################# 
## since we are trying to detemine either loan or no loan, which
## we will use a logistic regression model (and a classificaton model)
#################################################################

# 5a) Creatig the Bivariate Models

# Build Bivariate models to identify which of our variables are useful in predicting a 
# particular outcome (e.g. models that predict the outcome using a single independent variable.) 
# An independent variable shall be considered significant if there is at least one star at 
# the end of the coefficients row for that variable (e.g. the probability column having a value smaller than 0.05)

# Create a matrix containing only the Variable, its P-Value, and the "significance"
# for the numeric variables

Binomial_Results_Summary <- matrix (nrow=18, ncol =4)
colnames(Binomial_Results_Summary) <- c("Variable","Coefficient", "P-Value", "significance")
ranges <- c(0, 0.001, 0.01, 0.05, 0.1)
codes <- c("***" , "**","*", ".", " ")

LoanModBi01 = glm (bad_loan ~ loan_amnt, data=loan_train, family="binomial")
Binomial_Results_Summary [1,] <- c("Loan_amount", coef(summary(LoanModBi01))[2,"Estimate"], coef(summary(LoanModBi01))[2,"Pr(>|z|)"],"")
Binomial_Results_Summary [1,4] <- codes[findInterval(Binomial_Results_Summary[1,3], ranges)]

LoanModBi02 = glm (bad_loan ~ emp_length, data=loan_train, family="binomial")
Binomial_Results_Summary [2,] <- c("emp_length", coef(summary(LoanModBi02))[2,"Estimate"], coef(summary(LoanModBi02))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [2,4] <- codes[findInterval(Binomial_Results_Summary[2,3], ranges)]

LoanModBi03 = glm (bad_loan ~ annual_inc, data=loan_train, family="binomial")
Binomial_Results_Summary [3,] <- c("annual_inc", coef(summary(LoanModBi03))[2,"Estimate"], coef(summary(LoanModBi03))[2,"Pr(>|z|)"],"")
Binomial_Results_Summary [3,4] <- codes[findInterval(Binomial_Results_Summary[3,3], ranges)]

LoanModBi04 = glm (bad_loan ~ dti, data=loan_train, family="binomial")
Binomial_Results_Summary [4,] <- c("dti", coef(summary(LoanModBi04))[2,"Estimate"], coef(summary(LoanModBi04))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [4,4] <- codes[findInterval(Binomial_Results_Summary[4,3], ranges)]

LoanModBi05 = glm (bad_loan ~ delinq_2yrs, data=loan_train, family="binomial")
Binomial_Results_Summary [5,] <- c("delinq_2yrs",coef(summary(LoanModBi05))[2,"Estimate"], coef(summary(LoanModBi05))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [5,4] <- codes[findInterval(Binomial_Results_Summary[5,3], ranges)]

LoanModBi06 = glm (bad_loan ~ revol_util, data=loan_train, family="binomial")
Binomial_Results_Summary [6,] <- c("revol_util", coef(summary(LoanModBi06))[2,"Estimate"], coef(summary(LoanModBi06))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [6,4] <- codes[findInterval(Binomial_Results_Summary[6,3], ranges)]

LoanModBi07 = glm (bad_loan ~ total_acc, data=loan_train, family="binomial")
Binomial_Results_Summary [7,] <- c("total_acc", coef(summary(LoanModBi07))[2,"Estimate"], coef(summary(LoanModBi07))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [7,4] <- codes[findInterval(Binomial_Results_Summary[7,3], ranges)]

LoanModBi08 = glm (bad_loan ~ longest_credit_length, data=loan_train, family="binomial")
Binomial_Results_Summary [8,] <- c("longest_credit_length", coef(summary(LoanModBi08))[2,"Estimate"], coef(summary(LoanModBi08))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [8,4] <- codes[findInterval(Binomial_Results_Summary[8,3], ranges)]

# Binomial for all the dummy variables created earlier

# loan$bankrpc_state_low
# loan$bankrpc_state_med 
# loan$bankrpc_state_medhigh
# loan$bankrpc_state_high

LoanModBi09 = glm (bad_loan ~ bankrpc_state_low, data=loan_train, family="binomial")
Binomial_Results_Summary [9,] <- c("bankrpc_state_low (dummy)",coef(summary(LoanModBi09))[2,"Estimate"], coef(summary(LoanModBi09))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [9,4] <- codes[findInterval(Binomial_Results_Summary[9,3], ranges)]

LoanModBi10 = glm (bad_loan ~ bankrpc_state_med, data=loan_train, family="binomial")
Binomial_Results_Summary [10,] <- c("bankrpc_state_med (dummy)",coef(summary(LoanModBi10))[2,"Estimate"], coef(summary(LoanModBi10))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [10,4] <- codes[findInterval(Binomial_Results_Summary[10,3], ranges)]

LoanModBi11 = glm (bad_loan ~ bankrpc_state_medhigh, data=loan_train, family="binomial")
Binomial_Results_Summary [11,] <- c("bankrpc_state_medhigh (dummy)",coef(summary(LoanModBi11))[2,"Estimate"], coef(summary(LoanModBi11))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [11,4] <- codes[findInterval(Binomial_Results_Summary[11,3], ranges)]

LoanModBi12 = glm (bad_loan ~ bankrpc_state_high, data=loan_train, family="binomial")
Binomial_Results_Summary [12,] <- c("bankrpc_state_high (dummy)",coef(summary(LoanModBi12))[2,"Estimate"], coef(summary(LoanModBi12))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [12,4] <- codes[findInterval(Binomial_Results_Summary[12,3], ranges)]

# loan$homeown_other 
# loan$homeown_mort
# loan$homeown_rent

LoanModBi13 = glm (bad_loan ~ homeown_other, data=loan_train, family="binomial")
Binomial_Results_Summary [13,] <- c("homeown_other (dummy)",coef(summary(LoanModBi13))[2,"Estimate"], coef(summary(LoanModBi13))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [13,4] <- codes[findInterval(Binomial_Results_Summary[13,3], ranges)]

LoanModBi14 = glm (bad_loan ~ homeown_mort, data=loan_train, family="binomial")
Binomial_Results_Summary [14,] <- c("homeown_mort (dummy)",coef(summary(LoanModBi14))[2,"Estimate"], coef(summary(LoanModBi14))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [14,4] <- codes[findInterval(Binomial_Results_Summary[14,3], ranges)]

LoanModBi15 = glm (bad_loan ~ homeown_rent, data=loan_train, family="binomial")
Binomial_Results_Summary [15,] <- c("homeown_rent (dummy)",coef(summary(LoanModBi15))[2,"Estimate"], coef(summary(LoanModBi15))[2,"Pr(>|z|)"], "")
Binomial_Results_Summary [15,4] <- codes[findInterval(Binomial_Results_Summary[15,3], ranges)]

##################################################################################
# 5 c): chi-squared tests information for non-numeric variables:
##################################################################################
#
# http://stackoverflow.com/questions/3571909/calculate-correlation-cor-for-only-a-subset-of-columns
# http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
# http://stackoverflow.com/questions/26728197/error-creating-chisq-test-in-r-invalid-type-character-of-argument

# chi squared tests for the remaining five factor variables

Binomial_Results_Summary [16,] <- c("term (factor)","", chisq.test(loan_train$bad_loan,loan_train$term)$p.value, "")
Binomial_Results_Summary [16,4] <- codes[findInterval(Binomial_Results_Summary[16,3], ranges)]

Binomial_Results_Summary [17,] <- c("purpose (factor)","", chisq.test(loan_train$bad_loan,loan_train$purpose)$p.value, "")
Binomial_Results_Summary [17,4] <- codes[findInterval(Binomial_Results_Summary[17,3], ranges)]

Binomial_Results_Summary [18,] <- c("vstatus_verified (factor)","",chisq.test(loan_train$bad_loan,loan_train$vstatus_verified)$p.value, "")
Binomial_Results_Summary [18,4] <- codes[findInterval(Binomial_Results_Summary[18,3], ranges)]


########################################################
# Display complete matrix including Significance labels
########################################################

Binomial_Results_Summary

# Conclusion: from the above, the following variables are significant to predicting (P > +/- 0.8) a bad_loan:
#
#             loan_amnt, annual_inc, dti, delinq_2yrs, 
#             revol_util, longest_credit_length, 
#             total_acc, longest_credit_length, 
#             bankrpc_state_low, bankrpc_state_high, 
#             purchase_hardgood, purchase_financing, 
#             vstatus_verified, vstatus_notverified,  
#
# The following variables will not be used: 
#
#             emp_length, bankrpc_state_med, 
#             bankrpc_state_medhigh, homeown_other
#


###########################################################################################
# 5 d) Multi-variate Models for bad loans

# Often, variables that were significant in bivariate models are no longer significant in 
# multivariate analysis due to correlation between the variables.

# Evaluating the variable pairs to determine if any have a high degree of correlation 
# (a correlation greater than 0.8 or less than -0.8):

# When correlation is close to 1.
#   This means that there is a strong relationship between your two variables. This means that changes 
#   in one variable are strongly correlated with changes in the secondvariable. When correlation is 
#   very close to 1 we can conclude that there is a strong relationship between these two variables. 
# When correlation is close to 0.
#   This means that there is a weak relationship between your two variables. 
#   This means that changes in one variable are not correlated with changes in the second variable. 
#   If correlation were 0.01, we could conclude that our variables were not strongly correlated.
# When positive (+).
#   This means that as one variable increases in value, the second variable also increase in value. 
#   Similarly, as one variable decreases in value, the second variable also decreases in value. 
#   This is called a positive correlation. 
# When negative (-).
#   This means that as one variable increases in value, the second variable decreases in value. 
#   This is called a negative correlation. 

# compute all pair-wise correlations between numeric variables with:

loan_corr <-  cor(loan_train[c("loan_amnt", "annual_inc", "emp_length", "dti", 
                               "delinq_2yrs", "revol_util", "total_acc", "longest_credit_length")])

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# Displayed graphically: Positive correlations are displayed in blue and negative correlations 
# in red color. Color intensity and the size of the circle are proportional to 
# the correlation coefficients. In the right side of the correlogram, the 
# legend color shows the correlation coefficients and the corresponding colors.

library(corrplot)

corrplot(loan_corr, method = "circle", order = "alphabet",  tl.srt = 45,  sig.level = 0.01, insig = "blank", type = "lower")
corrplot(loan_corr, method = "number", order = "alphabet",  tl.srt = 45,  sig.level = 0.01, insig = "blank", type = "lower")

# conclusion: None of the numeric varaibles are excessively correlated that require additional management


##################################################################################
# 5 e): Updated model with only the significant variables
##################################################################################

LoanModel00 = glm (bad_loan ~ loan_amnt + annual_inc + dti + delinq_2yrs + revol_util +
                     total_acc + longest_credit_length + bankrpc_state_low +   
                     bankrpc_state_high  + homeown_mort + homeown_rent + 
                     term + purpose + vstatus_verified,
                   data=loan_train, family="binomial")

summary(LoanModel00)


######################################################################
### 6 Model Validation
######################################################################


######################################################################
### 6 a) Binary Classification Confusion Matrix
### Shows us the number of correct and incorrect predictions
### make by the classification moeel compared to the 
### actual outcomes (e.g. test dataset)
######################################################################

# See https://www.youtube.com/watch?v=ynRhJsX38Tw

# TP = True Positive    : predicted positive, test positive
# FP = False Positive   : predicted positive, test negative
# TN = True Negative    : predicted negative, test positive
# FN = False Negative   : predicted negative, test negative 

# For example, the false positives, or FP, are the number of data points for which we 
# predict bad loans, but they're actually good candidates. 
# And the false negatives, or FN, are the number of data points for which we predict good candidates, 
# but they're actually poor loan candidates.

# On one hand, this customer is a bank, and could assume the preference is to minimize the number of 
# bad loans, so we pick a large threshold value t, which will predict bad loan case rarely  
# 0 / 1 = true outcome. 1 = Bad Loan, 0 = Good loan
# FALSE / TRUE = predicted outcome

# In medical diagnosis, test sensitivity is the ability of a test to correctly identify those 
# with the disease (true positive rate), whereas test specificity is the ability of the test 
# to correctly identify those without the disease (true negative rate).
# Thus sensitivity quantifies the avoiding of false negatives, as specificity does for false 
# positives.

# However, higher risk customers also allow for the bank to charge higher interest rates, which in 
# turn drives more profit, but must be balanced against loss created by a bad loan.

predictTest = predict(LoanModel00, type="response", newdata=loan_test)

t_Summary <- matrix (nrow=6, ncol=7)
colnames(t_Summary) <- c("Threshold Value","TP","FP","TN","FN", "Sensitivity", "Specificity")


### t > 0.70
tt70 <- table(loan_test$bad_loan, predictTest> .70)
t_Summary [1,] <- c("t > .70",
                    tt70[1,"FALSE"],tt70[2,"FALSE"],tt70[2,"TRUE"],tt70[1,"TRUE"],
                    round((tt70[1,"FALSE"]/(tt70[1,"FALSE"]+tt70[1,"TRUE"])),4),
                    round((tt70[2,"TRUE"]/(tt70[2,"TRUE"]+tt70[2,"FALSE"])),4))

### t > 0.65
tt65 <- table(loan_test$bad_loan, predictTest> .65)
t_Summary [2,] <- c("t > .65",
                    tt65[1,"FALSE"],tt65[2,"FALSE"],tt65[2,"TRUE"],tt65[1,"TRUE"],
                    round((tt65[1,"FALSE"]/(tt65[1,"FALSE"]+tt65[1,"TRUE"])),4),
                    round((tt65[2,"TRUE"]/(tt65[2,"TRUE"]+tt65[2,"FALSE"])),4))

### t > 0.60
tt60 <- table(loan_test$bad_loan, predictTest> .60)
t_Summary [3,] <- c("t > .60",
                    tt60[1,"FALSE"],tt60[2,"FALSE"],tt60[2,"TRUE"],tt60[1,"TRUE"],
                    round((tt60[1,"FALSE"]/(tt60[1,"FALSE"]+tt60[1,"TRUE"])),4),
                    round((tt60[2,"TRUE"]/(tt60[2,"TRUE"]+tt60[2,"FALSE"])),4))

### t > 0.55
tt55 <- table(loan_test$bad_loan,predictTest>.55)
t_Summary [4,] <- c("t > .55",
                    tt55[1,"FALSE"],tt55[2,"FALSE"],tt55[2,"TRUE"],tt55[1,"TRUE"],
                    round((tt55[1,"FALSE"]/(tt55[1,"FALSE"]+tt55[1,"TRUE"])),4),
                    round((tt55[2,"TRUE"]/(tt55[2,"TRUE"]+tt55[2,"FALSE"])),4))

### t > 0.50
tt50 <- table(loan_test$bad_loan,predictTest>.50)
t_Summary [5,] <- c("t > .50",
                    tt50[1,"FALSE"],tt50[2,"FALSE"],tt50[2,"TRUE"],tt50[1,"TRUE"],
                    round((tt50[1,"FALSE"]/(tt50[1,"FALSE"]+tt50[1,"TRUE"])),4),
                    round((tt50[2,"TRUE"]/(tt50[2,"TRUE"]+tt50[2,"FALSE"])),4))

### t = 0.45
tt45 <- table(loan_test$bad_loan, predictTest > .45)
t_Summary [6,] <- c("t > .45",
                    tt45[1,"FALSE"],tt45[2,"FALSE"],tt45[2,"TRUE"],tt45[1,"TRUE"],
                    round((tt45[1,"FALSE"]/(tt45[1,"FALSE"]+tt45[1,"TRUE"])),4),
                    round((tt45[2,"TRUE"]/(tt45[2,"TRUE"]+tt45[2,"FALSE"])),4))

t_Summary

# specificity quantifies false positives. 
# The false positives, or FP, are the number of times we predict bad loans, but they're actually good candidates. 

# sensitivity quantifies the avoiding of false negatives, 
# The false negatives, or FN, are the number of times we predict good loans, but they're actually bad loan candidates.




###############################################################################################
# 6 b) A Receiver Operator Characteristic (ROC) curve for selecting the appropriate t threshold
# The ROC chart shows the False Positive Rate on the X-Axis,
# against the True Positive Rate (sensistivity) on the Y-Axis
###############################################################################################

# Selecting Thresholds
install.packages("ROCR")
library (ROCR)

# http://thestatsgeek.com/2014/05/05/area-under-the-roc-curve-assessing-discrimination-in-logistic-regression/
predictTest = predict(LoanModel00, type="response", newdata=loan_test)
ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, measure="tpr",x.measure="fpr")
SSperf =  performance(ROCRpred, measure="sens", x.measure="spec")
?performance
# where tpr = true positive rate, or senstivity
# where fpr = false positive rate, or specificity

# Specially, True Negative is really low (< 1%). Not always 50% is the cutoff for probabilities. 
# You need to check ROC curve to what's right cut-off (keeping True Positive and False Positive balanced). 
# Accuracy wise, nearing 70% should be fine. But data has so many '0' classes and few '1' classes. 
# So, if model predicts only '0' then also accuracy will come around 60-70%. But that's not intent of the model, 
# your model should be able to predict '1' classes as well. That's why  True Negative is also important. 
# Please try to improve it. Don't need to do lot of work, just take your best model and just work on that model. 

# clear buffer to ensure plots display correctly
dev.off()

plot (ROCRperf, colorize = TRUE,print.cutoffs.at=seq(0,1,0.05), text.adj=c(-0.2,1.7),
      main="Receiver Operator Characteristic (ROC) Curve", xlab="False Positive Rate", ylab="True Positive Rate")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

plot (SSperf, colorize = TRUE,print.cutoffs.at=seq(0,1,0.05), text.adj=c(-0.2,1.7),
      main="Sensitivity / Specificity Plots", xlab="tpr - sensitivity", ylab="tnr - specificity")
abline(a=1,b=-1,lwd=2,lty=2,col="gray")



?performance 

# You can compute the test set Area Under the Curve (AUC) by running the following two commands in R:
# The area under the ROC curve is often used as a measure of quality of
# the classification model - random model has an area under the curve
# equal to .5, a perfect classifier is equal to 1

aucROCR = as.numeric(performance(ROCRpred, "auc")@y.values)
aucROCR

# Test area under curve (AUC) = 66.032% (using matched bad/good training set)

#######################################################################################################
## 
## 7. Classification Trees (CART)
##
#   http://www.r-bloggers.com/classification-tree-models/
#   http://trevorstephens.com/post/72923766261/titanic-getting-started-with-r-part-3-decision
#   http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
##                          
#######################################################################################################

# logistic regression models are not easily interpretable.
# The model coefficients in logistic regression indicate the importance and relative effect of variables,
# but do not give a simple explanation of how a decision is made.


# This method builds what is called a tree by splitting on the values of the independent variables.
# To predict the outcome for a new observation or case, you can follow the splits in the tree and at the end,
# you predict the most frequent outcome in the training set that followed the same path.

# Some advantages of CART are that it does not assume a linear model, like logistic regression
# or linear regression, and it's a very interpretable model.

# TREE package
library(tree)
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


# Random Forest package
# http://trevorstephens.com/post/73770963794/titanic-getting-started-with-r-part-5-random
# https://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html
# http://www.inside-r.org/packages/cran/randomforest/docs/randomforest

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
# calculation of splits during training. A low Gini (i.e. higher decrease in Gini) means that a 
# particular predictor variable plays a greater role in partitioning the data into the defined classes.

varImpPlot(fit, sort = TRUE, type = 2, pch = 19, col = 1, cex = 1,
           main = "Relative Importance of Loan Variables \nin Random Forest Predictive Model\n MeanDecreaseGini")

# http://stackoverflow.com/questions/14996619/random-forest-output-interpretation
# http://discuss.analyticsvidhya.com/t/how-to-extract-important-variables-from-random-forest-model-using-varimpplot-in-r/1325


#######################################################################################################
## 
## 8. Calculating Area Under Curve (AUC) for variations of models
#
#     Model 02: remove variables (e.g. simplify the model) and improve or at least maintain sufficiently predictive?
#
#     Model 03a: Based on RPART: term, annual_inc, revol_util
#
#     Model 03b: Based on Tree: term, annual_inc
#   
#     Model 04a, 04b Based on randomForest: revol_util, dti, annual_inc, loan_amnt, total_acc, longest_credit_length, emp_length
#
##                          
#######################################################################################################


Model_Results_Summary <- matrix (nrow=6, ncol =4)
colnames(Model_Results_Summary) <- c("Model # and Type","AUC", "Var#", "Sample of Variables...")
Model_Results_Summary [1,1] <- "Model 01 : Log Reg"
Model_Results_Summary [1,2] <-  signif(auc00,digits=5)
Model_Results_Summary [1,3] <-  "14"
Model_Results_Summary [1,4] <- "loan_amnt + annual_inc + dti + delinq_2yrs ....."


# remove the less significant variables from Summary of LoanModel01:
#
#                                       longest_credit_length, bankrpc_state_high, 
#                                       homeown_rent, purpose

LoanModel02 = glm (bad_loan ~ loan_amnt + annual_inc + dti + delinq_2yrs + 
                     revol_util + total_acc + bankrpc_state_low + homeown_mort +    
                     term + vstatus_verified,
                   data=loan_train, family="binomial")

predictTest = predict(LoanModel02, type="response", newdata=loan_test)
ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
# Test area under curve (AUC) =  66.14686% (a negigible drop from 66.032% in exchange for a simplification)

Model_Results_Summary [2,1] <- "Model 02 : Log Reg"
Model_Results_Summary [2,2] <-  signif(auc,digits=5)
Model_Results_Summary [2,3] <-  "11"
Model_Results_Summary [2,4] <- "loan_amnt + annual_inc + dti + delinq_2yrs ....."


### Models adjusted based on TREE analysis

LoanModel03a = glm (bad_loan ~ term + annual_inc + revol_util, 
                    data=loan_train, family="binomial")

predictTest = predict(LoanModel03a, type="response", newdata=loan_test)
ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
# Test area under curve (AUC) =  65.14% 

Model_Results_Summary [3,1] <- "Model 03a: rpart"
Model_Results_Summary [3,2] <- signif(auc,digits=5)
Model_Results_Summary [3,3] <-  "3"
Model_Results_Summary [3,4] <- "annual_inc + revol_util + term"

LoanModel03b = glm (bad_loan ~ annual_inc + term,
                    data=loan_train, family="binomial")

predictTest = predict(LoanModel03b, type="response", newdata=loan_test)
ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
# Test area under curve (AUC) =  63.45% (a negigible drop from 66.032% in exchange for a simplification)

Model_Results_Summary [4,1] <- "Model 03b: tree"
Model_Results_Summary [4,2] <- signif(auc,digits=5)
Model_Results_Summary [4,3] <-  "2"
Model_Results_Summary [4,4] <- "annual_inc + term"


LoanModel04a = glm (bad_loan ~ revol_util + dti + annual_inc + loan_amnt,
                    data=loan_train, family="binomial")

predictTest = predict(LoanModel04a, type="response", newdata=loan_test)
ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
# Test area under curve (AUC) =  63.14% (a negigible drop from 66.032% in exchange for a simplification)

Model_Results_Summary [5,1] <- "Model 04a: Rand Forest"
Model_Results_Summary [5,2] <-  signif(auc,digits=5)
Model_Results_Summary [5,3] <-  "4"
Model_Results_Summary [5,4] <- "revol_util + dti + annual_inc + loan_amnt"

LoanModel04b = glm (bad_loan ~ revol_util+ dti + annual_inc + loan_amnt + total_acc + annual_inc + longest_credit_length + emp_length,
                    data=loan_train, family="binomial")

predictTest = predict(LoanModel04b, type="response", newdata=loan_test)
ROCRpred = prediction(predictTest, loan_test$bad_loan)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

# Test area under curve (AUC) =  63.14% (a negigible drop from 66.032% in exchange for a simplification)

Model_Results_Summary [6,1] <- "Model 04b: Rand Forest"
Model_Results_Summary [6,2] <-  signif(auc,digits=5)
Model_Results_Summary [6,3] <-  "8"
Model_Results_Summary [6,4] <- "revol_util + dti + annual_inc + loan_amnt ...."

Model_Results_Summary


#############################################################################################
# 9. EDA
#############################################################################################

install.packages("ggplot2")
library("ggplot2")
dev.off()

### Discrete X, Continuous Y 

ggplot(loan, aes(x = term, y = annual_inc, fill = loan$bad_loan), 
                 xlab = "1 = Bad Loan, 0 = Good loan", ylab = "Annual Income", main = "Term vs. Annual Income") + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 125000))

ggplot(loan, aes(x = term, y = loan_amnt, fill = loan$bad_loan, 
                 xlab = "1 = Bad Loan, 0 = Good loan", ylab = "Interest Rate")) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 40000))

ggplot(loan, aes(x = term, y = loan_amnt, fill = loan$bad_loan, 
                 xlab = "1 = Bad Loan, 0 = Good loan", ylab = "Interest Rate")) + 
  geom_bar() +
  coord_cartesian(ylim = c(0, 40000))

#### Continuous X, Continuous Y

ggplot(loan, aes(x = emp_length, y = loan_amnt)) + 
  geom_point() +
  facet_wrap(~bad_loan)

ggplot(loan, aes(x = emp_length, y = loan_amnt)) + 
  geom_jitter() +
  facet_wrap(~bad_loan)

ggplot(loan, aes(x = annual_inc, y = total_acc)) + 
  geom_jitter(alpha=1/20) +
  facet_wrap(~bad_loan)

(alpha = 1/20, shape =  21, fill = I('#F79420'))


LoanModel00 = glm (bad_loan ~ loan_amnt + annual_inc + dti + delinq_2yrs + revol_util +
                     total_acc + longest_credit_length + bankrpc_state_low +   
                     bankrpc_state_high  + homeown_mort + homeown_rent + 
                     term + purpose + vstatus_verified,
                   data=loan_train, family="binomial")

# The following variables will not be used: 
#
#             emp_length, bankrpc_state_med, 
#             bankrpc_state_medhigh, homeown_other