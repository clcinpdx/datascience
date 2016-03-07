# edX exercise for Unit 3, logistic regresssion

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L3 - Logistic Regression')

framingham  <- read.csv ("framingham.csv", sep = ',', header = TRUE)

str(framingham)

# use sample.split() function from the caTools package to split data for a classification problem
install.packages("caTools")
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
framinghamTrain = subset(framingham, split == TRUE)
framinghamTest = subset(framingham, split == FALSE)

# glm (dependent variable ~ independent variables, where "." selects all variables....)

framinghamLog = glm(TenYearCHD ~ ., 
                    data = framinghamTrain, family = binomial)
summary(framinghamLog)

# now we will use our model (framinghamLog) to make
# predictions on our test set

predictTest = predict(framinghamLog, type = "response", 
                      newdata = framinghamTest)
table (framinghamTest$TenYearCHD, predictTest > 0.5)

#     FALSE TRUE
#   0  1069    6
#   1   187   11


# This means that our model rarely predicts a 10-year CHD
# risk above 50%.

# What is the accuracy of this model?

# It's the sum of the cases we get right, 
# divided by the total number of observations in our data set

(1069 + 11) / (1069 + 6 + 187 + 11)

# So the accuracy of our model is about 84.8%. 

# We need to compare 
# this to the accuracy of a simple baseline method.
# The more frequent outcome in this case is 0,
# so the baseline method would always predict 0 or no CHD.
# This baseline method would get an accuracy of 1069

### Baseline Method
# total number of true negative cases,
# divided by the total number of observations in our data

(1066 + 6) / (1069 + 6 + 187 + 11)
# So the baseline accuracy of our model is about 84.2%

# But do we still have a valuable model by varying the threshold?
# Let's compute the out-of-sample AUC. 

# Selecting Thresholds
library (ROCR)

# A Receiver Operator Characteristic curve
ROCRpred = prediction(predictTest, framinghamTest$TenYearCHD)
value = as.numeric(performance(ROCRpred,"auc")@y.values)

# value = 0.7421095

# which means that the model can differentiate between low risk
# patients and high risk patients pretty well.

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot (ROCRperf)
plot (ROCRperf, colorize = TRUE)
plot (ROCRperf, colorize = TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Sensitivity is equal to the true positives divided by 
# the true positives plus the false negatives, and measures the 
# percentage of actual bad loan cases that we classify correctly. 
# This is often called the true positive rate.

FALSE	TRUE
0	1069	6
1	187	11

# The sensitivity is:
11/(11+187) 
# The specificity is 
1069/(1069+6)

# Specificity is equal to the true negatives divided by 
# the true negatives plus the false positives, and measures the 
# percentage of actual good loan cases that we classify correctly. 
# This is often called the true negative rate.

