# edX exercise for Unit 3, logistic regresssion

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L3 - Logistic Regression')

quality  <- read.csv ("quality.csv", sep = ',', header = TRUE)

head(quality)
str(quality)
summary(quality) # statistical summary - range of values#

ggplot (aes(x=quality$OfficeVisits, y=quality$Narcotics),
        data = quality) +
  geom_point(aes(color=factor(PoorCare)))

table(quality$PoorCare)
#  0  1 
#  98 33 

# Baseline - predict the most frequent outcome, good or poor care
mean('quality$PoorCare')

# Baseline = 98/131 = 0.7480916
# this is what we will try to beat with our logistic regresssion

# use sample.split() function from the caTools package to split data for a classification problem
install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
# Build a logistic regression model to predict PoorCare using the R :
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog) 


#################################################################
# split a data frame data, where the dependent variable is a continuous outcome (this was the case for all the datasets we used last week), you could instead use the sample() function. 
## create a training and test sample set
smp_size <- floor(0.75 * nrow(quality))

## set the seed to make partition reproductible
set.seed(88)
train_ind <- sample(seq_len(nrow(quality)), size = smp_size)

qualityTrain <- quality [train_ind, ]
qualityTest <- quality [-train_ind, ]

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, 
                 data = qualityTrain, family = binomial)
summary(QualityLog)
##################################################################
predictTrain = predict(QualityLog,type="response")
table(qualityTrain$PoorCare,predictTrain>0.5)
table(qualityTrain$PoorCare,predictTrain>0.7)
table(qualityTrain$PoorCare,predictTrain>0.2)

# Selecting Thresholds
install.packages("ROCR")
library (ROCR)

# A Receiver Operator Characteristic curve
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot (ROCRperf)
plot (ROCRperf, colorize = TRUE)
plot (ROCRperf, colorize = TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

#### Unit 3: Logistic Regression > Modeling the Expert: An Introduction to Logistic Regression 

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, 
                 data = qualityTrain, family = binomial)

predictTest = predict(QualityLog, type="response", newdata=qualityTest)

You can compute the test set AUC by running the following two commands in R:
  
  ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
