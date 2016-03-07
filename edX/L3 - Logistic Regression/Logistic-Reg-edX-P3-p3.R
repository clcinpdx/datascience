# edX exercise for Unit 3, logistic regresssion

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L3 - Logistic Regression')

polling  <- read.csv ("PollingData.csv", sep = ',', header = TRUE)

str(polling)

# Why are there 145 observations when there are three elections and 50 states 
# using table function, summarized by year, we see that in 2012 there were five missing
table (polling$Year)

2004 2008 2012 
50   50   45 

# next, using the summary function, we can 

summary(polling)


# use sample.split() function from the caTools package to split data for a classification problem
install.packages("caTools")
library(caTools)
set.seed(1000)
split = sample.split(polling$Republican, SplitRatio = 0.65)
PollingDataTrain = subset(polling, split == TRUE)
PollingDataTest = subset(polling, split == FALSE)
