
setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L4 - CART')
stevens = read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)

# install additional required packages/libraries
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library (rpart.plot)

StevensTree = rpart (Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 25)
prp(StevensTree)

PredictCart = predict(StevensTree, newdata = Test, type = "class")

# Now let's compute the accuracy of our model
# by building a confusion matrix.
# So we'll use the table function, and first give the true outcome

table(Test$Reverse, PredictCart)

# To compute the accuracy, we need to add up
# the observations we got correct, 41 plus 71, divided
# by the total number of observations in the table,
# or the total number of observations in our test set.
# So the accuracy of our CART model is 0.659.

