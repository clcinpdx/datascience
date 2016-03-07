#General Useful R/Rstudio Commands

############################################################
# How can I see what data sets are available when I start R?
data(package = .packages(all.available = TRUE))
############################################################

############################################################
# install packages

library("ggplot2", lib.loc="~/R/win-library/3.2")
install.packages("ggplot2")

library("gridExtra", lib.loc="~/R/win-library/3.2")
install.packages("gridExtra")

library("dplyr", lib.loc="~/R/win-library/3.2")
install.packages("dplyr")
############################################################

############################################################
# load data

getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/udacity')

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')

glimpse(pf)

############################################################

# The linetype parameter can take the values 0-6:
# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash

############################################################
# http://www.statmethods.net/input/datatypes.html


# use nrow when need to know the number of rows in a frame
nrow(dataframe)
# mean
mean(dataframe$variable)


###########################################################
## Making Predictions
###########################################################

## Making predictions and comparing to our test dataset using our PointsReg model
elantraPredictions = predict (elantra_sales_monthf2, newdata = elantra_test)

# from before, our R-squared value was 0.8193, which was a measure of 
# how well our model fit the training data
# Training model: elantra_sales_monthf = 
#                 lm (ElantraSales ~ Monthf + Unemployment + CPI_all + CPI_energy + Queries, 
#                 data = elantra_train)
# to get a measure of the predictions goodness of fit,
# we need to calculate the out of sample R-squared.

# Step 1: Compute the SSE and SST
SSE = sum((elantraPredictions - elantra_test$ElantraSales)^2)
# note: SSE calculation takes the linear regression model

SST = sum((mean(elantra$ElantraSales) - elantra_test$ElantraSales)^2)
R2 = 1 - SSE/SST
SSE
SST
R2

# Graphics errors

# Getting "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : 
# invalid graphics state" or invalid output
# http://stackoverflow.com/questions/20155581/persistent-invalid-graphics-state-error-when-using-ggplot2

dev.off()


#########################################
# Splitting a dataset in test and train #
#########################################
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


# syntax for the ggplot 

ggplot (aes(x=quality$OfficeVisits, y=quality$Narcotics),
        data = quality) +
  geom_point(aes(color=factor(PoorCare)))


