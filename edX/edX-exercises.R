
############################################################
# install packages
############################################################
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("tidyr")

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX')

wine  <- read.csv ("wine.csv", sep = ',', header = TRUE)

head(wine)
str(wine)
summary(wine) # statistical summary - range of values#


# Model one: One variable linear regression model using average growing season tempurature to predict price
# using function lm (linear regression model)
# See ?lm for help

model1 = lm (Price ~ AGST, data = wine)

summary (model1)

model1$residuals

SSE = sum(model1$residuals)
SSE

# adding a second independent variable, HarvestRain to
# see 

model2 = lm (Price ~ AGST + HarvestRain, data = wine)
summary (model2)

# adding a five "independent variables"

model3 = lm (Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary (model3)

# adding a two "independent variables"
model4 = lm (Price ~ HarvestRain + WinterRain, data = wine)
summary (model4)
SSE = sum(model4$residuals)
SSE

# adding just two "independent variables"

model4b = lm (Price ~ HarvestRain + WinterRain, data = wine)
summary (model4b)

# lets compute the correlation between a pair of variables using cor
# specifically, between winter rain and price and age and population

cor (wine$WinterRain, wine$Price)
cor (wine$Age, wine$FrancePop)
cor (wine)
cor (wine$HarvestRain,wine$WinterRain)

# adding a four "independent variables", removing two that were interdependent (age and FrancePop)

model5 = lm (Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary (model5)

pwd()


winetest <- read.csv("wine_test.csv")
summary (winetest)
str(winetest)

predictTest <- predict(model5, newdata = winetest)
predictTest

# comparing the values in winetest to the prediction values is pretty good. How do we confirm
# that they are good? Let's calculate first the Sum of Squared Errors (SSE), 
# then the Sum of Squared Total (SST)

SSE = sum((winetest$Price - predictTest)^2) 
SST = sum((winetest$Price - mean(wine$Price))^2) 

Rsquared = 1 - SSE/SST


# switching to the baseball / moneyball discussion

baseball  <- read.csv ("baseball.csv", sep = ',', header = TRUE)

head(baseball)
str(baseball)
summary(baseball) # statistical summary - range of values#

moneyball = subset (baseball,Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA

# plot run difference (RD) against Wins (W) to confirm there is a linear 
# relationship between the two variables

# enhanced with colors to indicate playoff success

ggplot (aes(x=moneyball$RD, y=moneyball$W), data = moneyball) +
  geom_point(aes(color = moneyball$Playoffs)) +
  geom_hline(aes(yintercept=95), color='blue') +
  geom_line()

#create a linear regression model

WinsReg = lm(W~RD,data=moneyball)
summary (WinsReg)

# from which we see that there is a strong relationship between
# run difference and wins - R square is high (.88)

# now, can use this model to confirm the claim made in Moneyball 
# that a team needs to score at least 135 more runs than they
# allow to win at least 95 games.          

RunsReg = lm(RS ~ OBP + SLGsus, data = moneyball)
summary(RunsReg)

# the above data indicates that BA may not be a good variable
# remove Batting Average (BA)
RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)



teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor (teamRank, wins2012)
cor (teamRank, wins2013)


# Basketball

NBA = read.csv(file='NBA_train.csv',sep =',',header=TRUE)
str(NBA)

# how many games does a team have to win to get into the playoffs?

# a quick graph makes it appear that it must be around 40 wins
ggplot (aes(x=NBA$PTS, y=NBA$W), data = NBA) +
  geom_point(aes(color = NBA$Playoffs)) +
  geom_hline(aes(yintercept=40), color='blue') 

summary (NBA)

table(NBA$W,NBA$Playoffs)
# based on this view, we estimate that a team needs to win
# about 43 games

#Can we use the difference between points scored
# and points allowed throughout the regular season in order
# to predict the number of games that a team will win?

NBA$PTSdiff = NBA$PTS - NBA$oppPTS


# Let's first make a scatter plot to see if it looks like 
# there's a linear relationship between the number of wins 
# that a team wins and the point difference.

ggplot (aes(x=NBA$PTSdiff, y=NBA$W), data = NBA) +
  geom_point(aes(color = NBA$Playoffs)) +
  geom_hline(aes(yintercept=43), color='blue') 

# yes, there is a stong positive relationship between winning and 
# point difference being non-negative

WinsReg = lm (W~PTSdiff, data = NBA)
summary (WinsReg)
str(NBA)

# The very high R-squared value (0.9423) indicates there is a highly related

PointsReg = lm (PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary (PointsReg)

# results in a view where there are numerous signficant variables,
# a few, like DRB, TOV, and BLK that do not seem to be significant

# Calculate the residuals and then the SSE (square of sum of standard errors)
PointsReg$residuals

# Sum of squares = sum of the residuals squared
SSE = sum(PointsReg$residuals^2)
SSE

# SSE not very easy to interpret. It would be more useful to calcuate
# the root mean square error. 

RMSE = sqrt (SSE/nrow(NBA))
RMSE

# which results in 184.4, which seems like a lot but when compared
# to the mean (average) number of points scored in a season

mean(NBA$PTS)

# you get 8370, which means we have a decent model, but let's see if we 
# can improve our model by removing less/insignificant variables


PointsReg = lm (PTS~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary (PointsReg)

# remove DRB
PointsReg = lm (PTS~ X2PA + X3PA + FTA + AST + ORB + TOV + STL + BLK, data = NBA)
summary (PointsReg)
SSE = sum(PointsReg$residuals^2)
RMSE = sqrt (SSE/nrow(NBA))
RMSE
# removing DRB increased the RMSE from 184.4049 to 184.4493

# remove TOV: Note: This should have been first as its P value was the highest
# and thus it was the least signficant
PointsReg = lm (PTS~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary (PointsReg)
SSE = sum(PointsReg$residuals^2)
RMSE = sqrt (SSE/nrow(NBA))
RMSE
# removing TOV decreased the RMSE from 184.4678, R-squared = 0.8991

# remove BLK 
PointsReg = lm (PTS~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary (PointsReg)
SSE = sum(PointsReg$residuals^2)
RMSE = sqrt (SSE/nrow(NBA))
RMSE
# removing BLK decreased the RMSE from 184.493, R-squared = 0.8991

# the fact that removing these four has little impact on R squared
# is a good sign that we could safely remove those less signficant factors

## Making predictions and comparing to our test dataset using our PointsReg model
NBA_test = read.csv("NBA_test.csv")
PointsPredictions = predict (PointsReg, newdata = NBA_test)

###########################################################
## Making Predictions
###########################################################
# from before, our R-squared value was 0.8991, which was a measure of 
# how well our model fit the training data
# Training model: PointsReg = lm (PTS~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)

# to get a measure of the predictions goodness of fit,
# we need to calculate the out of sample R-squared.

# Step 1: Compute the SSE and SST
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE/SST
R2
# And we see that we have an R-squared value of 0.8127

# Step 2: Compute Root Mean Squared Errors
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE

# results root mean squared error here is 196.37, a little larger than
# we had in our training dataset










