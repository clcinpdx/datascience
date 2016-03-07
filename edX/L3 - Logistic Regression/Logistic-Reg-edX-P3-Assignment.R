# Read in data

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L3 - Logistic Regression')


baseball = read.csv("baseball.csv")
str(baseball)
table(baseball$Team)
summary(baseball)

# Problem 1.1 - Limiting to Teams Making the Playoffs
# Each row in the baseball dataset represents a team in a particular year.
# How many team/year pairs are there in the whole dataset?
nrow(baseball)

# Problem 1.2 - Limiting to Teams Making the Playoffs
# Though the dataset contains data from 1962 until 2012, we 
# removed several years with shorter-than-usual seasons. Using the 
# table() function, identify the total number of years included 
# in this dataset.

years = table(baseball$Year)
length(years)

# Problem 1.3 - Limiting to Teams Making the Playoffs
# Because we're only analyzing teams that made the playoffs, 
# use the subset() function to replace baseball with a data frame 
# limited to teams that made the playoffs (so your subsetted data 
# frame should still be called "baseball"). How many team/year pairs 
# are included in the new dataset?

baseball <- subset (baseball, baseball$Playoffs == 1)

# Problem 1.4 - Limiting to Teams Making the Playoffs
# Through the years, different numbers of teams have been invited to 
# the playoffs. Which of the following has been the number of teams 
# making the playoffs in some season? Select all that apply.

table(baseball$Year)

# A fancier approach would be to use 
table(table(baseball$Year))



# Problem 2.1 - Adding an Important Predictor
# Just as we can use the names() function to get the names of a data 
# frame's columns, we can use it to get the names of the entries in a 
# table. What best describes the output of names(PlayoffTable)?


PlayoffTable = table(baseball$Year)
PlayoffTable
str(names(PlayoffTable)) 

# Problem 2.2 - Adding an Important Predictor
# Given a vector of names, the table will return a vector of frequencies.
# Which function call returns the number of playoff teams in 1990 and 
# 2001? 

years = PlayoffTable[c("1990", "2001")]
years

# Because PlayoffTable is an object and not a function, we look up 
# elements in it with square brackets instead of parentheses. We build 
# the vector of years to be passed with the c() function. Because the 
# names of PlayoffTable are strings and not numbers, we need to pass 
# "1990" and "2001".

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

table (baseball$NumCompetitors)


# Problem 3.1 - Bivariate Models for Predicting World Series Winner
# In this problem, we seek to predict whether a team won the World 
# Series; in our dataset this is denoted with a RankPlayoffs value of 1. 
# Add a variable named WorldSeries to the baseball data frame, 
# by typing the following command in your R console:

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table (baseball$WorldSeries)


# Problem 3.2 - Bivariate Models for Predicting World Series Winner
# When we're not sure which of our variables are useful in predicting a 
# particular outcome, it's often helpful to build bivariate models, which 
# are models that predict the outcome using a single independent variable. 
# We'll define an independent variable as significant if there is at 
# least one star at the end of the coefficients row for that variable 
# (this is equivalent to the probability column having a value smaller 
# than 0.05)

mod = glm (WorldSeries~Year+RS+RA+W+OBP+SLG+BA+RankSeason+OOBP+OSLG+NumCompetitors+League,data=baseball, family="binomial")
summary(mod)
mod1 = glm(WorldSeries~Year, data=baseball, family="binomial")
summary(mod1)
mod2 = glm(WorldSeries~RS, data=baseball, family="binomial")
summary(mod2)
mod3 = glm(WorldSeries~RA, data=baseball, family="binomial")
summary(mod3)
mod4 = glm(WorldSeries~W, data=baseball, family="binomial")
summary(mod4)
mod5 = glm(WorldSeries~OBP, data=baseball, family="binomial")
summary(mod5)
mod6 = glm(WorldSeries~SLG, data=baseball, family="binomial")
summary(mod6)
mod7 = glm(WorldSeries~BA, data=baseball, family="binomial")
summary(mod7)
mod8 = glm(WorldSeries~RankSeason, data=baseball, family="binomial")
summary(mod8)
mod9 = glm(WorldSeries~OOBP, data=baseball, family="binomial")
summary(mod9)
mod10 = glm(WorldSeries~OSLG, data=baseball, family="binomial")
summary(mod10)
mod11 = glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")
summary(mod11)
mod12 = glm(WorldSeries~League, data=baseball, family="binomial")
summary(mod12)

# Problem 4.1 - Multivariate Models for Predicting World Series Winner
# In this section, we'll consider multivariate models that combine the 
# variables we found to be significant in bivariate models. Build a 
# model using all of the variables that you found to be significant in 
# the bivariate models. How many variables are significant in the 
# combined model?

mod = glm (WorldSeries~Year+SLG+RankSeason+NumCompetitors,data=baseball, family="binomial")
summary(mod)

# Looking at summary(LogModel), you can see that none of the variables are significant 
# in the multivariate model!

# Problem 4.2 - Multivariate Models for Predicting World Series Winner
# Often, variables that were significant in bivariate models are no 
# longer significant in multivariate analysis due to correlation 
# between the variables. Which of the following variable pairs have 
# a high degree of correlation (a correlation greater than 0.8 or 
# less than -0.8)? Select all that apply.

# Year+SLG+RankSeason+NumCompetitors

cor(baseball$Year,baseball$SLG)
cor(baseball$Year,baseball$RankSeason)
cor(baseball$Year,baseball$NumCompetitors)
cor(baseball$SLG,baseball$RankSeason)
cor(baseball$SLG,baseball$NumCompetitors)
cor(baseball$RankSeason,baseball$NumCompetitors)

# As a shortcut, you can compute all pair-wise correlations between these variables with:
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])


# Problem 4.3 - Multivariate Models for Predicting World Series Winner
# Build all six of the two variable models listed in the previous 
# problem. Together with the four bivariate models, you should have 10 different logistic regression models. 
# Which model has the best AIC value (the minimum AIC value)?

modtwo1 = glm (WorldSeries~Year+RA,data=baseball, family="binomial")
summary(modtwo1)
#AIC: 233.88

modtwo2 = glm (WorldSeries~Year+RankSeason,data=baseball, family="binomial")
summary(modtwo1)
#AIC: 233.88

modtwo3 = glm (WorldSeries~Year+NumCompetitors,data=baseball, family="binomial")
summary(modtwo3)
#AIC: 232.9

modtwo4 = glm (WorldSeries~RA+RankSeason,data=baseball, family="binomial")
summary(modtwo4)
#AIC: 238.22

modtwo5 = glm (WorldSeries~RA+NumCompetitors,data=baseball, family="binomial")
summary(modtwo5)
#AIC: 232.74

modtwo6 = glm (WorldSeries~RankSeason+NumCompetitors,data=baseball, family="binomial")
summary(modtwo6)
#AIC: 232.52

mod1 = glm(WorldSeries~Year, data=baseball, family="binomial")
summary(mod1)
#AIC: 232.35

mod3 = glm(WorldSeries~RA, data=baseball, family="binomial")
summary(mod3)
#AIC: 237.88

mod8 = glm(WorldSeries~RankSeason, data=baseball, family="binomial")
summary(mod8)
#AIC: 238.75

mod11 = glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")
summary(mod11)
#AIC: 230.96 