# edX exercise for Unit 2, linear regresssion

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX')

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L2 - Linear Regression')

statedata  <- read.csv ("statedata.csv", sep = ',', header = TRUE)

head(statedata)
str(statedata)
summary(statedata) # statistical summary - range of values#


# Problem 1.1 - Data Exploration
# Plot all of the states' centers with latitude on the y axis  
# and longitude on the x axis (the "x" variable in our dataset). 

ggplot (aes(x=statedata$x, y=statedata$y), data = statedata) +
  geom_point()


# Problem 1.2 - Data Exploration 
# Using the tapply command, determine which region of the US 
# (West, North Central, South, or Northeast) has the highest 
# average high school graduation rate of all the states in the region

# tapply use case:
#   A dataset that can be broken up into groups
#   We want to break it up into groups
#   Within each group, we want to apply a function
# See http://www.r-bloggers.com/r-function-of-the-day-tapply/
# tapply(Summary Variable, Group Variable, Function)
  
tapply(statedata$HS.Grad, statedata$state.region, mean)

# Answer: The West has the highest gradution rate (62.00)


# Problem 1.3 - Data Exploration
# Make a boxplot of the murder rate by region 
# type ?boxplot in your console for help
# Which region has the highest median murder rate?

boxplot (statedata$Murder ~ statedata$state.region, data = statedata)

# the tilde "groups" the Murder data by the region

# Answer: The region with the highest median murder rate 
# (the one with the highest solid line in the box) is the South.


# Problem 1.4 - Data Exploration
# There is an outlier in the Northeast region of the boxplot 
# you just generated. Which state does this correspond to? 
# (Hint: There are many ways to find the answer to this question, 
# but one way is to use the subset command to only look at the 
# Northeast data.)

myboxplot <- boxplot (statedata$Murder ~ statedata$state.region, data = statedata)
myboxplot$out
# the value is 10.9, but from where did this come from?
names(myboxplot)
myboxplot$stats


NortheastData = subset(statedata, state.region == "Northeast")


boxplot (data = NortheastData,
         NortheastData$Murder ~ NortheastData$state.name)

# or

ggplot (aes(x=NortheastData$state.name, y=NortheastData$Murder),
        data = NortheastData) +
  geom_boxplot(aes(fill = 'Type'))



# Problem 2.1 - Predicting Life Expectancy - An Initial Model
# Build a model to predict life expectancy by state using the 
# state statistics we have in our dataset. Build the model with 
# all potential variables included 
# (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area). 
# Note that you should use the variable "Area" in your model, 
# NOT the variable "state.area".

# question: What is the coefficient for "Income" in your 
#          linear regression model?


LifeExp = lm ( Life.Exp ~ Population + Income + Illiteracy + Murder +
               HS.Grad + Frost + Area,
               data = statedata)

summary(LifeExp)

# Answer: Coefficient for Income = -2.180e-05 = 0.0000218
  
  
# Problem 2.2 - Predicting Life Expectancy - An Initial Model
# Call the coefficient for income x (the answer to Problem 2.1). 
# What is the interpretation of the coefficient x?

# Answer: If we increase income by one unit, then our model's prediction will increase by the coefficient of income, x. Because x is negative, this is the same as predicted life expectancy decreasing by |x|.


# Problem 2.3 - Predicting Life Expectancy - An Initial Model
# Now plot a graph of life expectancy vs. income using the command:
# plot(statedata$Income, statedata$Life.Exp)
plot(statedata$Income, statedata$Life.Exp)
#or
ggplot (aes (x=statedata$Income, y=statedata$Life.Exp), data = statedata) +
  geom_point()
# Quesion: Visually observe the plot. What appears to be the relationship?
# Answer: as income increases, so does life expectancy
#   Although the point in the lower right hand corner of the plot appears to 
#   be an outlier, we observe a positive linear relationship in the plot.

# Although income is an insignificant variable in the model, this
# does not mean that there is no association between income and 
# life expectancy. However, in the presence of all of the other 
# variables, income does not add statistically significant 
# explanatory power to the model. This means that 
# multicollinearity is probably the issue.


# Problem 3.1 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions
# Experiment with removing independent variables from the original model. Remember 
# to use the significance of the coefficients to decide which 
# variables to remove (remove the one with the largest "p-value" 
# first, or the one with the "t value" closest to zero), and to 
# remove them one at a time (this is called "backwards variable 
# selection"). This is important due to multicollinearity issues 
# - removing one insignificant variable may make another previously insignificant 
#variable become significant.

LifeExp = lm ( Life.Exp ~ Population + Income + Illiteracy + Murder +
                 HS.Grad + Frost + Area,
               data = statedata)

summary(LifeExp)

# with all variables, R-squared = 0.7362

# remove Area first, since it has the largest P value
LifeExp = lm ( Life.Exp ~ Population + Income + Illiteracy + Murder +
                 HS.Grad + Frost, data = statedata)

summary(LifeExp)

# without Area, R-squared = 0.7361 (very small decrease)
# remove Illiteracy next as its P value is 0.9340
LifeExp = lm ( Life.Exp ~ Population + Income + Murder +
                 HS.Grad + Frost, data = statedata)

summary(LifeExp)


# without Area and Illiteracy, R-squared = 0.7361 (no decrease)
# remove Income next as its P value is 0.9153
LifeExp = lm ( Life.Exp ~ Population + Murder +
                 HS.Grad + Frost, data = statedata)

summary(LifeExp)
# without Area, Illiteracy, and Income R-squared = 0.736 (sall decrease)

# This model with 4 variables is a good model. However, we can see 
# that the variable "Population" is not quite significant. In 
# practice, it would be up to you whether or not to keep the 
# variable "Population" or eliminate it for a 3-variable model. 
# Population does not add much statistical significance in the 
# presence of murder, high school graduation rate, and frost days.

# The fact that the coefficient for these removed variables is not
# zero in the intial model means it must be helping the R-squared
# value, even if it is only a very small improvement. So when we 
# force the variable to be removed, it will decrease the R-squared
# a little bit. However, this small decrease is worth it to have 
# a simpler model.


# Problem 3.3 - Predicting Life Expectancy - Refining the Model 
#               and Analyzing Predictions
# Using the simplified 4 variable model that we created, we'll now 
# take a look at how our predictions compare to the actual values.
# Take a look at the vector of predictions by using the predict 
# function (since we are just looking at predictions on the 
# training set, you don't need to pass a "newdata" argument to the predict function).

# Question: Which state do we predict to have the lowest life 
# expectancy? (Hint: use the sort function)

predictLife <- predict (LifeExp)

# minimum life expentancy
which.min(statedata$Life.Exp) 
# state 40 = South Carolina

# maximum life expentancy
which.max(statedata$Life.Exp) 
# state 11 = Hawaii

# look at residuals - which state has the smallest error
which.min(LifeExp$residuals) 
# state 19 = Hawaii

# look at residuals - which state has the largest error
which.max(LifeExp$residuals) 
# state 11 = Hawaii

########################################
## FORECASTING ELANTRA SALES
########################################


elantra  <- read.csv ("elantra.csv", sep = ',', header = TRUE)

head(elantra)
str(elantra)
summary(elantra) # statistical summary - range of values#

# Problem 1 - Loading the Data
# Load the data set. Split the data set into training and testing
# sets as follows: place all observations for 2012 and earlier 
# in the training set, and all observations for 2013 and 2014 
# into the testing set.

elantra_train = subset(elantra,elantra$Year < '2013')
elantra_test = subset(elantra,elantra$Year >= '2013')


# Problem 2.1 - A Linear Regression Model
# Build a linear regression model to predict monthly 
# Elantra sales using Unemployment, CPI_all, CPI_energy and 
# Queries as the independent variables. Use all of the training 
# set data to do this.

elantra_sales = lm (ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, 
                    data = elantra_train)
summary(elantra_sales)

# Answer: Multiple R-squared:  0.4282

# Problem 2.2 - Significant Variables
# How many variables are significant, or have levels that 
# are significant? Use 0.10 as your p-value cutoff.

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)   95385.36  170663.81   0.559    0.580
# Unemployment  -3179.90    3610.26  -0.881    0.385
# CPI_all        -297.65     704.84  -0.422    0.676
# CPI_energy       38.51     109.60   0.351    0.728
# Queries          19.03      11.26   1.690    0.101

elantra_sales_month = lm (ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, 
                    data = elantra_train)
summary(elantra_sales_month)

# now, capture Month as a factor variable

as.factor

elantra_train$Monthf <- as.factor(elantra_train$Month)
elantra_test$Monthf <- as.factor(elantra_test$Month)

elantra_sales_monthf = lm (ElantraSales ~ Monthf + Unemployment + CPI_all + CPI_energy + Queries, 
                          data = elantra_train)
summary(elantra_sales_monthf)

# Which of the following variables is CPI_energy highly correlated
# with? Select all that apply. (Include only variables where the absolute value of the correlation exceeds 0.6. 
cor (elantra)

# or 

cor(elantra_train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# Which of the following variables is Queries highly correlated 
# with? 

cor(elantra_train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# Answer: Unemployment, CPI_energy, CPI_all

# Problem 6.1 - A Reduced Model
# Let us now simplify our model (the model using the factor 
# version of the Month variable). We will do this by iteratively removing 
# variables, one at a time. Remove the variable with the highest 
# p-value (i.e., the least statistically significant variable) 
# from the model. Repeat this until there 
# are no variables that are insignificant or variables for which all 
# of the factor levels are insignificant. Use a threshold of 0.10 to determine whether a variable is significant.

elantra_sales_monthf = lm (ElantraSales ~ Monthf + Unemployment + CPI_all + CPI_energy + Queries, 
                           data = elantra_train)
summary(elantra_sales_monthf)

# first remove Queries
elantra_sales_monthf2 = lm (ElantraSales ~ Monthf + Unemployment + CPI_all + CPI_energy, 
                           data = elantra_train)
summary(elantra_sales_monthf2)

# Problem 6.2 - Test Set Predictions
# Using the model from Problem 6.1, make predictions on the 
# test set. What is the sum of squared errors of the model on the 
# test set?


## Making predictions and comparing to our test dataset using our PointsReg model
elantraPredictions = predict (elantra_sales_monthf2, newdata = elantra_test)

###########################################################
## Making Predictions
###########################################################
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
# And we see that we have an R-squared value of 0.6158505

summary(elantra_test)

# Step 2: Compute Root Mean Squared Errors
RMSE = sqrt(SSE/nrow(elantra_test))
RMSE

# results root mean squared error here is 196.37, a little larger than
# we had in our training dataset
