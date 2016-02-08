############################################################
# load pseuo_facebook data
############################################################
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/udacity')

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')


# Write code to create a new data frame called 'pf.fc_by_age_gender', that contains
# information on each age AND gender group.
# The data frame should contain the following variables:
#    mean_friend_count,
#    median_friend_count,
#    n (the number of users in each age and gender grouping)

# note 1: You can include multiple variables to split the data frame 
# when using group_by() function in the dplyr package. 
# using chained commands... 
#   new_data_frame <- data_frame %.% 
#     group_by(variable1, variable2) %.% 

# note 2: Summarize removes on layer of grouping
#
# note 3: filter(!is.na(gender)) removes entries where gender is N/A

pf.fc_by_age_gender <- pf %>%
  filter (!is.na(gender)) %>%
  group_by(age,gender) %>%
  summarize (mean_friend_count = mean(friend_count),
             median_friend_count = median(friend_count),
             n = n()) %>%
  ungroup () %>%
  arrange(age)

head(pf.fc_by_age_gender)

# L5, p5
# Use pf.fc_by_age_gender to create a line graph showing the
# median friend count over the ages for each gender. 

# using pf.fc_by_age_gender summary data frame created earlier
ggplot (aes(x=age, y = median_friend_count), 
        data = subset(pf.fc_by_age_gender, !is.na(gender))) +
        geom_line( aes(color = gender)) 

# without using summary data frame
ggplot(aes(x = age, y = friend_count), 
       data = subset(pf, !is.na(gender))) +   
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)


# L5, p7: Reshaping Data using reshape2 package in "wide format"

# note: dcast used because we want a data frame as the output. 
# note: acast for array

install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender.wide <- dcast (pf.fc_by_age_gender,
                                   age ~ gender,
                                   value.var = 'median_friend_count')

View(pf.fc_by_age_gender.wide)

# Note: We could also create a similar data frame using the dplyr and tidyr packages: 
  pf.fc_by_age_gender.wide <- subset(pf.fc_by_age_gender[c('age', 'gender', 'median_friend_count')],                   !is.na(gender)) %>% 
  spread(gender, median_friend_count) %>% 
  mutate(ratio = male / female) 


# L5,p8 : Ratio Plot
# Plot the ratio of the female to male median friend counts using the data frame
# pf.fc_by_age_gender.wide.
  
  ggplot (aes(x=age, y = (female/male) ), 
          data = pf.fc_by_age_gender.wide) +
    geom_line(aes(color='red')) +
    geom_hline (aes(yintercept=1),linetype = 2)
  
# L5,p9 : Third Quantitative Variable
# Create a variable called year_joined in the pf data frame using the variable
# tenure (recorded in days) and 2014 as the reference year.

# note 1: to add a variable to an existing dataframe, use the following syntax: 
#   dataFrame$newColumn <- dataFrame$oldColumn1 + dataFrame$oldColumn2

  # note 2: use floor command to round down to nearest year. Ceiling is to round up
 
  pf$year_joined <- floor(2014 - (pf$tenure / 365))
  
  head(pf)

 
 # L5, p10: Cut a Variable
 
 # Create a new variable in the data frame called year_joined.bucket by using
 # the cut function on the variable year_joined.
 # You need to create the following buckets for the new variable, year_joined.bucket
 #        (2004, 2009]
 #        (2009, 2011]
 #        (2011, 2012]
 #        (2012, 2014]
 # Note that a parenthesis means exclude the year and a bracket means include the year.

 
# Note: What situation is cut useful in?
# In many data analysis settings, it might be useful to break up a 
# continuous variable such as age into a categorical variable. 
# Or, you might want to classify a categorical variable like year into a 
# larger bin, such as 1990-2000. Good for simple displays of demographic 
# data in tables. 
# see http://www.r-bloggers.com/r-function-of-the-day-cut/
 
  ## basic usage of cut with a numeric variable
  ##   c1 <- cut(clinical.trial$age, breaks = 4)
 
  ## if year.enroll is a factor, so must convert to numeric first!
  ##   c2 <- cut(as.numeric(as.character(clinical.trial$year.enroll)),
  ##             breaks = 3) 
 
 # check to see if tenure is a factor or a numeric
 class(pf$year_joined)  
 
 ## specify break points explicitly using seq function
 ## look what seq does  
     seq(30, 80, by = 10)
     #        (2004, 2009]
     #        (2009, 2011]
     #        (2011, 2012]
     #        (2012, 2014]
 

 pf.year_joined.bucket <- cut (as.numeric(pf$year_joined), 
            breaks = c(2004,2009,2011,2012,2014))
 
table (pf.year_joined.bucket)



# L5, p11:  Plotting It All Together
# Create a line graph of friend_count vs. age
# so that each year_joined.bucket is a line
# tracking the median user friend_count across
# age. This means you should have four different
# lines on your plot.

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_line(aes(color = pf$year_joined.bucket), 
            stat = 'summary', fun.y = median)


# L5, p12: Plot the Grand Mean

# (1) Add another geom_line to code below
# to plot the grand mean of the friend count vs age.
# (2) Use a different line type for the grand mean.

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_line(aes(color = pf$year_joined.bucket), 
            stat = 'summary', fun.y = mean) +
  geom_line(linetype = 2, stat = 'summary', fun.y = mean)

# L5, p13: Friending Rate
# how many friends does each user have since they started using the service

with (subset(pf, pf$tenure > 0), summary(friend_count/tenure))

# L5, p14: Friendships Initiated
# Create a line graph of mean of friendships_initiated per day (of tenure)
# vs. tenure colored by year_joined.bucket.
# You need to make use of the variables tenure,friendships_initiated, and year_joined.bucket.
# You also need to subset the data to only consider user with at least
# one day of tenure.


ggplot(aes(x = tenure, y = (friendships_initiated / tenure)), 
       data = pf) + 
  geom_line(aes(color = pf$year_joined.bucket), 
            stat = 'summary', fun.y = mean) 



# L5, p14: Bias Variance Trade off Revisited

ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = pf) +
  geom_smooth(aes(color = pf$year_joined.bucket))



# L5, p18:  Histograms Revisited
## load yogurt data set
## https://s3.amazonaws.com/udacity-hosted-downloads/ud651/yogurt.csv

yo <- read.csv('yogurt.csv',sep = ',', header=TRUE)
str(yo)

# change the id from an INT to a FACTOR
yo$id <- factor(yo$id)
str(yo)

qplot (yo$price, geom="histogram")

# L5, p19: Number of purchase
# Create a new variable called all.purchases,
# which gives the total counts of yogurt for
# each observation or household.
# dataFrame <- transform(dataFrame, newColumnName = some equation)
# So, to get the sum of two columns and store that into a new column with transform(), you would use code such as:
# dataFrame <- transform(dataFrame, newColumn = oldColumn1 + oldColumn2)

yo <- transform (yo ,all.purchases = (yo$strawberry + yo$blueberry + 
                                      yo$pina.colada + yo$plain + yo$mixed.berry))

qplot (x=all.purchases, data = yo, binwidth = 1)

# L5, p20: Prices over time
# Create a scatterplot of price vs time.

ggplot (aes(x=time, y = price), data = yo) +
  geom_jitter(alpha = 1/20, shape =  21, fill = I('#F79420'))

# L5, p22: Looking at Samples of Households

# set the seed so as to ensure reproducable results
set.seed (4230)
sample.ids <- sample (levels(yo$id),16)

ggplot (aes (x = time, y = price), 
        data = subset (yo, id %in% sample.ids)) +
        facet_wrap(~id) +
        geom_line() +
        geom_point(aes(size=all.purchases),pch = 1)


# L5, 23: Scatterplot matrix


#install.packages(GGally)
library (GGally)
theme_set(theme_minimal(20))

# set the see for reproducable results.
set.seed (1836)
pf_subset <-pf[,c(2:15)] #notice limiting the variables 2 through 15, removing 1 and 16
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset),1000),])


         