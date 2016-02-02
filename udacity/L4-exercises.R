# www.udacity.com course UD651

############################################################
# install packages
############################################################
library("ggplot2", lib.loc="~/R/win-library/3.2")
install.packages("ggplot2")

library("gridExtra", lib.loc="~/R/win-library/3.2")
install.packages("gridExtra")

library("dplyr", lib.loc="~/R/win-library/3.2")
install.packages("dplyr")

############################################################
# load pseuo_facebook data
############################################################
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/udacity')

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')





glimpse(pf)

#L4,p3
qplot(x=age,y=friend_count,data=pf)

# L4, p4 (transparency and jitter to better reflect age being continuous)
# geom_point = scatter plot
# geom_jitter = help with overplotting that occurs when a lot of data is close 
#               to one another. Adds some "noise" and makes plots looks more disperse 
# alpha = sets the transparency so as to see layers more  1/20 means that it 
#         will take 20 dots to be as dark as a currently displayed dot

ggplot(aes(x=age,y=friend_count),data=pf,ylab='friends') +
  geom_jitter(alpha=1/20) +
  xlim(13,90)

# L4, p5 (coord_trans) : generated plot with two continuous variables
ggplot(aes(x=age,y=friend_count),data=pf,ylab='friends') +
  geom_point(alpha=1/20) +
  xlim(13,90) +
  coord_trans(y = "sqrt")
  
#L4, p6 : Alpha and Jitter

# friends initiated and age
qplot(x=age,y=friendships_initiated,data=pf)

ggplot(aes(x=age,y=friendships_initiated),data=pf) +
  geom_point(alpha=1/20) +
  xlim(13,90)

# using Jitter, "Y" jitter position constrained to prevent negative Y numbers 
ggplot(aes(x=age,y=friendships_initiated),data=pf) +
  geom_jitter(alpha=1/20, position = position_jitter(height = 0)) +
  xlim(13,90) +
  ylim (0,5000) +
  coord_trans(y = "sqrt")

#L4, p9
install.packages('dplyr')
library(dplyr)

# unchained technique
age_groups <- group_by(pf,age)
pf.fc_by_age <- summarize (age_groups,
                           friend_count_mean = mean(friend_count),
                           friend_count_median = median (friend_count),
                           n = n())
pf.fc_by_age <- arrange (pf.fc_by_age, age)
pf.fc_by_age

# chaining, using the %>% dplyr function
rm (pf.fc_by_age)

pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarize (friend_count_mean = mean(friend_count),
             friend_count_median = median(friend_count),
             n = n()) %>%
  arrange(age)

head(pf.fc_by_age)

# visualize this data: Plot mean friend count vs. Age
# using geom_line()
# http://docs.ggplot2.org/0.9.3/geom_line.html

ggplot(aes(x=age,y=friend_count_mean),data=pf.fc_by_age) +
  geom_line() +
  xlim(13,65) +
  coord_trans(y = "log10")

# two plots combined, jitter and mean

ggplot(aes(x=age,y=friendships_initiated),data=pf) +
  coord_cartesian(xlim = c(13, 90)) +
  geom_jitter(alpha=1/20, 
              position = position_jitter(height = 0),
              color = 'red') +
  coord_trans(y = "sqrt") +
  geom_line (stat = 'summary', fun.y = mean)

# two plots combined, jitter and mean with quantiles 10 and 90
# note: updated geom_line sytax

ggplot(aes(x=age,y=friendships_initiated),data=pf) +
  coord_cartesian(xlim = c(13, 90)) +
  geom_jitter(alpha=1/20, 
              position = position_jitter(height = 0),
              color = 'orange') +
  coord_trans(y = "sqrt") +
  geom_line (stat = 'summary', fun.y = mean) +
  geom_line (stat = 'summary', fun.y = quantile, 
            fun.args = list ( probs = .1), linetype=2, color='blue') +
  geom_line (stat = 'summary', fun.y = quantile,
            fun.args = list ( probs = .9), linetype = 2, color = 'blue') +
  geom_line (stat = 'summary', fun.y = quantile,
             fun.args = list ( probs = .5), linetype = 1, color = 'blue') 

# removing sqrt so as to scale Y axis

ggplot(aes(x=age,y=friendships_initiated),data=pf) +
  coord_cartesian(xlim = c(13, 70), ylim=c(0,1000)) +
  geom_jitter(alpha=1/20, 
              position = position_jitter(height = 0),
              color = 'orange') +
  geom_line (stat = 'summary', fun.y = mean) +
  geom_line (stat = 'summary', fun.y = quantile, 
             fun.args = list ( probs = .1), linetype=2, color='blue') +
  geom_line (stat = 'summary', fun.y = quantile,
             fun.args = list ( probs = .9), linetype = 2, color = 'blue') +
  geom_line (stat = 'summary', fun.y = quantile,
             fun.args = list ( probs = .5), linetype = 1, color = 'blue') 
  

#L4, p12
# https://www.udacity.com/course/viewer#!/c-ud651/l-755298985/e-865168749/m-876198587

## for documation, type "?cor.test"
## Test for Association/Correlation Between Paired Samples:
## cor.test((x, y,
##          alternative = c("two.sided", "less", "greater"),
##          method = c("pearson", "kendall", "spearman"),
##          exact = NULL, conf.level = 0.95, continuity = FALSE, ...))

######################################################################
# What is the correlation between age and friend count (Pearson's )
# -0.3 > correlation > 0.3 is meaningful, but small. 
# Around 0.5 is moderate, and 0.7 and greater is large
######################################################################

cor.test (pf$age,pf$friend_count,method="pearson")
#or
with (pf,cor.test(age,friend_count,method='pearson'))

# subset to remove likely outliers
with (subset(pf, age <= 70), cor.test(age, friend_count, method='pearson'))

# L4 p15 - Scatterplots
# Create a scatterplot of likes_received (y) vs. www_likes_received (x). 

ggplot(aes(x=www_likes_received,y=likes_received),data=pf) +
  geom_point() +
  xlim (0, quantile(pf$www_likes_received, 0.95)) +
  ylim (0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm', color = 'red')
  coord_cartesian(xlim = c(0, 1500), ylim = c(0,2500)) 
  
# Strong correlation - obviously these should be correlated  
with (pf,cor.test(www_likes_received,likes_received,method='pearson'))



# L4 p18 - Correlation
# Create a scatterplot of temperature (Temp) vs. months (Month).
install.packages('alr3')
library('alr3')
data('Mitchell',package = 'alr3')

ggplot(data=Mitchell, aes(x = Month, y = Temp)) + geom_point() 
with (Mitchell,cor.test(Month,Temp, method='pearson')) 


# group by month
?ggplot

ggplot(data=Mitchell, aes(x = Month, y = Temp)) + 
  geom_point() + 
  scale_x_discrete(breaks = seq(0,203,12))

# more obvious relationship using line
ggplot(data=Mitchell, aes(x = Month, y = Temp)) + 
  geom_line() + 
  scale_x_discrete(breaks = seq(0,203,12))

# L4 p22 - Understanding Noise: Age to Age Months
# Note: requires friend_count_mean, see earlier in L4

ggplot (aes(x=age, y = friend_count_mean), data = pf.fc_by_age) +
    geom_line()

## create an age_with_months variable
rm (pf.age_with_months)

pf$age_with_months <- pf$age + (1 - (pf$dob_month/12)) 

glimpse (pf)

ggplot (aes(x=age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()

pf.fc_by_age_months <- pf %>%
  group_by(age_with_months) %>%
  summarize (friend_count_mean = mean(friend_count),
             friend_count_median = median(friend_count),
             n = n()) %>%
  arrange(age_with_months)

head(pf.fc_by_age_months)

# Create a new scatterplot showing friend_count_mean
# versus the new variable, age_with_months. Subset the data to investigate
# users with ages less than 71

ggplot (aes(x = age_with_months, y = friend_count_mean), 
        data = subset(pf.fc_by_age_months, age_with_months < 71)) +
  geom_line()

rm(p1)

p1 <- ggplot (aes(x=age, y = friend_count_mean), 
              data = subset(pf.fc_by_age, age < 71)) +
     geom_line()

  
p2 <- ggplot (aes(x = age_with_months, y = friend_count_mean), 
             data = subset(pf.fc_by_age_months, age_with_months < 71)) +
     geom_line()

grid.arrange(p2,p1,ncol=1)

