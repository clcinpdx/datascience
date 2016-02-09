#R-Scripts exploring Capstone Dataset

############################################################
# install packages
############################################################
library("ggplot2", lib.loc="~/R/win-library/3.2")
install.packages("ggplot2")

library("gridExtra", lib.loc="~/R/win-library/3.2")
install.packages("gridExtra")

library("dplyr", lib.loc="~/R/win-library/3.2")
install.packages("dplyr")

library("tidyr", lib.loc="~/R/win-library/3.2")
install.packages("tidyr")

library("stringr", lib.loc="~/R/win-library/3.2")
install.packages("stringr")

############################################################
# set working directory and load data
############################################################
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/capstone')

loan  <- read.csv ("loan.csv", sep = ',', header = TRUE)

#############################################################################################
# 1. Split the dataset into training and the test sets.
#############################################################################################

## 75% of the sample size
smp_size <- floor(0.75 * nrow(loan))

## set the seed to make partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(loan)), size = smp_size)

ltrain <- loan [train_ind, ]
ltest <- loan [-train_ind, ]


#############################################################################################
# 2. Exploration
#############################################################################################


# various views of the loan data
dim(loan)
View(loan)
summary(loan)

qplot(x=loan_amnt,y=annual_inc,data=loan)


ggplot (aes(x=loan_amnt,y=annual_inc),data=loan) +
  geom_point(alpha= 0.1) +
  geom_line(linetype = 1, stat = 'summary', fun.y = mean, color = 'blue') +
  coord_cartesian(xlim = c(0, 35000), ylim = c(0,185000)) +
  facet_grid(~bad_loan)

ggplot (aes(x=loan_amnt,y=purpose),data=loan) +
  geom_point(alpha= 0.1) +
  coord_cartesian(xlim = c(0, 35000)) +
  facet_grid(~bad_loan)

ggplot (aes(x=loan_amnt),data=loan) +
  geom_histogram () +
  coord_cartesian(xlim = c(0, 35000)) +
  facet_grid(~bad_loan)



