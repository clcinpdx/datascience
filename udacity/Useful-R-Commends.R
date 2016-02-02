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