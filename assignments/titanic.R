## Craig Calder
# Data Wrangling Exercise 1: Basic Data Manipulation

############################################################
# 1. set working directory and load data
############################################################

getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/assignments')

# create the loan dataframe from the loan.csv data file
titanic  <- read.csv ("titanic3.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)

str (titanic)

# 1: Port of embarkation
# The embarked column has one missing value, which is known to correspond to a passenger who actually 
# embarked at Southampton. Find the missing value and replace it with S.

# recode "S" to missing for variable embarked
titanic_embarked_NA <- subset(titanic,titanic$embarked=="")
titanic_embarked <- subset(titanic,!titanic$embarked=="")
titanic_embarked_NA$embarked <- c("S")
titanic <- rbind(titanic_embarked_NA,titanic_embarked)
remove (titanic_embarked_NA,titanic_embarked)


# 2: Age
# You'll notice that a lot of the values in the Age column are missing. While there are many ways to fill these missing values, using the mean or median of the 
# rest of the values is quite common in such cases. Calculate the mean of the Age column and use that value to populate the missing values
# Think about other ways you could have populated the missing values in the age column. Why would you pick any of those over the mean (or not)?

titanic_noage <- subset(titanic,is.na(titanic$age))
titanic_age <- subset(titanic,titanic$age > '0')
titanic_noage$age <- mean(titanic_age$age)
titanic <- rbind(titanic_noage,titanic_age)
remove (titanic_noage,titanic_age)


# 3: Lifeboat
# You're interested in looking at the distribution of passengers in different lifeboats, 
# but as we know, many passengers did not make it to a boat :-( This means that there are a lot of missing 
# values in the boat column. Fill these empty slots with a dummy value e.g. NA
                                                                                                                                              
titanic_noboat <- subset(titanic,titanic$boat=="")
titanic_boat <- subset(titanic,titanic$boat > '0')
titanic_noboat$boat <-c("NA")
titanic <- rbind(titanic_noboat,titanic_boat)
remove (titanic_boat,titanic_noboat)


# 4: Cabin
# You notice that many passengers don't have a cabin number associated with them.
# Does it make sense to fill missing cabin numbers with a value?

No

#What does a missing value here mean?
# You have a hunch that the fact that the cabin number is missing might be a useful indicator of survival. 
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

titanic$has_cabin_number <-  ifelse(titanic$cabin=="",'0','1')


# 6: Submit the project on Github
# Include your code, the original data as a CSV file titanic_original.csv, and the cleaned up 
# data as a CSV file called titanic_clean.csv.

write.table(titanic, "titanic_clean.csv", sep=",")
                                      
                                                                                                               