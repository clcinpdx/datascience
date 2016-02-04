## run_analysis.R
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
# set working directory
############################################################
getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/UCI HAR Dataset')

############################################################
# 1. Merge the training and the test sets to create one data set.
############################################################

# handy dataframe cleanup functions
rm(features, subject_test,subject_train,X_test,y_test, X_train,y_train,test_dc,train_dc)


# create data sets from .txt files and name the variable 

    features <- read.csv ('features.txt', sep = '', header=FALSE) 
      colnames (features) <- c ("feature_val","feature_txt")

    subject_test <- read.csv ('test/subject_test.txt', sep = '', header=FALSE)
      colnames (subject_test) <- c ("subject_val")

    subject_train <- read.csv ('train/subject_train.txt', sep = '', header=FALSE)  
      colnames (subject_train) <- c ("subject_val")
      
    X_test <- read.csv ('test/X_test.txt', sep = '', header=FALSE)
      colnames (X_test) <- features$feature_txt
    
    y_test <- read.csv ('test/y_test.txt', sep = '', header=FALSE)
      colnames (y_test) <- c ("y_val")
    
    X_train <- read.csv ('train/X_train.txt', sep = '', header=FALSE)
      colnames (X_train) -> features$feature_txt
    
    y_train <- read.csv ('train/y_train.txt', sep = '', header=FALSE)
      colnames (y_train) <- c ("y_val")
    
    
# gather test and training variables
    test_dc <- cbind (subject_test, y_test, X_test)
    
    train_dc <- cbind (subject_train, y_train, X_train)
    
# combine test and training data into single frame
    combined_dc <- bind_rows(test_dc,train_dc)




# 2. Extracts columns containing mean and standard deviation for each measurement 




# 3. Creates variables called ActivityLabel and ActivityName that label all 
#    observations with the corresponding activity labels and names respectively

    separate (activity_labels,c("ActivityLabel","ActivityName"), sep = ",")
      
      glimpse(activity_labels)


# 4. From the data set in step 4, creates a second, independent tidy data 
#    set with the average of each variable for each activity and each subject.
    
    
    