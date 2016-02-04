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


# 1. Merges the training and the test sets to create one data set.

#cleanup functions
rm(features, subject_test,subject_train,X_test,y_test, X_train,y_train,test_dc,train_dc)

# create data sets from .txt files and name the variable 

    features <- read.table('features.txt',sep = "\t", header=FALSE) 
      colnames (features) <- c ("feature_val")

    subject_test <- read.table('test/subject_test.txt',sep = "\t", header=FALSE)
      colnames (subject_test) <- c ("subject_val")
      
    X_test <- read.table('test/X_test.txt',sep = "\t", header=FALSE)
      colnames (X_test) <- c ("X_val")
      
    y_test <- read.table('test/y_test.txt',sep = "\t", header=FALSE)
      colnames (y_test) <- c ("y_val")
      
    subject_train <- read.table('train/subject_train.txt',sep = "\t", header=FALSE)  
      colnames (subject_train) <- c ("subject_val")
    
    X_train <- read.table('train/X_train.txt',sep = "\t", header=FALSE)
      colnames (X_train) <- c ("X_val")
    
    y_train <- read.table('train/y_train.txt',sep = "\t", header=FALSE)
      colnames (y_train) <- c ("y_val")
    
    
# gather variables and convert rows into columns   
    test_dc <- cbind (subject_test, y_test, X_test)
    
    train_dc <- cbind (subject_train, y_train, X_train)
    
    combined_dc <- bind_rows(test_dc,train_dc)

# split the third column (X_test_val) into descrete columns based on the fact 
# for each user there are 561 observations.
    



# 2. Extracts columns containing mean and standard deviation for each measurement 




# 3. Creates variables called ActivityLabel and ActivityName that label all 
#    observations with the corresponding activity labels and names respectively

    separate (activity_labels,c("ActivityLabel","ActivityName"), sep = ",")
      
      glimpse(activity_labels)


# 4. From the data set in step 4, creates a second, independent tidy data 
#    set with the average of each variable for each activity and each subject.
    
    
    