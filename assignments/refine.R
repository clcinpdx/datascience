## Craig Calder
# Data Wrangling Exercise 1: Basic Data Manipulation

############################################################
# 1. set working directory and load data
############################################################

getwd()

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/assignments')

# create the loan dataframe from the loan.csv data file
refine  <- read.csv ("refine.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)

str (refine)

# 1: Clean up brand names
# Clean up the 'company' column, so all of the misspellings of the brand names are standardized. 
# For example, you can transform the values in the column to be: philips, akzo, van houten and unilever
# (all lowercase).

library (dplyr)

refine$company <- lapply(refine$company, function(v) {
                              if (is.character(v)) return(tolower(v))
                              else return(v)
                              })
# fix company misspellings
# http://www.endmemo.com/program/R/gsub.php
# regular expressions: http://regexlib.com/CheatSheet.aspx

refine$company <- gsub("\\w+ps$","philips",refine$company,fixed=FALSE)
refine$company <- gsub("^ak\\w*\\s*\\w*","akzo",refine$company,fixed=FALSE)
refine$company <- gsub("^van\\w*\\s*\\w*","van houten",refine$company,fixed=FALSE)
refine$company <- gsub("^uni\\w*\\s*\\w*","unilever",refine$company,fixed=FALSE)


# 2: Separate product code and number
# Separate the product code and product number into separate columns i.e. add two new columns 
# called product_code and product_number, containing the product code and number respectively
library (tidyr)
refine <- separate(refine,Product.code...number,c("product_code","product_number"),sep = "-", remove=FALSE)

# 3: Add product categories
# You learn that the product codes actually represent the following product categories:
#  p = Smartphone, v = TV, x = Laptop, q = Tablet

refine <- mutate(refine,product_category=product_code)
refine$product_category <- as.factor(refine$product_category)
levels(refine$product_category) <- c ("Smartphone","Tablet","TV","Laptop")

# 4: Add full address for geocoding
# You'd like to view the customer information on a map. In order to do that, the addresses need to be in a 
# form that can be easily geocoded. Create a new column full_address that concatenates the three address 
# fields (address, city, country), separated by commas.

refine <- refine %>%
          unite(full_address, address,city,country,sep =",", remove=FALSE)

# 5: Create dummy variables for company and product category
# see http://stats.stackexchange.com/questions/94010/understanding-dummy-manual-or-automated-variable-creation-in-glm

# 5a) Add four binary (1 or 0) columns for company: 
#     company_philips, company_akzo, company_van_houten and company_unilever

company_phillips  = ifelse(refine$company=="philips", 1, 0)
refine$company_phillips <- as.factor (company_phillips)

company_akzo  = ifelse(refine$company=="akzo", 1, 0)
refine$company_akzo <- as.factor (company_akzo)

company_van_houten  = ifelse(refine$company=="van houten", 1, 0)
refine$company_van_houten <- as.factor (company_van_houten)

company_unilever  = ifelse(refine$company=="unilever", 1, 0)
refine$company_unilever <- as.factor (company_unilever)

# 5b) Add four binary (1 or 0) columns for product category: 
# product_smartphone, product_tv, product_laptop and product_tablet

product_smartphone  = ifelse(refine$product_category=="Smartphone", 1, 0)
refine$product_smartphone <- as.factor (product_smartphone)

product_tv  = ifelse(refine$product_category=="TV", 1, 0)
refine$product_tv <- as.factor (product_tv)

product_laptop  = ifelse(refine$product_category=="Laptop", 1, 0)
refine$product_laptop <- as.factor (product_laptop)

product_tablet  = ifelse(refine$product_category=="Tablet", 1, 0)
refine$product_tablet <- as.factor (product_tablet)


# 6: Submit the project on Github
# Include your code, the original data as a CSV file refine_original.csv, and the cleaned up 
# data as a CSV file called refine_clean.csv.

write.table(refine, "refine_clean.csv", sep=",")


