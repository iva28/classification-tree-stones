#loading "The Rolling Stones" dataset
dataset <- read.csv('stones_analysis.csv',stringsAsFactors = FALSE,check.names = FALSE)

#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

#transforming dataset
source('Utility.R')
dataset <- transormed.dataframe(dataset = dataset)

#making outcome variable "Award"
unique(dataset$Certification)
str(dataset$Certification)

dataset$Award <- factor(ifelse(test = dataset$Certification != "No", yes = "Yes", no = "No"), levels = c("Yes", "No"))
prop.table(table(dataset$Award))

#deleting Certification variable that was used for creating Award variable
dataset$Certification <- NULL

length(unique(dataset$Title))
#each song title is unique so it will be excluded from model
dataset$Title <- NULL

#checking missing values in dataset
apply(dataset, MARGIN = 2,FUN = function(x) sum(is.na(x)))
#the aren't any NA values
apply(dataset, MARGIN = 2,FUN = function(x) sum(x == "", na.rm = T))
#there are not any "" values
apply(dataset, MARGIN = 2,FUN = function(x) sum(x == " ", na.rm = T))
#the are not any " " values

#checking for good predictors to be included in model


