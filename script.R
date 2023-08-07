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
library(ggplot2)
#checking for attribute 'Year Recorded'
ggplot(dataset, mapping = aes(x = `Year Recorded`, fill = Award)) + geom_density(alpha = 0.5)
#Year in which the song has been recorded, has influence on its chance for getting Award
#Around 1970 the number of awarded songs is the highest and this variable will be included in model

#checking for attribute 'Year Released'
ggplot(dataset, mapping = aes(x = `Year Released`, fill = Award)) + geom_density(alpha = 0.5)
#Analogous to previous variable, variable 'Year Released' has an influence on songs chance for getting an award
#therefore it will be included in model

#checking for attribute 'Album name'
ggplot(dataset, mapping = aes(x = `Album name`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
# Album name doesn't affect wheter the song has won an award or not, so it will be excluded from model
dataset$`Album name` <- NULL

#checking for attribute 'Record Label'
ggplot(dataset, mapping = aes(x = `Record Label`, fill = Award)) + geom_bar(position = 'dodge', width = 0.4)+theme_bw() 
#Record Label slightly affects song's ability to win an award  so it will be included

#checking for attribute 'Album Type'
ggplot(dataset, mapping = aes(x = `Album type`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#Album type  affects song's ability to win an award  so it will be included

#checking for attribute 'Track number'
ggplot(dataset, mapping = aes(x = `Track number`, fill = Award)) + geom_density(alpha = 0.5)
#Track number doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$`Track number` <- NULL

#checking for attribute 'Song duration'
ggplot(dataset, mapping = aes(x = `Song duration`, fill = Award)) + geom_density(alpha = 0.5)
# Song duration affects song's ability to win an award  so it will be included

#Songwriter attribute doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$`Songwriter s ` <- NULL

#checking for attribute 'Lead vocal s'
ggplot(dataset, mapping = aes(x = `Lead vocal s `, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#Lead vocal s doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$`Lead vocal s ` <- NULL

#checking for attribute 'acousticness'
ggplot(dataset, mapping = aes(x = `acousticness`, fill = Award)) + geom_density(alpha = 0.5)
#acousticness affects song's ability to win an award  so it will be included

#checking for attribute 'energy'
ggplot(dataset, mapping = aes(x = `energy`, fill = Award)) + geom_density(alpha = 0.5)
#energy doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$energy <- NULL

#checking for attribute 'danceability'
ggplot(dataset, mapping = aes(x = `danceability`, fill = Award)) + geom_density(alpha = 0.5)
#danceability affects song's ability to win an award  so it will be included

#checking for attribute 'instrumentalness'
ggplot(dataset, mapping = aes(x = `instrumentalness`, fill = Award)) + geom_density(alpha = 0.5)
#instrumentalness affects song's ability to win an award  so it will be included

#checking for attribute 'liveness'
ggplot(dataset, mapping = aes(x = `liveness`, fill = Award)) + geom_density(alpha = 0.5)
#liveness affects song's ability to win an award  so it will be included

#checking for attribute 'loudness'
ggplot(dataset, mapping = aes(x = `loudness`, fill = Award)) + geom_density(alpha = 0.5)
#loudness doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$loudness <- NULL

#checking for attribute 'speechiness'
ggplot(dataset, mapping = aes(x = `speechiness`, fill = Award)) + geom_density(alpha = 0.5)
#speechiness affects song's ability to win an award  so it will be included

#checking for attribute 'tempo'
ggplot(dataset, mapping = aes(x = `tempo`, fill = Award)) + geom_density(alpha = 0.5)
#tempo affects song's ability to win an award  so it will be included

#checking for attribute 'valence'
ggplot(dataset, mapping = aes(x = `valence`, fill = Award)) + geom_density(alpha = 0.5)
#valence doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$valence <- NULL

#checking for attribute 'British charts'
ggplot(dataset, mapping = aes(x = `British charts`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#British charts affects song's ability to win an award  so it will be included

#checking for attribute 'UK Peak Pos'
ggplot(dataset, mapping = aes(x = `UK Peak Pos`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#UK Peak Pos affects song's ability to win an award, but it has too many levels in comparison to dataset size so 
# it will be excluded
dataset$`UK Peak Pos` <- NULL

#checking for attribute 'Woc'
ggplot(dataset, mapping = aes(x = `WoC`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#The number of weeks  the song has been on chart affects song's ability to win an award  so it will be included

#checking for attribute 'US Cash'
ggplot(dataset, mapping = aes(x = `US Cash`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
# US Cash doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$`US Cash` <- NULL

#checking for attribute 'US Rec World'
ggplot(dataset, mapping = aes(x = `US Rec World`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#US Rec World affects song's ability to win an award  so it will be included

#checking for attribute 'AUS'
ggplot(dataset, mapping = aes(x = `AUS`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
# AUS doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$AUS <- NULL

#checking for attribute 'US'
ggplot(dataset, mapping = aes(x = `US`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
# US doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$US <- NULL

#checking for attribute 'GER'
ggplot(dataset, mapping = aes(x = `GER`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
# GER doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$GER <- NULL

#checking for attribute 'NLD'
ggplot(dataset, mapping = aes(x = `NLD`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#NLD doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$NLD <- NULL

#checking for attribute 'FRA'
ggplot(dataset, mapping = aes(x = `FRA`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#FRA doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$FRA <- NULL

#checking for attribute 'SWI'
ggplot(dataset, mapping = aes(x = `SWI`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
# SWI doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$SWI <- NULL

#checking for attribute 'POL'
ggplot(dataset, mapping = aes(x = `POL`, fill = Award)) + geom_bar(position = 'fill', width = 0.4)+theme_bw() 
#POL doesn't affect whether the song has won an award or not, so it will be excluded from model
dataset$POL <- NULL

#Date variable will be excluded
dataset$Date <- NULL
