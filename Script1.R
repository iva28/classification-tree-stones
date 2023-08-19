# In classification tree algorithm the outcome variable is factor variable,
# whereas the predicting variables can be all types( factor, numeric..) 

#loading "The Rolling Stones" dataset
dataset <- read.csv('stones_analysis.csv',stringsAsFactors = FALSE,check.names = FALSE)

#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

#transforming dataset
source('Utility.R')
dataset <- transormed.dataframe(dataset = dataset)

#checking for NA values
all(complete.cases(dataset))

# Date variable will be excluded, since it doesn't make sense to transform it 
# into factor variable
length(unique(dataset$Date))
dataset$Date <- NULL

length(unique(dataset$Title))
#Too many different values for attribute 'Title', since every song has its own unique name
dataset$Title <- NULL

length(unique(dataset$`Songwriter s `))
#Too many different values for attribute 'Songwriter s '
dataset$`Songwriter s ` <- NULL

# creating outcome variable OnChart
# If there is at least one value in columns, from 'British charts' to 'POL', that is different
# than 'No' the value for OnChart variable will be TRUE, alternatively the value will be FALSE
# this is checked for every row

# Create a logical matrix indicating where the value is "No"
first.chart <- which(colnames(dataset) == 'British charts')
last.chart <- which(colnames(dataset) == 'POL')
no.matrix <- dataset[, first.chart : last.chart] == "No"
no.matrix <- rowSums(!no.matrix) > 0
# adding outcome variable to dataset based on the value in no.matrix 
dataset$OnChart <- as.factor(ifelse(no.matrix == 'TRUE', "Yes","No"))
#eliminating variables that were used for creating outcome variable
dataset <- dataset[,-c(first.chart:last.chart)]

# examining the proportion through distribution
prop.table(table(dataset$OnChart))
# about 76% of songs have been on some chart, while about 24% have not been

#checking for NA values
apply(dataset, 2, function(x) sum(is.na(x)))

#checking for good predictors to be included in model
library(ggplot2)
ggplot(dataset, mapping = aes(x = `Year Recorded`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'Year Recorded' will be excluded from model since the density function doesn't differ for songs that 
# have been on charts and for the songs that haven't been
ggplot(dataset, mapping = aes(x = `Year Released`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'Year Released' will be excluded from model since the density function doesn't differ for songs that 
# have been on charts and for the songs that haven't been
ggplot(dataset, mapping = aes(x = `Album name`, fill = OnChart)) + geom_bar(position = 'fill', width = 0.4)
# Variable 'Album name' has too many levels in comparison to dataset's size, so it will be excluded from analysis
ggplot(dataset, mapping = aes(x = `Record Label`, fill = OnChart)) + geom_bar(position = 'fill', width = 0.4)
# 'Record Label' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `Album type`, fill = OnChart)) + geom_bar(position = 'fill', width = 0.4)
# 'Album type' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `Track number`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'Track number' will be excluded from model since the density function doesn't differ for songs that 
# have been on charts and for the songs that haven't been
ggplot(dataset, mapping = aes(x = `Song duration`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'Song duration' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `Lead vocal s `, fill = OnChart)) + geom_bar(position = 'fill', width = 0.4)
# 'Lead vocal s' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `acousticness`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'acousticness' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `danceability`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'danceability' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `energy`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'energy' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `instrumentalness`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'instrumentalness' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `liveness`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'liveness' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `loudness`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'loudness' affects song's ability to appear on charts, so it will be included
ggplot(dataset, mapping = aes(x = `speechiness`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'speechiness' will be excluded from model since the density function doesn't differ for songs that 
# have been on charts and for the songs that haven't been
ggplot(dataset, mapping = aes(x = `tempo`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'tempo' will be excluded from model since the density function doesn't differ for songs that 
# have been on charts and for the songs that haven't been
ggplot(dataset, mapping = aes(x = `valence`, fill = OnChart)) + geom_density(alpha = 0.5)
# 'valence' will be excluded from model since the density function doesn't differ for songs that 
# have been on charts and for the songs that haven't been
ggplot(dataset, mapping = aes(x = `Certification`, fill = OnChart)) + geom_bar(position = 'fill', width = 0.4)
# 'Certification' variable will be excluded from model since the value of outcome variable mostly doesn't 
# differ 

# Year Recorded, Year Released, Album name, Track number, speechiness, tempo, valence, Certification
dataset$`Year Recorded` <- NULL
dataset$`Year Released` <- NULL
dataset$`Album name` <- NULL
dataset$`Track number` <- NULL
dataset$speechiness <- NULL
dataset$tempo <- NULL
dataset$`valence` <- NULL
dataset$`Certification` <- NULL

# creating training and test sets
library(caret)
set.seed(1)
train.indices <- createDataPartition(dataset$OnChart, p = 0.8, list = FALSE)
train.data <- dataset[train.indices,]
test.data <- dataset[-train.indices,]

# doing cross-validation to find optimal cp( complexity parametar) 
library(e1071)
numFolds <- trainControl(method = 'cv', number = 10)
# the range for the cp values to examine in the cross-validation
cpGrid <-  expand.grid( .cp = seq(0.001, to = 0.05, by = 0.0025)) 
set.seed(1)
dt.cv <- train(x = train.data[,-ncol(dataset)], 
               y = train.data$OnChart, 
               method = "rpart", 
               trControl = numFolds, 
               tuneGrid = cpGrid)

dt.cv
# Best accuracy is for cp = 0.0485
plot(dt.cv)
cp.best <- dt.cv$bestTune$cp

#creating a prediction model using Classification Trees
library(rpart)
tree1 <- rpart(OnChart ~ ., 
               data = train.data,
               method = "class",
               control = rpart.control(cp = cp.best))
tree1
#printing tree
library(rpart.plot)
rpart.plot(tree1, extra = 104, cex = 0.5)

# Album type, liveness, energy, danceability are variables that have been used for creating the classificaion tree
# In root node there are 238 observations and the dominant class is No( song hasn't been on charts), 76% of observations( songs)
# in the train set have not been on any chart and 24% have been

# Album type is the most important variable in the classification tree
# 16% of songs in train set have 'Live' value for attribute Album type and 57% of them have been on charts
# 84% of songs have 'Studio' or 'Compilation' values for attribute Album type and 82% of them haven't been on charts

# 4% of observations( songs) in train set have value greater or equal to 0.72 for attribute 'liveness' and 67% of them have been on charts

# 5% of observations( songs) in train set have value greater or equal to 0.9 for attribute 'energy' and 85% of them have been on charts

# 3% of observations( songs) in train set have value greater or equal to 0.56 for attribute 'danceability' and 75% of them have been on charts

# evaluation of the created model on test set
tree1.pred <- predict(object = tree1, newdata = test.data, type = 'class')
head(tree1.pred)

# to evaluate the predictive quality of the created model we will create confusion matrix
tree1.cm <- table(true = test.data$OnChart, predicted = tree1.pred)
tree1.cm

# computing evaluation metrics
tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval

# Accuracy is the proportion of correct predictions made by the model out of total predictions
# For 41 songs we have predicted that they won't be on charts and they weren't on charts and 
# for 4 songs we have predicted that they will be on charts and they were
# For 45 songs( out of 59) we have correctly predicted their value resulting in accuracy of 76%

# Precision is the ratio of true positive predictions to the total number of positive predictions (true positives + false positives)
# For 41 songs the predicted value was No( they haven't been on charts) and their real value is also No
# For 10 songs the predicted value was No( they haven't been on charts) and their real value is Yes( they have been on charts)
# Therefore the precision metrics is 80.4%

# Recall is the ratio of true positive predictions to the total number of actual positive instances (true positives + false negatives)
# For 41 songs the predicted value was No( they haven't been on charts) and their real value is also No
# For 4 songs the predicted value was Yes( they have been on charts) and their real value is No
# Recall is therefore 91%

#  F1-score is the harmonic mean of precision and recall and it is around 85%




