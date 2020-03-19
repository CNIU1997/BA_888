stringsAsFactors=FALSE
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
library(tidyverse)
# load the data
train= read.csv('train_data.csv')

##############Remove Redundant Features################
# calculate correlation matrix
train %>% select(-X1,-X,-year,-Sector,-Class)-> train
train[] <- lapply(train, function(x) as.numeric(as.character(x)))

correlationMatrix <- cor(train[,1:222],use="complete.obs")
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
important_var=colnames(train[,-highlyCorrelated])
important_var
#######################################################

##############Feature Selection########################
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
#train
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, 
                      method="cv", 
                      number=200)

#deal with NAs before modeling 
train <- na.omit(train)

# run the RFE algorithm
results <- rfe(train[,1:222], 
               train$PRICE_VAR, 
               sizes=c(1:30), 
               rfeControl=control,
               na.rm =T)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
######################################################
