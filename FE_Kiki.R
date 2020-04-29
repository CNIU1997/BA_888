stringsAsFactors=FALSE
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
library(tidyverse)
# load the data
train= read.csv('train_data.csv')
test = read.csv('test_data.csv')
##############Remove Redundant Features################
# calculate correlation matrix
train %>% select(-X1,-X,-year,-Sector,-Class)-> train
train[] <- lapply(train, function(x) as.numeric(as.character(x)))

correlationMatrix <- cor(train[,1:222],use="complete.obs")
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.2)
# print indexes of highly correlated attributes
print(highlyCorrelated)
important_var=colnames(train[,-highlyCorrelated])
important_var
#######################################################

select_names <- c('Revenue',
                  'Gross_Profit',
                  'Net_Income',
                  'Net_Income_Com',
                  'EPS_Diluted',
                  'Weighted_Average_Shs_Out_(Dil)',
                  'EBIT_Margin',
                  'Profit_Margin',
                  'Free_Cash_Flow_margin',
                  'Consolidated_Income',
                  'Net_Profit_Margin',
                  'Cash_and_short-term_investments',
                  'Total_non-current_assets',
                  'Free_Cash_Flow',
                  'priceEarningsToGrowthRatio',
                  'ebitperRevenue',
                  'niperEBT',
                  'pretaxProfitMargin',
                  'netProfitMargin',
                  'returnOnAssets',
                  'eBITperRevenue',
                  'Return_on_Tangible_Assets',
                  '3Y_Net_Income_Growth_(per_Share)',
                  'Book_Value_per_Share_Growth')

train[,names(train) %in% select_names] -> select_data

correlationMatrix2 <- cor(select_data[,1:15],use="complete.obs")
# summarize the correlation matrix
print(correlationMatrix2)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated2 <- findCorrelation(correlationMatrix2, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated2)
important_var2=colnames(select_data[,-highlyCorrelated2])
important_var2

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
                      number=20)

#deal with NAs before modeling 
train <- na.omit(train)

# run the RFE algorithm
results <- rfe(train[,1:50], 
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
train[,names(train) %in% important_var2] -> df_train
df_trainsss = read.csv('train_data.csv')
df_train['Class'] = df_trainsss$Class


test_df <- test[,names(test) %in% important_var2] 
df_testsss = read.csv('test_data.csv')
test_df['Class'] = df_testsss$Class

write.csv(df_train, "df_train.csv")
write.csv(test_df, "test_df.csv")


##############XGBOOST########################
# library(edgar)
# library(edgarWebR)
# library(xgboost)
# ## set the seed to make your partition reproducible
# set.seed(123)
# 
# ## get train and test
# train_df <- train[,names(train) %in% important_var2] %>%  as.matrix() 
# test_df <- test[,names(test) %in% important_var2] %>%  as.matrix() 
# 
# ## bring back  train again to get label Class
# df_trainsss = read.csv('train_data.csv')
# train.label <- df_train$Class
# test.label <- test$Class
# 
# dtrain <- xgb.DMatrix(data = train_df, label= train.label)
# dtest <- xgb.DMatrix(data = test_df, label= test.label)
# 
# # Train the XGBoost classifer
# # train a model using our training data
# model <- xgboost(data = dtrain, # the data   
#                  nround = 3, # max number of boosting iterations
#                  objective = "binary:logistic") 
# 
# # generate predictions for our held-out testing data
# pred <- predict(model, dtest)
# 
# # get & print the classification error
# err <- mean(as.numeric(pred > 0.5) != test.label)
# print(paste("test-error=", err))
