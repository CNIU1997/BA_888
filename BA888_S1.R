library(readr)
library(dplyr)
library(tidyverse)
library(skimr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(edgar)
library(edgarWebR)
library("knitr")

dat_14 <- read_csv("2014_Financial_Data.csv")
dat_15 <- read_csv("2015_Financial_Data.csv")
dat_16 <- read_csv("2016_Financial_Data.csv")
dat_17 <- read_csv("2017_Financial_Data.csv")
dat_18 <- read_csv("2018_Financial_Data.csv")

#data cleaning
colnames(dat_14) = gsub(" ", "_", colnames(dat_14))
colnames(dat_15) = gsub(" ", "_", colnames(dat_15))
colnames(dat_16) = gsub(" ", "_", colnames(dat_16))
colnames(dat_17) = gsub(" ", "_", colnames(dat_17))
colnames(dat_18) = gsub(" ", "_", colnames(dat_18))

dat_14 <- dat_14 %>% 
  mutate(  'PRICE_VAR' = `2015_PRICE_VAR_[%]` ) 

dat_15 <- dat_15 %>% 
  mutate(  'PRICE_VAR' = `2016_PRICE_VAR_[%]` ) 

dat_16 <- dat_16 %>% 
  mutate(  'PRICE_VAR' = `2017_PRICE_VAR_[%]` ) 

dat_17 <- dat_17 %>% 
  mutate(  'PRICE_VAR' = `2018_PRICE_VAR_[%]` ) 

dat_18 <- dat_18 %>% 
  mutate(  'PRICE_VAR' = `2019_PRICE_VAR_[%]` ) 

#add year column to each data set 
dat_14$year <- 2014
dat_15$year <- 2015
dat_16$year <- 2016
dat_17$year <- 2017
dat_18$year <- 2018







############################# merge 2014-2017 as train, 2018 as test


data <- merge(dat_14,dat_15, by ="X1", all = T) %>% 
  merge(dat_16,by ="X1", all = T) %>% 
  merge(dat_17, by ="X1", all = T)



dat_15[,(names(dat_14) == names(dat_15)) == FALSE]



###########################

#################################Cleaning 
# replace NA with 0 for all numeric values
dat_14 %>% 
  select_if(., is.numeric) %>% 
  mutate_all(~replace(., is.na(.), 0)) -> dat_14

dat_15 %>% 
  select_if(., is.numeric) %>% 
  mutate_all(~replace(., is.na(.), 0)) -> dat_15

dat_16 %>% 
  select_if(., is.numeric) %>% 
  mutate_all(~replace(., is.na(.), 0)) -> dat_16

# check outliers 
boxplot(dat_14$returnOnAssets)$out -> outVlaues
# dat_14$returnOnAssets[ !(dat_14$returnOnAssets %in%outVlaues) ]


################################################################

# #exploratory dataset analysis 
select_if(dat_14,is.numeric) -> dat2_14

# dat2_14 %>% 
#   cor(use="pairwise.complete.obs") -> cor_numVar
# head(cor_numVar,2)
# 
# as.matrix(sort(cor_numVar[,"2015_PRICE_VAR_[%]"], decreasing = TRUE)) -> cor_sorted
# CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
# cor_numVar <- cor_numVar[CorHigh, CorHigh]
# corrplot.mixed(cor_numVar,tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = 0.7, number.cex=0.7)
# 
# dat2_14$`R&D_Expenses`
# ####look at some (17) important variables 
# summary(dat_14)
# 
# dat_14 %>%
#   select(starts_with("Revenue"), Class) %>%
#   na.omit %>%
#   group_by(Class) %>%
#   summarise( mean_Revenue = mean(Revenue),
#              mean_Revenue_Growth = mean(Revenue_Growth),
#              mean_Revenue_per_Share = mean(Revenue_per_Share)) %>% 
#   kable()
# 
# 
# dat_14 %>% 
#   select(ends_with("Expenses"), Class) %>% 
#   na.omit %>% 
#   group_by(Class) %>% 
#   summarise( mean_RD_Expenses= mean(`R&D_Expenses`),
#              mean_Operating_Expenses = mean(`Operating_Expenses`)) %>% 
#   ggplot()+
#   geom_bar(aes(factor(Class),mean_Expense), stat = "identity",fill="#005b5e")+
#   labs(
#     x="Class",
#     y="Average Expense"
#   )+coord_flip()
# 
# dat_14 %>%
#   select(ends_with("value"), Class) %>%
#   na.omit %>%
#   group_by(Class) %>%
#   summarise( mean_Eenterprise_Value= mean(Enterprise_Value),
#              mean_Tangible_Asset_Value = mean(Tangible_Asset_Value)) %>% 
#   kable()
# dat_14 %>%
#   select(starts_with("debt"), Class) %>%
#   na.omit %>%
#   group_by(Class) %>%
#   summarise( mean_debtRatio= mean(debtRatio),
#              mean_debtEquityRatio = mean(debtEquityRatio),
#              mean_Debt_to_Assets = mean(Debt_to_Assets),
#              mean_Debt_Growth = mean(Debt_Growth)) %>% 
#   kable()
# 
# dat_14 %>%  
#   group_by(Class) %>% 
#   count
# 
# dat_14 %>% 
#   select(starts_with("Revenue"), Class) %>% 
#   na.omit %>% 
#   group_by(Class) %>% 
#   ggplot()+
#   geom_boxplot(aes(factor(Class),Revenue, group=Class),color="#005b5e")+
#   labs(
#     x="Class"
#   )
# 
# dat_14 %>% 
#   ggplot()+
#   geom_bar(aes(factor(Class)), fill = "#005b5e")+
#   labs(
#     title="Distribution of Buy-worthy and Not Buy-worthy Stocks",
#     x = "Class",
#     y = "Number of Stocks"
#   )
# 
# 
# getFilings(cik.no = c(1000180, 38079), c('10-K','10-Q'), 
#            2006, quarter = c(1, 2, 3), downl.permit = "n")
# words.list <- scan(system.file('data/negwords.txt', package = 'edgar'), what='character')
# 
# 
# senti.words <- getSentimentCount(word.frq, words.list)
# 
# report <- getFilings(2018, 1000180, '10-K')
# 
# info <- getFilingInfo('United Technologies', 1994) 
# 

###########################################################dat_14 explory 
select_if(dat_14,is.numeric) -> dat2_14
dat2_14 %>% 
  select(-year) -> dat2_14

#apply(dat2_14, 2, var, na.rm=TRUE) > 0 -> GET


##################################pca
library(arules)
library(arulesViz)
library(corrplot)
library(factoextra)
library(cowplot)
library(purrr)
library(cluster)
library(factoextra)
library(gridExtra)
c <- prcomp(dat2_14,center = TRUE, scale = TRUE)
fviz_screeplot(c, addlables = T,ylim = c(0,100))#3/4

get_eigenvalue(c) >1 #221
#choose 78

c_pcs <- predict(c, newdata = dat2_14)
class(c_pcs)
c_pcs <- as.data.frame(c_pcs)

##using 78 conponents
c_pca5 <- c_pcs[,1:78]

c_scale <- scale(c_pca5)

#eval numer of k using silhouette and wss
# fviz_nbclust(c_scale, kmeans, method = 'wss', k.max = 30)--- takes a  long time to run

k_wss = function(k) {
  km = kmeans(c_scale, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}

x <- 1:20
wass <- map_dbl(x,k_wss)
plot(x,wass, type ="b")#wss- no useful suggestion

fviz_nbclust(c_scale, kmeans, method = 'silhouette', k.max = 20)##silhouette suggests 6 clusters
#2/5 excellent 

##################################Forward Backward selection
# select_names <- c( "Cash_and_cash_equivalents",             
#                    "Effect_of_forex_changes_on_cash",       
#                    "operatingCashFlowPerShare",             
#                    "freeCashFlowPerShare",                  
#                    "Graham_Number",                         
#                    "Gross_Profit_Growth",                   
#                    "Operating_Income_Growth",               
#                    "EPS_Diluted_Growth",                    
#                    "Weighted_Average_Shares_Growth",        
#                    "Weighted_Average_Shares_Diluted_Growth",
#                    "Operating_Cash_Flow_growth",            
#                    "Receivables_growth",                    
#                    "Inventory_Growth",                      
#                    "Asset_Growth")
# 
# train_data[,names(train_data) %in% select_names] -> select_data
# select_data$Price<-train_data$PRICE_VAR
# fit1<-lm(Price~.,select_data)
# backward=stepAIC(fit1,direction="backward")
# summary(backward)
# fit2<-lm(Price~1,select_data)
# forward=stepAIC(fit2,direction = "forward",scope=list(upper=fit1,lower=fit2))
# summary(forward)
# combined=stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
# summary(combined)

##################################randomforest 
library(randomForest)
require(caTools)
rf <- randomForest(
  Class ~ .,
  data= train
)

plot(rf)

pred = predict(rf, newdata=test[-221])

#get index of Class
grep("Class", colnames(test))

##################################ridge
library(glmnet)


y <- dat2_14 %>% select("2015_PRICE_VAR_[%]") %>% unlist() %>%
  as.numeric()
x <- dat2_14 %>% select(-Class,-"2015_PRICE_VAR_[%]") %>% data.matrix()


fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
summary(fit)

lambdas = 10^seq(10, -2, length = 100)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

y_predicted <- predict(fit, s = opt_lambda, newx = x)
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse / sst
rsq

##################################xgboost
library(xgboost)
############split train test
smp_size <- floor(0.8 * nrow(dat2_14))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dat2_14)), size = smp_size)

train <- dat2_14[train_ind,] %>% select(-Class,-"2015_PRICE_VAR_[%]") %>%  as.matrix()  #3046 221
test <- dat2_14[-train_ind, ] %>% select(-Class,-"2015_PRICE_VAR_[%]") %>%  as.matrix() #762 221
train.label <- dat2_14[train_ind,] %>% select(Class) %>% as.matrix()
test.label <- dat2_14[-train_ind,] %>% select(Class) %>% as.matrix()

dtrain <- xgb.DMatrix(data = train, label= train.label)
dtest <- xgb.DMatrix(data = test, label= test.label)

# Train the XGBoost classifer
# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic") 

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test.label)
print(paste("test-error=", err))

###########################################################dat_14 explory ends


#####################################outliers 
## outliers
source("http://goo.gl/UUyEzD")
outlierKD(test_data, Revenue)








