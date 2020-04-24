## import 
library(reshape2)
library(ggplot2)
library(dplyr)
library(Boruta)
library(Amelia)
library(RColorBrewer)
library(aplot)
library(tidyverse)
library(olsrr)
library(party)
install.packages("relaimpo")
library(relaimpo)

##
read_file<- read.csv("train_data.csv")

str(read_file)
summary(read_file)

##
read_file[-c(2,224,226)]<-lapply(read_file[-c(2,224,226)], as.numeric)
read_file[2]<- lapply(read_file[2],as.character)
classVariables = sapply(read_file, function(x) class(x))
length(names(which(sapply(read_file, class) == "factor"))) 
length(names(which(sapply(read_file, class) == "numeric"))) 
length(names(which(sapply(read_file, class) == "character"))) 


## Random Forest Method,find a set of predictors that best explains the variance in the response variable.
df_rf= read_file[-c(1,2,225,227)]
cf1 <- cforest(Class ~ . , data= df_rf, control=cforest_unbiased(mtry=2,ntree=50))
RF_variable_importance<- varimp(cf1) # get variable importance, based on mean decrease in accuracy
varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cf1)  # more robust towards class imbalance.

## Relative Importance, the relative importance of variables fed into a lm model can be determined as a relative percentage.
lmMod <- lm(Class ~ . , data = df_rf)  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # relative importance



##
read_file[-226][is.na(read_file[-226])] =0

## wrote an function to graph out missing value
ggplot_missing<- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(read_file) 
## no white lines shows in the graph which means there is no missing values

## set seeds for reproduction
set.seed(888)
## It is a binary classification problem with multiple features.
## feature selections 
boruta_stock_train <- Boruta(Class~., data =read_file[-c(1,2,225,227)], doTrace = 3)# don't even try to run it, take forever
#
boruta_stock_train_extra <- Boruta(Class~., data =read_file[-c(1,2,225,227)], doTrace = 3, maxRuns=200)## can do more Runs with maxRuns specified 

#
print(boruta_stock_train)
boruta_stock_train$finalDecision[boruta_stock_train$finalDecision[]=="Confirmed"]
length(boruta_stock_train$finalDecision[boruta_stock_train$finalDecision[]=="Confirmed"])
#
print(boruta_stock_train_extra)
boruta_stock_train_extra$finalDecision[boruta_stock_train_extra$finalDecision[]=="Confirmed"]
length(boruta_stock_train_extra$finalDecision[boruta_stock_train_extra$finalDecision[]=="Confirmed"])


## since we still have 64 tentative attributes left, let check these variables
## here Tentative features have an importance that is so close to their best shadow features that Boruta is not able to 
## make a decision with the desired confidence in the default number of Random Forest runs.
#take a call on tentative features
boruta_stock <- TentativeRoughFix(boruta_stock_train)
print(boruta_stock)
#
boruta_stock_extra <- TentativeRoughFix(boruta_stock_train_extra)
print(boruta_stock_extra)
## now boruta down it's work

##Plot out results
## The y axis label Importance represents the Z score of every feature in the shuffled dataset.
## The blue boxplots correspond to minimal, average and maximum Z score of a shadow feature, 
## while the orangered and lightblue boxplots represent Z scores of rejected and confirmed features, respectively. 
## As you can see the orangered boxplots have lower Z score than that of maximum Z score of shadow feature which is precisely 
## the reason they were put in unimportant category.
plot(boruta_stock, xlab = "", xaxt = "n",ylab = "Importance: Z-score", ylim=c(-5, 10),
     main= " Z-score of every feature in the shuffled dataset",
     col=c("grey","lightblue2","orangered")[as.numeric(boruta_stock$finalDecision)])
legend("topleft", legend=unique(levels(boruta_stock$finalDecision)), pch=16, col=c("grey","lightblue2","orangered"))

lz<-lapply(1:ncol(boruta_stock$ImpHistory),function(i)
  boruta_stock$ImpHistory[is.finite(boruta_stock$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_stock$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_stock$ImpHistory), cex.axis = 0.8,hadj =0.25)

#
plot(boruta_stock_extra, xlab = "", xaxt = "n",ylab = "Importance: Z-score", ylim=c(-10, 20),
     main= " Z-score of every feature in the shuffled dataset",
     col=c("grey","lightblue2","orangered")[as.numeric(boruta_stock_extra$finalDecision)])
legend("topleft", legend=unique(levels(boruta_stock_extra$finalDecision)), pch=16, col=c("grey","lightblue2","orangered"))

lz<-lapply(1:ncol(boruta_stock_extra$ImpHistory),function(i)
  boruta_stock_extra$ImpHistory[is.finite(boruta_stock_extra$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_stock_extra$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_stock_extra$ImpHistory), cex.axis = 0.8,hadj =0.25)

##confirm the importance of the features
getSelectedAttributes(boruta_stock, withTentative = T)
#
getSelectedAttributes(boruta_stock_extra, withTentative = T)
##store results into a dataframe
stock_df <- attStats(boruta_stock)
# 
stock_df_extra <- attStats(boruta_stock_extra)

## save results in the environment
save.image(file='Boruta_feature_selection_results_version1.RData')
## check if its in the current dictory
dir()
## load pervious saved results
load('Boruta_feature_selection_results_version1.RData')


## test the selected variables performance 
feature_selected <- cbind(variable = rownames(stock_df_extra), stock_df_extra)
rownames(feature_selected) <- 1:nrow(feature_selected)
feature_selected %>% filter(decision=="Confirmed") %>% arrange(desc(meanImp))%>% filter(meanImp>7) %>% select(variable)# top27_features
feature_selected %>% filter(decision=="Confirmed") %>% arrange(desc(meanImp))%>% filter(meanImp>6) %>% select(variable)# top57_features

##load test dataset
df_test<- read.csv("test_data.csv")
glimpse(df_test)
summary(df_test)

##Check is there any duplicated columns 
df_test$niperEBT==df_test$nIperEBT # same columns 
df_test$eBTperEBIT== df_test$ebtperEBIT #Same columns
df_test$Net_Income_Com== df_test$Net_Income ## Not the same columns 
df_test$ priceToBookRatio== df_test$PB_ratio ## Not the same columns
df_test$Net_Profit_Margin== df_test$netProfitMargin ## Not the same columns

#top27 features  accuarcy : 0.6833 
classifier <- glm(Class~niperEBT+ Effect_of_forex_changes_on_cash+ Earnings_Yield+ effectiveTaxRate+
                    SG.A_to_Revenue+ priceFairValue+ Weighted_Average_Shares_Diluted_Growth+ EV_to_Free_cash_flow+
                    Sector+ Gross_Profit_Growth+ EV_to_Operating_cash_flow+ Weighted_Average_Shares_Growth+
                    eBTperEBIT+ assetTurnover+ EV_to_Sales+ Net_Income_Com+ Net_Income+
                    Enterprise_Value_over_EBITDA+ Revenue_Growth+ Operating_Cash_Flow_per_Share+ Inventory_Growth+
                    Earnings_Before_Tax_Margin+ operatingCashFlowPerShare+ priceToOperatingCashFlowsRatio+ POCF_ratio, 
                  family='binomial', data =read_file[-c(1,2,225,227)])
summary(classifier)
N_test= nrow(df_test)
predictions <- predict(classifier,newdata=df_test[-c(1,2,225,227)],type="response")[1:N_test]
confusionMatrix(factor(round(predictions)),factor(df_test['Class'][1:N_test,]))

#top57 features accuarcy: : 0.5162  
classifier_1 <- glm(Class~niperEBT+ Effect_of_forex_changes_on_cash+ Earnings_Yield+ effectiveTaxRate+ SG.A_to_Revenue+ 
                     priceFairValue+ Weighted_Average_Shares_Diluted_Growth+ EV_to_Free_cash_flow+ Sector+ Gross_Profit_Growth+ 
                     EV_to_Operating_cash_flow+ Weighted_Average_Shares_Growth+ eBTperEBIT+ assetTurnover+ EV_to_Sales+ Net_Income_Com+ 
                     Net_Income+ Enterprise_Value_over_EBITDA+ Revenue_Growth+ Operating_Cash_Flow_per_Share+ Inventory_Growth+ 
                     Earnings_Before_Tax_Margin+ operatingCashFlowPerShare+ priceToOperatingCashFlowsRatio+ POCF_ratio+ 
                     Free_Cash_Flow_Yield+ Consolidated_Income+ Profit_Margin+ priceToBookRatio+ PB_ratio+ priceBookValueRatio+ 
                     PTB_ratio+ Earnings_before_Tax+ operatingCashFlowSalesRatio+ enterpriseValueMultiple+ grossProfitMargin+ 
                     Net_Profit_Margin+ netProfitMargin+ Gross_Margin+ Net_Income_per_Share+ EBIT_Margin+ eBITperRevenue+ 
                     Graham_Number+ ebitperRevenue+ priceSalesRatio+ EBIT+ pretaxProfitMargin+ Operating_Cash_Flow+ 
                     Total_non.current_assets+ Free_Cash_Flow_margin+ EBITDA+ Income_Quality+EBITDA_Margin+ 
                     Book_Value_per_Share_Growth+ dividendpaidAndCapexCoverageRatios,
                   family='binomial', data =read_file[-c(1,2,225,227)])
summary(classifier_1)
predictions_1 <- predict(classifier_1,newdata=df_test[-c(1,2,225,227)],type="response")[1:N_test]
confusionMatrix(factor(round(predictions_1)),factor(df_test['Class'][1:N_test,]))


