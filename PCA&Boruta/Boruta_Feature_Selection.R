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
library(relaimpo)
library(pROC)
library(ROCR)
library(plyr)
library(caret)
library(VGAM)
library(tidyverse)
library(varImp)

##
read_file<- read.csv("train_data.csv")
str(read_file)
summary(read_file)
dim(read_file)
deleted_names<- c('Consolidated_Income',
                  'EPS_Diluted',
                  'PB_ratio',
                  'PTB_ratio',
                  'Price_to_Sales_Ratio',
                  'PE_ratio',
                  'PFCF_ratio',
                  'POCF_ratio',
                  'Return_on_Tangible_Assets',
                  'ROE',
                  'ROIC',
                  'Payables_Turnover',
                  'Inventory_Turnover',
                  'Days_of_Inventory_on_Hand',
                  'Days_Payables_Outstanding',
                  'Debt_to_Assets',
                  'Debt_to_Equity',
                  'Interest_Coverage',
                  'cashFlowCoverageRatios',
                  'Operating_Cash_Flow_per_Share',
                  'Free_Cash_Flow_per_Share',
                  'Cash_per_Share',
                  'Payout_Ratio',
                  'EPS_Diluted_Growth',
                  'Weighted_Average_Shs_Out_(Dil)',
                  'Weighted_Average_Shares_Diluted_Growth')
read_file[,!(names(read_file) %in% deleted_names)] -> read_file
dim(read_file)

##
read_file[-c(2,199,201)]<-lapply(read_file[-c(2,199,201)], as.numeric) ## X1, Sector, Class
read_file[2]<- lapply(read_file[2],as.character)## X1
classVariables = sapply(read_file, function(x) class(x))
length(names(which(sapply(read_file, class) == "factor"))) 
length(names(which(sapply(read_file, class) == "numeric"))) 
length(names(which(sapply(read_file, class) == "character"))) 

##
read_file[-201][is.na(read_file[-201])] =0 #Class

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
boruta_stock_train <- Boruta(Class~., data =read_file[-c(1,2,200,202)], doTrace = 3)# don't even try to run it, take forever 
## X,X1,PRICE_VAR,year
#
boruta_stock_train_extra <- Boruta(Class~., data =read_file[-c(1,2,200,202)], doTrace = 3, maxRuns=200)## can do more Runs with maxRuns specified 

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

## test the selected variables performance 
feature_selected <- cbind(variable = rownames(stock_df_extra), stock_df_extra)
rownames(feature_selected) <- 1:nrow(feature_selected)

feature_selected %>% filter(decision=="Confirmed") %>% arrange(desc(meanImp))%>% filter(meanImp>7)# top27_features
feature_selected %>% filter(decision=="Confirmed") %>% arrange(desc(meanImp))%>% filter(meanImp>6)# top57_features

##load test dataset
df_test<- read.csv("test_data.csv")
glimpse(df_test)
summary(df_test)
dim(df_test)

##Check is there any duplicated columns 
df_test$niperEBT==df_test$nIperEBT # same columns
df_test$eBTperEBIT== df_test$ebtperEBIT #Same columns
df_test$Net_Income_Com== df_test$Net_Income ## Not the same columns
df_test$ priceToBookRatio== df_test$PB_ratio ## Not the same columns
df_test$Net_Profit_Margin== df_test$netProfitMargin ## Not the same columns
df_test[,!(names(df_test) %in% deleted_names)] -> df_test


##Convert Class to a factor (and don't use "0" and "1" as levels) 
class(read_file$Class)
## drop any value in class not equal to 1 or o
read_file<-read_file[(read_file$Class=="1" | read_file$Class=="0"),]
## 
read_file$Class <- read_file$Class[read_file$Class != c("True","X1")]
summary(read_file$Class)
read_file$Class <- droplevels(read_file$Class)
## reorder the factor level
read_file$Class<- factor(read_file$Class, levels=rev(levels(read_file$Class)))
levels(read_file$Class)
##Convert Class to a factor (and don't use "0" and "1" as levels) 
class(df_test$Class)
df_test$Class<- as.factor(df_test$Class)
## drop any value in class not equal to 1 or o
df_test<-df_test[(df_test$Class=="1" | df_test$Class=="0"),]
## 
df_test$Class <- df_test$Class[df_test$Class != c("True","X1")]
summary(df_test$Class)
df_test$Class <- droplevels(df_test$Class)
## reorder the factor level
df_test$Class<- factor(df_test$Class, levels=rev(levels(df_test$Class)))
levels(df_test$Class)


#top28 features  accuarcy : 0.679  
classifier <- glm(Class~nIperEBT+ 
                     Effect_of_forex_changes_on_cash+ 
                     Earnings_Yield+ 
                     effectiveTaxRate+
                     priceFairValue+
                     SG.A_to_Revenue+ 
                     EV_to_Free_cash_flow+
                     Weighted_Average_Shares_Growth+ 
                     Gross_Profit_Growth+
                     Sector+ 
                     assetTurnover+ 
                     eBTperEBIT+
                     Net_Income+
                     Net_Income_Com+
                     EV_to_Sales+ 
                     priceToOperatingCashFlowsRatio+
                     priceToBookRatio+
                     priceBookValueRatio+
                     Enterprise_Value_over_EBITDA+
                     operatingCashFlowPerShare+
                     Earnings_Before_Tax_Margin+ 
                     Revenue_Growth+ 
                     Profit_Margin+
                     Inventory_Growth+
                     Free_Cash_Flow_Yield+
                     operatingCashFlowSalesRatio+
                     grossProfitMargin+
                     Earnings_before_Tax,
                   family='binomial', data =read_file[-c(1,2,200,202)])

summary(classifier)
N_test= nrow(df_test)
predictions <- predict(classifier,newdata=df_test[-c(1,2,200,202)],type="response")[1:N_test]
predictions<- round(predictions) %>% as.factor()
## reorder the factor level
predictions<- factor(predictions, levels=rev(levels(predictions)))
levels(predictions)
confusionMatrix(factor(predictions),factor(df_test['Class'][1:N_test,]))


#top51 features accuarcy : 0.524 (the largetest model vglm with multinomial can handel)
classifier_1 <- glm(Class~nIperEBT+ 
                       Effect_of_forex_changes_on_cash+ 
                       Earnings_Yield+ 
                       effectiveTaxRate+
                       priceFairValue+
                       SG.A_to_Revenue+ 
                       EV_to_Free_cash_flow+
                       Weighted_Average_Shares_Growth+ 
                       Gross_Profit_Growth+
                       Sector+ 
                       assetTurnover+ 
                       eBTperEBIT+
                       Net_Income+
                       Net_Income_Com+
                       EV_to_Sales+ 
                       priceToOperatingCashFlowsRatio+
                       priceToBookRatio+
                       priceBookValueRatio+
                       Enterprise_Value_over_EBITDA+
                       operatingCashFlowPerShare+
                       Earnings_Before_Tax_Margin+ 
                       Revenue_Growth+ 
                       Profit_Margin+
                       Inventory_Growth+
                       Free_Cash_Flow_Yield+
                       operatingCashFlowSalesRatio+
                       grossProfitMargin+
                       Earnings_before_Tax+
                       enterpriseValueMultiple+
                       Gross_Margin+
                       Net_Income_per_Share+
                       priceSalesRatio+
                       Net_Profit_Margin+
                       netProfitMargin+
                       EBIT_Margin+
                       EBIT+
                       Operating_Cash_Flow+
                       eBITperRevenue+
                       Free_Cash_Flow_margin+
                       Income_Quality+
                       pretaxProfitMargin+
                       EBITDA_Margin+
                       Graham_Number+
                       dividendpaidAndCapexCoverageRatios+
                       Total_non.current_assets+
                       Free_Cash_Flow+
                       returnOnEquity+
                       Book_Value_per_Share_Growth+
                       Receivables_Turnover+
                       dividendYield+
                       Current_ratio+
                       Operating_Income,
                    family='binomial', data =read_file[-c(1,2,200,202)])
summary(classifier_1)
predictions_1 <- predict(classifier_1,newdata=df_test[-c(1,2,200,202)],type="response")[1:N_test]
predictions_1<- round(predictions_1) %>% as.factor()
## reorder the factor level
predictions_1<- factor(predictions_1, levels=rev(levels(predictions_1)))
levels(predictions_1)
confusionMatrix(predictions_1,df_test['Class'][1:N_test,])

## The most accuarte model so far
## Accuracy :0.694 
## From EBITDA_Margin to dividendpaidAndCapexCoverageRatios stays the same accuarcy
classifier_2 <- glm(Class~nIperEBT+ 
                       Effect_of_forex_changes_on_cash+ 
                       Earnings_Yield+ 
                       effectiveTaxRate+
                       priceFairValue+
                       SG.A_to_Revenue+ 
                       EV_to_Free_cash_flow+
                       Weighted_Average_Shares_Growth+ 
                       Gross_Profit_Growth+
                       Sector+ 
                       assetTurnover+ 
                       eBTperEBIT+
                       Net_Income+
                       Net_Income_Com+
                       EV_to_Sales+ 
                       priceToOperatingCashFlowsRatio+
                       priceToBookRatio+
                       priceBookValueRatio+
                       Enterprise_Value_over_EBITDA+
                       operatingCashFlowPerShare+
                       Earnings_Before_Tax_Margin+ 
                       Revenue_Growth+ 
                       Profit_Margin+
                       Inventory_Growth+
                       Free_Cash_Flow_Yield+
                       operatingCashFlowSalesRatio+
                       grossProfitMargin+
                       Earnings_before_Tax+
                       enterpriseValueMultiple+
                       Gross_Margin+
                       Net_Income_per_Share+
                       priceSalesRatio+
                       Net_Profit_Margin+
                       netProfitMargin+
                       EBIT_Margin+
                       EBIT+
                       Operating_Cash_Flow+
                       eBITperRevenue+
                       Free_Cash_Flow_margin+
                       Income_Quality+
                       pretaxProfitMargin+
                       EBITDA_Margin,
                     family='binomial', data =read_file[-c(1,2,200,202)])
summary(classifier_2)
predictions_2 <- predict(classifier_2,newdata=df_test[-c(1,2,200,202)],type="response")[1:N_test]
predictions_2<- round(predictions_2) %>% as.factor()
## reorder the factor level
predictions_2<- factor(predictions_2, levels=rev(levels(predictions_2)))
levels(predictions_2)
confusionMatrix(predictions_2,df_test['Class'][1:N_test,])

## save results in the environment
# save.image(file='Boruta_feature_selection_results_version2.RData')
## check if its in the current dictory
dir()
## load pervious saved results
load('Boruta_feature_selection_results_version2.RData')



## Plot AUC 
df_test$Class<- df_test$Class %>% as.numeric()
predictions_2<- predictions_2 %>% as.numeric()

pROC_obj <- roc(df_test$Class,predictions_2,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
## title: "Area under the receiver operating characteristic curve (AUC)"
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")


# plot(boruta_stock, xlab = "", xaxt = "n",ylab = "Importance: Z-score", ylim=c(-5, 10),
#      main= " Z-score of every feature in the shuffled dataset",
#      col=c("grey","lightblue2","orangered")[as.numeric(boruta_stock$finalDecision)])
# legend("topleft", legend=unique(levels(boruta_stock$finalDecision)), pch=16, col=c("grey","lightblue2","orangered"))
# 
# lz<-lapply(1:ncol(boruta_stock$ImpHistory),function(i)
#   boruta_stock$ImpHistory[is.finite(boruta_stock$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta_stock$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta_stock$ImpHistory), cex.axis = 0.8,hadj =0.25)



##Radar Chart
select_names <- c('nIperEBT', 
                  'Effect_of_forex_changes_on_cash', 
                  'Earnings_Yield', 
                  'effectiveTaxRate',
                  'priceFairValue',
                  'SG.A_to_Revenue', 
                  'EV_to_Free_cash_flow',
                  'Weighted_Average_Shares_Growth', 
                  'Gross_Profit_Growth',
                  'assetTurnover', 
                  'eBTperEBIT',
                  'Net_Income',
                  'Net_Income_Com',
                  'EV_to_Sales',
                  'priceToOperatingCashFlowsRatio',
                  'priceToBookRatio',
                  'priceBookValueRatio',
                  'Enterprise_Value_over_EBITDA',
                  'operatingCashFlowPerShare',
                  'Earnings_Before_Tax_Margin', 
                  'Revenue_Growth', 
                  'Profit_Margin',
                  'Inventory_Growth',
                  'Free_Cash_Flow_Yield',
                  'operatingCashFlowSalesRatio',
                  'grossProfitMargin',
                  'Earnings_before_Tax',
                  'enterpriseValueMultiple',
                  'Gross_Margin',
                  'Net_Income_per_Share',
                  'priceSalesRatio',
                  'Net_Profit_Margin',
                  'netProfitMargin',
                  'EBIT_Margin',
                  'EBIT',
                  'Operating_Cash_Flow',
                  'eBITperRevenue',
                  'Free_Cash_Flow_margin',
                  'Income_Quality',
                  'pretaxProfitMargin',
                  'EBITDA_Margin',
                  'Class')
## 'Sector' excluded
read_file[,names(read_file) %in% select_names] -> select_data
rename_list<- c("Class",colnames(select_data[1:41]))
rename_list_2<- colnames(select_data[1:41])

## Radar Chart
#############RadarChart############## 
data_summary<-select_data %>% group_by(Class) %>% summarise_all(list(max, min, mean)) %>%  as.data.frame() 
dim(data_summary)
max<- data_summary[,1:42]
max<- apply(max, 2, max) %>% t()%>% as.data.frame()
max <- setNames(max, rename_list)
max<- max[,2:42]
# max_1<- max[1,2:42]
# max_0<- max[2,2:42]
min<- data_summary[,43:83]
min<- apply(min, 2, min) %>% t()%>% as.data.frame()
min <- setNames(min, rename_list_2)
# min_1<- min[1,1:41]
# min_0<- min[2,1:41]
mean<- data_summary[,84:124]
mean <- setNames(mean, rename_list_2)
mean_1<- mean[1,1:41]
mean_0<- mean[2,1:41]
data_summary_1<- rbind(max,min,mean_1)
data_summary_0<- rbind(max,min,mean_0)
data_summary_1 = as.data.frame(sapply(data_summary_1, as.numeric))
data_summary_0 = as.data.frame(sapply(data_summary_0, as.numeric))

data_summary_1_normalized<- BBmisc::normalize(data_summary_1, method = "standardize", range = c(0, 1)) 
data_summary_0_normalized<- BBmisc::normalize(data_summary_0, method = "standardize", range = c(0, 1))
data_summary_1_scaled<- base::scale(data_summary_1) %>% as.data.frame()
data_summary_0_scaled<- base::scale(data_summary_0) %>% as.data.frame()


###############

op <- par(mar=c(10, 6, 10, 6),mfrow=c(1, 2))
#cluster_1: Class=1
radarchart(data_summary_1, 
           pcol=rgb(0.1,0.4,0.7,0.9),
           pfcol=rgb(0.1,0.8,1,0.5),
           #custom the grid
           cglcol="grey", cglty=5, axislabcol="grey",caxislabels=seq(0,20000,100), cglwd=0.8,
           #custom labels
           vlcex=0.8,
           title="Class 1 Radar Chart"
)


#cluster_2: Class=0
radarchart(data_summary_0, 
           pcol=rgb(0.1,0.4,0.7,0.9),
           pfcol=rgb(0.1,0.8,1,0.5),
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
           #custom labels
           vlcex=0.8,
           title="Class 0 Radar Chart"
)


op <- par(mar=c(12, 5, 12, 5),mfrow=c(1, 2))
#cluster_1: Class=1
radarchart(data_summary_1_normalized, 
           pcol=rgb(0.1,0.4,0.7,0.9),
           pfcol=rgb(0.1,0.8,1,0.5),
           #custom the grid
           cglcol="grey", cglty=5, axislabcol="grey", caxislabels=seq(0,1,0.5), cglwd=0.8,
           #custom labels
           vlcex=0.8,
           title="Class 1 Radar Chart normalized"
)


#cluster_2: Class=0
radarchart(data_summary_0_normalized, 
           pcol=rgb(0.1,0.4,0.7,0.9),
           pfcol=rgb(0.1,0.8,1,0.5),
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8,
           title="Class 0 Radar Chart normalized"
)




## scaled radar chart
#op <- par(mar=c(12, 5, 12, 5),mfrow=c(1, 2))
# radarchart(data_summary_1_scaled, 
#            pcol=rgb(0.1,0.4,0.7,0.9),
#            pfcol=rgb(0.1,0.8,1,0.5),
#            #custom the grid
#            cglcol="grey", cglty=5, axislabcol="grey", caxislabels=seq(0,1,0.5), cglwd=0.8,
#            #custom labels
#            vlcex=0.8,
#            title="Class 1 Radar Chart scaled"
# )
# radarchart(data_summary_0_scaled, 
#            pcol=rgb(0.1,0.4,0.7,0.9),
#            pfcol=rgb(0.1,0.8,1,0.5),
#            #custom the grid
#            cglcol="grey", cglty=5, axislabcol="grey", caxislabels=seq(0,1,0.5), cglwd=0.8,
#            #custom labels
#            vlcex=0.8,
#            title="Class 0 Radar Chart scaled"
# )


# ## Random Forest Method,find a set of predictors that best explains the variance in the response variable.
# cf1 <- cforest(Class~nIperEBT+ 
#                  Effect_of_forex_changes_on_cash+ 
#                  Earnings_Yield+ 
#                  effectiveTaxRate+
#                  priceFairValue+
#                  SG.A_to_Revenue+ 
#                  EV_to_Free_cash_flow+
#                  Weighted_Average_Shares_Growth+ 
#                  Gross_Profit_Growth+
#                  Sector+ 
#                  assetTurnover+ 
#                  eBTperEBIT+
#                  Net_Income+
#                  Net_Income_Com+
#                  EV_to_Sales+ 
#                  priceToOperatingCashFlowsRatio+
#                  priceToBookRatio+
#                  priceBookValueRatio+
#                  Enterprise_Value_over_EBITDA+
#                  operatingCashFlowPerShare+
#                  Earnings_Before_Tax_Margin+ 
#                  Revenue_Growth+ 
#                  Profit_Margin+
#                  Inventory_Growth+
#                  Free_Cash_Flow_Yield+
#                  operatingCashFlowSalesRatio+
#                  grossProfitMargin+
#                  Earnings_before_Tax,
#                data= read_file[-c(1,2,200,202)],  ## X, X1, PRICE_VAR, year
#                control=cforest_unbiased(mtry=2,ntree=50))
# RF_variable_importance<- varimp(cf1) # get variable importance, based on mean decrease in accuracy
# varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
# varimpAUC(cf1)  # more robust towards class imbalance.
# 
# ## Relative Importance, the relative importance of variables fed into a lm model can be determined as a relative percentage.
# lmMod <- lm(Class ~ . , data = df_rf)  # fit lm() model
# relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
# sort(relImportance$lmg, decreasing=TRUE)  # relative importance
