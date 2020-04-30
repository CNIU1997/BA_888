## Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
library(corrplot)
library(RColorBrewer)
library(ggplot2) 
library(GGally)
library(xtable)
library(readr)
library(tidyverse)

## import dataset for correlation plot
corr_eda<- read_csv("train_eda.csv")
glimpse(corr_eda)
corr_eda$Sector<- NULL
corr_eda$Class<- NULL
corr_eda$X1<-NULL
dim(corr_eda)

## creat correlation matrix 
mcor<-round(cor(corr_eda),2)
mcor
## find out which correlation equal to 1
which.names <- function(DF, value){
  ind <- which(DF==value, arr.ind=TRUE)
  print(paste(rownames(DF)[ind[,"row"]],  colnames(DF)[ind[,"col"]], sep=', '))
}
corr_1<- which.names(mcor,1)
## Results from Excel selection, refers to Duplicated Column Remove Selection Process.xlsx last sheet
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
corr_eda[,!(names(corr_eda) %in% deleted_names)] -> my_data
dim(my_data)
glimpse(my_data)

##25 variabels
select_names <- c('niperEBT',
                  'Effect_of_forex_changes_on_cash',
                  'Earnings_Yield',
                  'effectiveTaxRate',
                  'SG.A_to_Revenue',
                  'priceFairValue',
                  'Weighted_Average_Shares_Diluted_Growth',
                  'EV_to_Free_cash_flow',
                  'Gross_Profit_Growth',
                  'EV_to_Operating_cash_flow',
                  'Weighted_Average_Shares_Growth',
                  'eBTperEBIT',
                  'assetTurnover',
                  'EV_to_Sales',
                  'Net_Income_Com',
                  'Net_Income',
                  'Enterprise_Value_over_EBITDA',
                  'Revenue_Growth',
                  'Operating_Cash_Flow_per_Share',
                  'Inventory_Growth',
                  'Earnings_Before_Tax_Margin',
                  'priceToOperatingCashFlowsRatio',
                  'Free_Cash_Flow_Yield',
                  'Profit_Margin',
                  'priceToBookRatio',
                  'priceBookValueRatio')

my_data[,names(my_data) %in% select_names] -> my_data_1
dim(my_data_1)

##
M <-cor(my_data_1)
## Visualization of a correlation matrix for 25 numerical variables"
corrplot.mixed(M, lower = "circle", upper = "number",tl.pos = "lt",order="hclust",
               addgrid.col = "black", pch.col="black",
               lower.col = brewer.pal(n=9, name="Blues"),upper.col = brewer.pal(n=9, name="Blues"), 
               tl.cex=1.2, number.cex=0.8,tl.col="black",tl.srt=45)
#ggpairs(my_data) ##dont try to run it, it will bomb your local Rstudio

##Correlation matrix analysis
mcor<-round(cor(my_data),2)
mcor
# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

## import dataset for crosstab
eda<- read_csv("train_data.csv")
# glimpse(eda)
# colnames(eda)
dim(eda)

eda<-eda[(eda$Class=="1" | eda$Class=="0"),]
eda$Class <- eda$Class[eda$Class != c("True","X1")]
eda$Sector <- eda$Sector[eda$Sector != c("True","X1")]
dim(eda)

deleted_name<- c('Consolidated_Income',
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
eda[,!(names(eda) %in% deleted_name)] -> my_data
my_data$X1<-NULL
my_data$X1_1<-NULL
my_data$year<- NULL
my_data$PRICE_VAR<-NULL
glimpse(my_data)
dim(my_data)

## Frequency count
crosstab(eda,col.vars = "Class",type = "f")
crosstab(eda,row.vars = "Sector",col.vars = "Class", type = "f")
## Frequency count
crosstab(eda,row.vars = c("Class","Sector"),col.vars = "year", type = "f")



