## Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
library(corrplot)
library(RColorBrewer)
library(ggplot2) 
library(GGally)
library(xtable)
library(readr)
library(tidyverse)


## import dataset for crosstab
eda<- read_csv("train_data.csv")
glimpse(eda)
colnames(eda)

eda<-eda[(eda$Class=="1" | eda$Class=="0"),]
eda$Class <- eda$Class[eda$Class != c("True","X1")]
eda$Sector <- eda$Sector[eda$Sector != c("True","X1")]

## Frequency count
crosstab(eda,col.vars = "Class",type = "f")
crosstab(eda,row.vars = "Sector",col.vars = "Class", type = "f")
## Frequency count
crosstab(eda,row.vars = c("Class","Sector"),col.vars = "year", type = "f")


## import dataset for correlation plot
corr_eda<- read_csv("train_eda.csv")
glimpse(corr_eda)
corr_eda$Sector<- NULL
corr_eda$Class<- NULL
dim(corr_eda)

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
                  'operatingCashFlowPerShare', 
                  'priceToOperatingCashFlowsRatio', 
                  'POCF_ratio',
                  'Free_Cash_Flow_Yield',
                  'Consolidated_Income',
                  'Profit_Margin',
                  'priceToBookRatio',
                  'PB_ratio',
                  'priceBookValueRatio',
                  'PTB_ratio')
# 25 variabels 
# select_names <- c('niperEBT',
#                   'Effect_of_forex_changes_on_cash',
#                   'Earnings_Yield',
#                   'effectiveTaxRate',
#                   'SG.A_to_Revenue',
#                   'priceFairValue',
#                   'Weighted_Average_Shares_Diluted_Growth',
#                   'EV_to_Free_cash_flow',
#                   'Gross_Profit_Growth',
#                   'EV_to_Operating_cash_flow',
#                   'Weighted_Average_Shares_Growth',
#                   'eBTperEBIT', 
#                   'assetTurnover',
#                   'EV_to_Sales',
#                   'Net_Income_Com',
#                   'Net_Income',
#                   'Enterprise_Value_over_EBITDA',
#                   'Revenue_Growth',
#                   'Operating_Cash_Flow_per_Share',
#                   'Inventory_Growth',
#                   'Earnings_Before_Tax_Margin', 
#                   'priceToOperatingCashFlowsRatio',
#                   'Free_Cash_Flow_Yield',
#                   'Profit_Margin',
#                   'priceToBookRatio',
#                   'priceBookValueRatio')

corr_eda[,names(corr_eda) %in% select_names] -> my_data

##
M <-cor(my_data)
## Visualization of a correlation matrix for top 30 numerical variables"
corrplot.mixed(M, lower = "circle", upper = "number",tl.pos = "lt",order="hclust",
               addgrid.col = "black", pch.col="black",
               lower.col = brewer.pal(n=9, name="Blues"),upper.col = brewer.pal(n=9, name="Blues"), 
               tl.cex=0.9, number.cex=0.5,tl.col="black",tl.srt=45)
#ggpairs(my_data) ##dont try to run it, it will bomb your local Rstudio
corrplot.mixed(corr_eda,lower = "circle", upper = "number",tl.pos = "lt",order="hclust",
               addgrid.col = "black", pch.col="black",
               lower.col = brewer.pal(n=9, name="Blues"),upper.col = brewer.pal(n=9, name="Blues"), 
               tl.cex=1, number.cex=0.5,tl.col="black",tl.srt=45)

##
tmp = mcor # Copy matrix
tmp[ tmp < -0.2 | tmp > 0.2 ] = 0
corrplot.mixed(tmp,lower = "circle", upper = "number",tl.pos = "lt",order="hclust",
               addgrid.col = "black", pch.col="black",
               lower.col = brewer.pal(n=9, name="Blues"),upper.col = brewer.pal(n=9, name="Blues"), 
               tl.cex=1, number.cex=0.5,tl.col="black",tl.srt=45,na.label = "NA")

##Correlation matrix analysis
mcor<-round(cor(my_data),2)
mcor
# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

upper<-tmp
upper[upper.tri(tmp)]<-""
upper<-as.data.frame(upper)
upper
dim(upper)
