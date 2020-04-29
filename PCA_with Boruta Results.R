options(stringsAsFactors=FALSE)
options(digits = 3)
set.seed(8881)
## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(mlbench)
library(caret)
library(factoextra)
library(cluster)
library(radarchart)
library(fmsb)
library(grDevices)
# 'niperEBT',
# 'Effect_of_forex_changes_on_cash',
# 'Earnings_Yield',
# 'effectiveTaxRate',
# 'SG.A_to_Revenue',
# 'priceFairValue',
# 'Weighted_Average_Shares_Diluted_Growth',
# 'EV_to_Free_cash_flow',
# 'Gross_Profit_Growth',
# 'EV_to_Operating_cash_flow',
# 'Weighted_Average_Shares_Growth',
# 'eBTperEBIT', 
# 'assetTurnover',
# 'EV_to_Sales',
# 'Net_Income_Com',
# 'Net_Income',
# 'Enterprise_Value_over_EBITDA',
# 'Revenue_Growth',
# 'Operating_Cash_Flow_per_Share'
# 'Inventory_Growth'
# 'Earnings_Before_Tax_Margin',
# 'operatingCashFlowPerShare', 
# 'priceToOperatingCashFlowsRatio', 
# 'POCF_ratio',
# 'Free_Cash_Flow_Yield',
# 'Consolidated_Income',
# 'Profit_Margin',
# 'priceToBookRatio',
# 'PB_ratio',
# 'priceBookValueRatio',
# 'PTB_ratio'


# load the data
train= read.csv('train_data.csv')
dim(train)
train<- na.omit(train)

#Remove Redundant Features
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

train[,names(train) %in% select_names] -> select_data
select_data[] <- lapply(select_data, function(x) as.numeric(as.character(x)))

# explore raw dataset 
glimpse(select_data)
skimr::skim(select_data)
dim(select_data)
select_data<- na.omit(select_data) ## remove NAs
dim(select_data)
#pca
c <- prcomp(select_data,center = TRUE, scale = TRUE)

# Inspect eigenvalues
head(get_eigenvalue(c),12)
# Visualize eigenvalues 
fviz_screeplot(c, addlables = T, ylim = c(0,40)) #4-5 dimensions

c_pcs <- predict(c, newdata = select_data)
class(c_pcs)
c_pcs <- as.data.frame(c_pcs)

##using 6 conponents
c_pca2 <- c_pcs[,1:2]
c_pca6 <- c_pcs[,1:6]
# c_pca5 <- c_pcs[,1:5]

#k-mean using pca(6)
c_scale <- scale(c_pca6)

#eval numer of k using silhouette and wss
fviz_nbclust(c_scale, kmeans, method = 'wss', k.max = 10)

k_wss = function(k) {
  km = kmeans(c_scale, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}
# 
x <- 1:30
wass <- map_dbl(x,k_wss)
plot(x,wass, type ="b") # 4

fviz_nbclust(c_scale, kmeans, method = 'silhouette', k.max = 10)#silhouette suggests 2 clusters

#Kmeans k of 2/3/4/5/6
pca_k2 <- kmeans(c_scale,2,25,30)
pca_k3 <- kmeans(c_scale,3,25,30)
pca_k4 <- kmeans(c_scale,4,25,30)
pca_k5 <- kmeans(c_scale,5,25,30)
pca_k6 <- kmeans(c_scale,6,25,30)
# 
table(pca_k2$cluster)
table(pca_k3$cluster)
table(pca_k4$cluster)
table(pca_k5$cluster)
table(pca_k6$cluster)
# 
pca_k2$tot.withinss
pca_k3$tot.withinss
pca_k4$tot.withinss
pca_k5$tot.withinss
pca_k6$tot.withinss

fviz_cluster(pca_k2,select_data)
fviz_cluster(pca_k3,select_data)
fviz_cluster(pca_k4,select_data)
fviz_cluster(pca_k5,select_data)
fviz_cluster(pca_k6,select_data)

# final decision: 2 clusters
##adding back clusters and 2 pca variables to original dataset
train1<-na.omit(select_data)
train1$cluster <- pca_k2$cluster
final_data <- cbind(train1,c_pca2)

###analyze clusters using final data......
glimpse(final_data)

#
data_summary<-final_data %>% group_by(cluster) %>% summarise_all(mean) %>% dplyr::select(-starts_with("PC"))
final_data2<-final_data %>% dplyr::select(-starts_with("PC")) %>% group_by(cluster)


## Radar Chart
#############RadarChart############## 
data_summary<- final_data %>% 
  group_by(cluster) %>% summarise_all(mean) %>% 
  dplyr::select(-starts_with("PC"))%>% as.data.frame() 
summary<-summary(data_summary)
dim(summary)
max<- summary[6,1:32]
max<-as.numeric(gsub("Max.   :", "", max))
max
data_summary <- rbind(max, rep(-25,32), data_summary)
data_summary$cluster<-NULL
###############

#cluster_1
cluster_1<-data_summary[c(1:2,3),]
radarchart(cluster_1, 
           pcol=rgb(0.1,0.4,0.7,0.9),
           pfcol=rgb(0.1,0.8,1,0.5),
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8
)

#cluster_2
cluster_2<-data_summary[c(1:2,4),]
radarchart(cluster_2, 
           pcol=rgb(0.1,0.4,0.7,0.9),
           pfcol=rgb(0.1,0.8,1,0.5),
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)
