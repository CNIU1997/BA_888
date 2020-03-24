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

# 'Revenue_Growth', 'Cash_and_cash_equivalents',
# 'Effect_of_forex_changes_on_cash', 'Net_cash_flow_/_Change_in_cash',
# 'operatingCashFlowPerShare', 'freeCashFlowPerShare', 'Graham_Number',
# 'Graham_Net-Net', 'Gross_Profit_Growth', 'Operating_Income_Growth',
# 'EPS_Diluted_Growth', 'Weighted_Average_Shares_Growth',
# 'Weighted_Average_Shares_Diluted_Growth', 'Operating_Cash_Flow_growth',
# 'Receivables_growth', 'Inventory_Growth', 'Asset_Growth',
# 'SG&A_Expenses_Growth'

# load the data
train= read.csv('train_data.csv')
dim(train)
train<- na.omit(train)

#Remove Redundant Features
select_names <- c('Revenue_Growth', 
                  'Cash_and_cash_equivalents',
                  'Effect_of_forex_changes_on_cash', 
                  'Net_cash_flow_/_Change_in_cash',
                  'operatingCashFlowPerShare', 
                  'freeCashFlowPerShare', 
                  'Graham_Number',
                  'Graham_Net-Net', 
                  'Gross_Profit_Growth', 
                  'Operating_Income_Growth',
                  'EPS_Diluted_Growth', 
                  'Weighted_Average_Shares_Growth',
                  'Weighted_Average_Shares_Diluted_Growth', 
                  'Operating_Cash_Flow_growth',
                  'Receivables_growth', 
                  'Inventory_Growth', 
                  'Asset_Growth',
                  'SG&A_Expenses_Growth')

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

##using 4 conponents
c_pca4 <- c_pcs[,1:4]
# c_pca5 <- c_pcs[,1:5]

#k-mean using pca(5)
c_scale <- scale(c_pca4)

#eval numer of k using silhouette and wss
fviz_nbclust(c_scale, kmeans, method = 'wss', k.max = 30)

k_wss = function(k) {
  km = kmeans(c_scale, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}
# 
x <- 1:30
wass <- map_dbl(x,k_wss)
plot(x,wass, type ="b") # 4

fviz_nbclust(c_scale, kmeans, method = 'silhouette', k.max = 20)#silhouette suggests 2 clusters

#Kmeans k of 2/3/4/5/6
pca_k2 <- kmeans(c_scale,2,25,30)
pca_k3 <- kmeans(c_scale,3,25,30)
pca_k4 <- kmeans(c_scale,4,25,30)
pca_k5 <- kmeans(c_scale,5,25,30)
# pca_k6 <- kmeans(c_scale,6,25,30)
# 
table(pca_k2$cluster)
table(pca_k3$cluster)
table(pca_k4$cluster)
table(pca_k5$cluster)
# table(pca_k6$cluster)
# 
pca_k2$tot.withinss
pca_k3$tot.withinss
pca_k4$tot.withinss
pca_k5$tot.withinss
# pca_k6$tot.withinss

fviz_cluster(pca_k2,select_data)
fviz_cluster(pca_k3,select_data)
fviz_cluster(pca_k4,select_data)
fviz_cluster(pca_k5,select_data)
# fviz_cluster(pca_k6,select_data))

# final decision: 2 clusters
##adding back clusters and 2 pca variables to original dataset
train1<-na.omit(select_data)
train1$cluster <- pca_k2$cluster
final_data <- cbind(train1,c_pca2)

###analyze clusters using final data......
glimpse(final_data)

#
data_summary<-final_data %>% group_by(cluster) %>% summarise_all(mean) %>% select(-starts_with("PC"))
final_data2<-final_data %>% select(-starts_with("PC")) %>% group_by(cluster)


## Radar Chart
#############RadarChart############## 
data_summary<- final_data %>% 
  group_by(cluster) %>% summarise_all(mean) %>% 
  select(-starts_with("PC"))%>% as.data.frame() 
summary<-summary(data_summary)
max<- summary[6,1:21]
max<-as.numeric(gsub("Max.   :", "", max))
max
data_summary <- rbind(max, rep(0,223), data_summary)
data_summary$cluster<-NULL
###############

#cluster_1
cluster_1<-data_summary[c(1:2,3),]
radarchart(cluster_1, 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=1
)
#cluster_2
cluster_2<-data_summary[c(1:2,4),]
radarchart(cluster_2, 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)
