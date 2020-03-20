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

# load the data
train= read.csv('train_data.csv')
dim(train)
train<- na.omit(train)

#Remove Redundant Features
train %>% select(-X1,-X,-year,-Sector,-Class)-> train
train[] <- lapply(train, function(x) as.numeric(as.character(x)))

# explore raw dataset 
glimpse(train)
skimr::skim(train)
dim(train)

#pca
c <- prcomp(na.omit(train),center = TRUE, scale = TRUE)
fviz_screeplot(c, addlables = T, ylim = c(0,10)) #3 dimensions

c_pcs <- predict(c, newdata = na.omit(train))
class(c_pcs)
c_pcs <- as.data.frame(c_pcs)

##using 3 conponents
c_pca3 <- c_pcs[,1:3]
c_pca2 <- c_pcs[,1:2]

#k-mean using pca(3)
c_scale <- scale(c_pca3)

#eval numer of k using silhouette and wss
fviz_nbclust(c_scale, kmeans, method = 'wss', k.max = 30)

k_wss = function(k) {
  km = kmeans(c_scale, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}
# 
x <- 1:10
wass <- map_dbl(x,k_wss)
plot(x,wass, type ="b") # 4

fviz_nbclust(c_scale, kmeans, method = 'silhouette', k.max = 20)#silhouette suggests 2 clusters

#Kmeans k of 2/3/4/5/6
pca_k2 <- kmeans(c_scale,2,25,30)
# pca_k3 <- kmeans(c_scale,3,25,30)
# pca_k4 <- kmeans(c_scale,4,25,30)
# pca_k5 <- kmeans(c_scale,5,25,30)
# pca_k6 <- kmeans(c_scale,6,25,30)
# 
# table(pca_k2$cluster)
# table(pca_k3$cluster)
# table(pca_k4$cluster)
# table(pca_k5$cluster)
# table(pca_k6$cluster)
# 
# pca_k2$tot.withinss
# pca_k3$tot.withinss
# pca_k4$tot.withinss
# pca_k5$tot.withinss
# pca_k6$tot.withinss

# fviz_cluster(pca_k2,na.omit(train))
# fviz_cluster(pca_k3,na.omit(train))
# fviz_cluster(pca_k4,na.omit(train))
# fviz_cluster(pca_k5,na.omit(train))
# fviz_cluster(pca_k6,na.omit(train))

# final decision: 2 clusters
##adding back clusters and 2 pca variables to original dataset
train<-na.omit(train)
train$cluster <- pca_k2$cluster
final_data <- cbind(train,c_pca2)

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
max<- summary[6,1:223]
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
