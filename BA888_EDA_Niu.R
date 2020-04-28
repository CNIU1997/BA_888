## Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
library(corrplot)
library(RColorBrewer)
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
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
crosstab(eda,row.vars = "Sector",col.vars = "Class", type = "f")
## Frequency count
crosstab(eda,row.vars = c("Class","Sector"),col.vars = "year", type = "f")


## ## import dataset for correlation plot
corr_eda<- read_csv("train_eda.csv")
glimpse(corr_eda)
corr_eda$Sector<- NULL
corr_eda$Class<- NULL
dim(corr_eda)
my_data= corr_eda[,1:45]

##
M <-cor(my_data)
corrplot(M, type="lower", order="hclust",
         col=brewer.pal(n=9, name="Blues"))
rquery.cormat(my_data)
ggpairs(my_data)

##Correlation matrix analysis
mcor<-round(cor(my_data),2)
mcor
# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper
