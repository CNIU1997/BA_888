## import 
library(reshape2)
library(ggplot2)
library(dplyr)
library(Boruta)
library(Amelia)
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

##
set.seed(888)
##
boruta_stock_train <- Boruta(Class~., data =read_file[-c(1,2,227)], doTrace = 3, maxRuns=200)# don't even try to run it, take forever

boruta_stock_train_extra <- Boruta(Class~., data =read_file[-c(1,2,227)], doTrace = 3, maxRuns=200)

print(boruta_stock_train)
