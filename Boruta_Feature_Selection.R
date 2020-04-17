## import 
library(reshape2)
library(ggplot2)
library(dplyr)
library(Boruta)
library(Amelia)
library(RColorBrewer)
library(aplot)
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

## set seeds for reproduction
set.seed(888)
## feature selections 
boruta_stock_train <- Boruta(Class~., data =read_file[-c(1,2,225,227)], doTrace = 3)# don't even try to run it, take forever

boruta_stock_train_extra <- Boruta(Class~., data =read_file[-c(1,2,225,227)], doTrace = 3, maxRuns=200)## can do more Runs with maxRuns specified 

print(boruta_stock_train)
boruta_stock_train$finalDecision[boruta_stock_train$finalDecision[]=="Confirmed"]
length(boruta_stock_train$finalDecision[boruta_stock_train$finalDecision[]=="Confirmed"])

## since we still have 64 tentative attributes left, let check these variables
## here Tentative features have an importance that is so close to their best shadow features that Boruta is not able to 
## make a decision with the desired confidence in the default number of Random Forest runs.
#take a call on tentative features
boruta_stock <- TentativeRoughFix(boruta_stock_train)
print(boruta_stock)
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

##confirm the importance of the features
getSelectedAttributes(boruta_stock, withTentative = F)
##store results into a dataframe
stock_df <- attStats(boruta_stock)

## save results in the environment
save.image(file='Boruta_feature_selection_results.RData')
## check if its in the current dictory
dir()
## load pervious saved results
load('Boruta_feature_selection_results.RData')


