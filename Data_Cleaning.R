library(readr)
library(dplyr)
library(tidyverse)
library(skimr)

##***************************Notes**************************************
#1) variable names include both upper case and lower cases 
#2) some variable names inlcude underscores but others include no space

# LEAVE YOUR NOTES HERE IF NECESSARY 
##*********************************************************************


##read datasets
dat_14 <- read_csv("2014_Financial_Data.csv")
dat_15 <- read_csv("2015_Financial_Data.csv")
dat_16 <- read_csv("2016_Financial_Data.csv")
dat_17 <- read_csv("2017_Financial_Data.csv")
dat_18 <- read_csv("2018_Financial_Data.csv")

############data cleaning############
colnames(dat_14) = gsub(" ", "_", colnames(dat_14))
colnames(dat_15) = gsub(" ", "_", colnames(dat_15))
colnames(dat_16) = gsub(" ", "_", colnames(dat_16))
colnames(dat_17) = gsub(" ", "_", colnames(dat_17))
colnames(dat_18) = gsub(" ", "_", colnames(dat_18))

##change '201X_PRICE_VAR_[%]' col into one name 'PRICE_VAR'
colnames(dat_14)[colnames(dat_14) == '2015_PRICE_VAR_[%]'] <- "PRICE_VAR"
colnames(dat_15)[colnames(dat_15) == '2016_PRICE_VAR_[%]'] <- "PRICE_VAR"
colnames(dat_16)[colnames(dat_16) == '2017_PRICE_VAR_[%]'] <- "PRICE_VAR"
colnames(dat_17)[colnames(dat_17) == '2018_PRICE_VAR_[%]'] <- "PRICE_VAR"
colnames(dat_18)[colnames(dat_18) == '2019_PRICE_VAR_[%]'] <- "PRICE_VAR"

##add year column to each year
dat_14$year <- 2014
dat_15$year <- 2015
dat_16$year <- 2016
dat_17$year <- 2017
dat_18$year <- 2018

##combine data from 2014 to 2017 as train_data, 208 as test_data 
train_data <- rbind(dat_14, dat_15, by = "X1") %>% 
  rbind(dat_16,by ="X1", all = T) %>%
  rbind(dat_17, by ="X1", all = T)

test_data <- dat_18

##replace NAs with 0 
train_data %>% 
  mutate_all(~replace(., is.na(.), 0)) -> train_data

test_data %>% mutate_all(~replace(.,is.na(.),0)) -> test_data


