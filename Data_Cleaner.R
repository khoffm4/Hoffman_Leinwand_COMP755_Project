library(readr)
library(tidyverse)
#Set wd
setwd("~/Documents/Github/Hoffman_Leinwand_COMP755_Project")

train <- read.csv('train.csv')
original_train <- train 
dates <- as.Date(train$pickup_datetime)
train <- cbind(train, dates)

head <- head(train)
pickuptimeh <- as.POSIXlt(head$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
pickuptime <- as.POSIXlt(train$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
train$pickup_datetime <- pickuptime

pickuptime_hour <- trunc(train$pickuptime, 'hour')
cbind(train, pickuptime_hour)
