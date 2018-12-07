#Demand Model
library('ggplot2')
library('forecast')
library('tseries')


# ARIMA Model
setwd("~/Documents/Github/Hoffman_Leinwand_COMP755_Project")
#setwd("~/Github/Hoffman_Leinwand_COMP755_Project")


# Data <-read.csv('Demand_Data3_Train_CV_2017.csv')
#  Test_Data <- read.csv('Demand_Data3_Test_Predictions_2017.csv')

OData <- read.csv('Demand_Data3_Total_CV_2017_with_zeros.csv')


#Convert those itegers as factors:
OData$PULocationID = as.factor(OData$PULocationID)
OData$Hour = as.factor(OData$Hour)
OData$Day = as.factor(OData$Day)
OData$Day_of_week = as.factor(OData$Day_of_week)
OData$Month = as.factor(OData$Month)


#Test is actually a validation
OTest = OData[OData$Month %in% c(10),]
OTest2 = OData[OData$Month %in% c(11,12),]
OTrain = OData[OData$Month %in% c(1:9),]
#Test2 IS ACUTALLY THE TEST SET OK 


RMSE_list = rep(NA, length(unique(OData$PULocationID)))
MAD_list = rep(NA, length(unique(OData$PULocationID)))

PUL_list = as.numeric(levels(OData$PULocationID))

N_data_list = matrix(rep(NA, length(unique(OData$PULocationID)) * 3), ncol = 3)

for (i in c(1:length(PUL_list))) {
  #for (i in c(1)) {
  print('i:')
  print(i)
  
  
  #Make Train and Test Data
  Data = OData
  Train = OTrain
  Test = OTest
  Test2 = OTest2
  Train = as.data.frame(Train)
  Test = as.data.frame(Test)
  Test2 = as.data.frame(Test2)
  
  N_data_list[i,] = c(length(Train$PULocationID),length(Test$PULocationID), length(Test2$PULocationID))
  
  
  
  print(i / length(PUL_list))
  #Choose a "zip" code
  PUL= PUL_list[i]
  
  #Subset data
  Data = subset(Data, PULocationID == PUL)
  Train = subset(Train, PULocationID == PUL)
  Test = subset(Test, PULocationID == PUL)
  Test2 = subset(Test2, PULocationID == PUL)
  
  #compute residuals
  Train_resids = Train$passenger_count - Train$OLS_fitted_value
  Test_resids = Test$passenger_count - Test$OLS_fitted_value
  Test2_resids = Test2$passenger_count - Test2$OLS_fitted_value
  All_resids = c(Train_resids , Test_resids, Test2_resids)
  
  
  
  #Arima
  Train_resids_ts = ts(Train_resids)
  assign(paste0("fit", 1), auto.arima(Train_resids_ts, seasonal=TRUE))
  
  Test_resids_ts = ts(Test_resids, start = length(Train_resids_ts) + 1)
  Test2_resids_ts = ts(Test2_resids, start = length(Train_resids_ts) +length(Test_resids_ts) + 1)
  
  All_resids_ts = ts(c(Train_resids_ts, Test_resids_ts, Test2_resids))
  All_resids_model = Arima(All_resids_ts, model = fit1)
  #plot(fitted(All_resids_model), col = 'red')
  
  
  #Untransform data
  fitted = fitted(All_resids_model)
  All_normalizers =  c(Train$OLS_fitted_value ,Test$OLS_fitted_value, Test2$OLS_fitted_value )
  
  length(fitted(All_resids_model))
  length(All_normalizers)
  
  #Training Data Plot
  len  = 300
  plot(((Train_resids) +Train$OLS_fitted_value )[0:len], type ='l')
  lines(window( (fitted(All_resids_model ) + Data$OLS_fitted_value)[0:len], start =1), col = 'red')
  
  
  #Test2 Data Plot
  start = length(Train_resids) + length(Test_resids)
  plot(((Test2_resids)+ Test2$OLS_fitted_value)[0:len], type ='l')
  lines(window( ((fitted(All_resids_model ) + All_normalizers)[start+1 : length(All_normalizers)]) , start =1), col = 'red')
  
  #Compute RMSE
  fitted = fitted(All_resids_model) + All_normalizers
  fitted_test2 = fitted[c(length(Train_resids) + length(Test_resids)) : length(All_resids)  ][-1]
  actual_test2 = (Test2_resids+ Test2$OLS_fitted_value)
  
  RMSE = sqrt(sum(fitted_test2 - actual_test2)^2 / length(fitted_test2))
  MAPE = sum(abs((fitted_test2 - actual_test2) / actual_test2)) * (100/length(fitted_test2))
  MAD = mean(abs(fitted_test2 - actual_test2))
  
  
  print('RMSE:')
  print(RMSE)
  
  print("Mean Absolute Deviation:")
  print(MAD)
  
  
  RMSE_list[i] = RMSE
  MAD_list[i] = MAD
  
}


mean(RMSE_list)
mean(MAD_list)


