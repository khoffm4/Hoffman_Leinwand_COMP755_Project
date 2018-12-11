#Predictive model 6 but done on time

library('ggplot2')
library('forecast')
library('tseries')


# ARIMA Model
setwd("~/Documents/Github2/Hoffman_Leinwand_COMP755_Project")
#setwd("~/Github/Hoffman_Leinwand_COMP755_Project")


# Data <-read.csv('Demand_Data3_Train_CV_2017.csv')
#  Test_Data <- read.csv('Demand_Data3_Test_Predictions_2017.csv')

OData <- read.csv('neighbors_2017_combined_train_and_test_with_predictions_fixed_with_zeros.csv')

#Replace -1 with NaN
OData[OData == -1] = NaN

#Convert those itegers as factors:
OData$PU_DO = as.factor(OData$PU_DO)
OData$hour = as.factor(OData$hour)
OData$day_of_week = as.factor(OData$day_of_week)
OData$month = as.factor(OData$month)
OData$True_Month = as.factor(OData$True_Month)



#Test is actually a validation
OTest = OData[OData$month %in% c(10),]
#OTest = OTest[OTest$True_Month <10,]

OTest2 = OData[OData$True_Month %in% c(11,12),]
OTrain = OData[OData$month %in% c(1:9),]
#Test2 IS ACUTALLY THE TEST SET OK 


RMSE_list = rep(NA, length(unique(OData$PU_DO)))
MAD_list = rep(NA, length(unique(OData$PU_DO)))
PUL_list = (levels(OData$PU_DO))
N_data_list = matrix(rep(NA, length(unique(OData$PU_DO)) * 3), ncol = 3)


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
  

  print(i / length(PUL_list))
  #Choose a "zip" code
  PUL= PUL_list[i]
  
  #Subset data
  Data = subset(Data, PU_DO == PUL)
  Train = subset(Train, PU_DO == PUL)
  Test = subset(Test, PU_DO == PUL)
  Test2 = subset(Test2, PU_DO == PUL)
  
  N_data_list[i,] = c(length(Train$time_delta),length(Test$time_delta), length(Test2$time_delta))
  
  
  #compute residuals
  Train_resids = Train$time_delta - Train$OLS_fitted_value
  Test_resids = Test$time_delta - Test$OLS_fitted_value
  Test2_resids = Test2$time_delta - Test2$OLS_Prediction
  All_resids = c(Train_resids , Test_resids, Test2_resids)
  
  
  
  #Arima
  Train_resids_ts = ts(Train_resids)
  assign(paste0("fit", i), auto.arima(Train_resids_ts, seasonal=TRUE))
  
  Test_resids_ts = ts(Test_resids, start = length(Train_resids_ts) + 1)
  Test2_resids_ts = ts(Test2_resids, start = length(Train_resids_ts) +length(Test_resids_ts) + 1)
  
  All_resids_ts = ts(c(Train_resids_ts, Test_resids_ts, Test2_resids))
  All_resids_model = Arima(All_resids_ts, model = fit1)
  plot(fitted(All_resids_model), col = 'red')
  
  
  #Untransform data
  fitted = fitted(All_resids_model)
  All_normalizers =  c(Train$OLS_fitted_value ,Test$OLS_fitted_value, Test2$OLS_Prediction )
  
  length(fitted(All_resids_model))
  length(All_normalizers)
  
  #Training Data Plot
  len  = 300
  plot(((Train_resids) +Train$OLS_fitted_value  )[0:len], type ='l', main = 'Time to go from Zone 4 to Zone 164 and its ARIMA prediction', xlab ='Time', ylab = 'Time')
  lines(window( (fitted + All_normalizers )[0:len], start =1), col = 'red')
  
  plot(((Train_resids) +Train$OLS_fitted_value ), type ='l')
  lines(window( (fitted + All_normalizers )[0:len], start =1), col = 'red')
  
  
  #Test2 Data Plot
  start = length(Train_resids) + length(Test_resids)
  plot(((Test2_resids)+ Test2$OLS_Prediction)[0:len], type ='l')
  lines(window( ((fitted + All_normalizers)[start+1 : length(All_normalizers)]) , start =1), col = 'red')
  
  #Compute RMSE
  fitted = fitted(All_resids_model) + All_normalizers
  fitted_test2 = fitted[c(length(Train_resids) + length(Test_resids)) : length(All_resids)  ][-1]
  actual_test2 = (Test2_resids+ Test2$OLS_Prediction)
  
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


#RMSE_list = RMSE_list[-c(1,2)]
#MAPE_list = MAPE_list[-c(1,2)]

mean(RMSE_list)
mean(MAD_list)
