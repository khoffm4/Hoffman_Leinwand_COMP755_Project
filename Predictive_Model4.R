library('ggplot2')
library('forecast')
library('tseries')


# ARIMA Model
setwd("~/Documents/Github/Hoffman_Leinwand_COMP755_Project")
#setwd("~/Github/Hoffman_Leinwand_COMP755_Project")

RMSE_list = rep(NA, length(unique(Data$PULocationID)))
MAPE_list = rep(NA, length(unique(Data$PULocationID)))




PUL_list = as.numeric(levels(Data$PULocationID))

for (i in c(1:length(PUL_list))) {
  
  
  Data <-read.csv('Demand_Data3_Train_CV_2017.csv')
  Test_Data <- read.csv('Demand_Data3_Test_Predictions_2017.csv')
  
  
  #Convert those itegers as factors:
  Data$PULocationID = as.factor(Data$PULocationID)
  Data$Hour = as.factor(Data$Hour)
  Data$Day = as.factor(Data$Day)
  Data$Day_of_week = as.factor(Data$Day_of_week)
  Data$Month = as.factor(Data$Month)
  
  Test = Data[Data$Month == 10,]
  Data = Data[Data$Month %in% c(1:9),]
  
  
  #Make Train and Test Data
  Train = Data
  Test = Test_Data
  Train = as.data.frame(Train)
  Test = as.data.frame(Test)
  
  
  print(i / length(PUL_list))
  #Choose a "zip" code
  PUL= PUL_list[i]
  Train = subset(Data, PULocationID == PUL)
  Test = subset(Test, PULocationID == PUL)
  
  #Ridership graph
  plot(c(Train$passenger_count, Test$passenger_count), type ='l')
  
  
  Train_resids = Train$passenger_count - Train$OLS_fitted_value
  Test_resids = Test$passenger_count - Test$OLS_Prediction
  
  
  #min  = min(Train_resids,Test_resids)
  #Train_resids = log(Train_resids + abs(min)) - min
  #Test_resids = log(Test_resids + abs(min))- min
  
  
  ##Box cox transofmr
  #library(MASS)
  #trans = Train_resids + abs(min(Train_resids)) + 3
  #z = boxcox(trans ~ 1, lambda = seq(-6,6,0.1))
  
  
  All_resids = c(Train_resids , Test_resids)
  #length(All_resids)
  #plot(log(All_resids -min(All_resids)))
  All_normalizers =  c(Train$OLS_fitted_value ,Test$OLS_Prediction)
  #length(All_normalizers)
  
  #Arima
  Train_resids_ts = ts(Train_resids)
  assign(paste0("fit", 1), auto.arima(Train_resids_ts, seasonal=TRUE))
  
  Test_resids_ts = ts(Test_resids, start = length(Train_resids_ts) + 1)
  All_resids_ts = ts(c(Train_resids_ts, Test_resids_ts))
  All_resids_model = Arima(All_resids_ts, model = fit1)
  plot(fitted(All_resids_model), col = 'red')
  
  
  
  #Untransform data
  fitted = fitted(All_resids_model)
  fitted = fitted

  
  plot(c(Train$passenger_count, Test$passenger_count), type ='l')
  lines(window( fitted + All_normalizers , start =1), col = 'red')
  
  #lines(window(fitted(All_resids_model), start = length(Train_resids_ts)+1),  col = 'red')
  
  #Compute RMSE
  fitted = fitted(All_resids_model) + All_normalizers
  fitted_test = fitted[c(6549: length(All_resids)  )]
  actual_test = Data$passenger_count[c(6549: length(All_resids) )]
  
  RMSE = sqrt(sum(fitted_test - actual_test)^2 / length(Test_resids))
  MAPE = sum(abs((fitted_test - actual_test) / actual_test)) * (100/length(Test_resids))
  
  MAPE = mean(abs((fitted_test - actual_test))/ actual_test) * 100
  
  print('RMSE:')
  print(RMSE)
  
  print("MAPE:")
  print(MAPE)

  
  RMSE_list[i] = RMSE
  MAPE_list[i] = MAPE
}
