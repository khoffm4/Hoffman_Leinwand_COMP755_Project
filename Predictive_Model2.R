library('ggplot2')
library('forecast')
library('tseries')


# ARIMA Model
setwd("~/Documents/Github/Hoffman_Leinwand_COMP755_Project")
#setwd("~/Github/Hoffman_Leinwand_COMP755_Project")


Data <-read.csv('Demand_Data3_Train_CV_2017.csv')
Test_Data <- read.csv('Demand_Data3_Test_Predictions_2017.csv')

#Convert those itegers as factors:
Data$PULocationID = as.factor(Data$PULocationID)
Data$Hour = as.factor(Data$Hour)
Data$Day = as.factor(Data$Day)
Data$Day_of_week = as.factor(Data$Day_of_week)
Data$Month = as.factor(Data$Month)

#And for testing Data
Test_Data$PULocationID = as.factor(Test_Data$PULocationID)
Test_Data$Hour = as.factor(Test_Data$Hour)
Test_Data$Day = as.factor(Test_Data$Day)
Test_Data$Day_of_week = as.factor(Test_Data$Day_of_week)
Test_Data$Month = as.factor(Test_Data$Month)



#Make Train and Test Data
Train = Data
Test = Test_Data
Train = as.data.frame(Train)
Test = as.data.frame(Test)

#Create the lagged training and testing data
#library(data.table)
##Train_Data  = as.data.frame(Data$passenger_count)
#colnames(Train_Data)[colnames(Train_Data)=="Data$passenger_count"] <- "Y"
#setDT(Train_Data)[, c("X") := .(shift( Data$passenger_count, 1L, fill = NA, type = "lag"))]
#Train_Data= Train_Data[c(-1)]

#Test_Data  = as.data.frame(Test$passenger_count)
#colnames(Test_Data)[colnames(Test_Data)=="Test$passenger_count"] <- "Y"
#setDT(Test_Data)[, c("X") := .(shift( Test$passenger_count, 1L, fill = NA, type = "lag"))]
#Test_Data= Test_Data[c(-1)]


#count_ma = ts(resids, frequency=3)
#decomp = stl(count_ma, s.window="periodic")
#deseasonal_cnt <- seasadj(decomp)

#assign(paste0("fit", PULocationID), auto.arima(count_ma, seasonal=TRUE))

#PUL_list = unique(Data$PULocationID)

#Choose a zip code
PULocationID = 4
Passenger_Count <- subset(Data, PULocationID == PULocationID)
Passenger_Count <- Passenger_Count$passenger_count
Passenger_Count <- as.numeric(Passenger_Count)

PULocationID =  Data[which(Data$PULocationID == PULocationID),]



resids = Data$passenger_count - Data$OLS_fitted_value
Train_resids = resids[c(1:370470)]
Test_resids = resids[c(370471: length(resids))]
Train_resids = Train_resids[!is.na(Train_resids)]


#Arima
count_ma = ts(Train_resids)
#decomp = stl(count_ma, s.window="periodic")
#deseasonal_cnt <- seasadj(decomp)

assign(paste0("fit", 1), auto.arima(count_ma, seasonal=TRUE))

#fcast <- forecast(fit1, h=24)
#predict(fcast)

#check=predict(fit1, n.ahead = 1, newxreg=  as.matrix(rnorm(100)))
data2 = ts(Test$passenger_count, start = length(count_ma) + 1)
data12 = ts(c(count_ma, data2))
model12 = Arima(data12, model = fit1)
plot(data2-Test$OLS_Prediction)
lines(window(fitted(model12), start = length(count_ma)+1),  col = 'red')

