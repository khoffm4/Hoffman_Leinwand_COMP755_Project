library('ggplot2')
library('forecast')
library('tseries')
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials


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

PUL_list = unique(Data$PULocationID)

#Choose a zip code

PULocationID = 4
Passenger_Count <- subset(Data, PULocationID == PULocationID)
Passenger_Count <- Passenger_Count$passenger_count
Passenger_Count <- as.numeric(Passenger_Count)

PULocationID =  Data[which(Data$PULocationID == PULocationID),]


for (i in c(1:length(unique(Data$PULocationID)))){
  print(i)
  PULocationID =  as.integer(PUL_list[i])

  
  
  Passenger_Count <- subset(Train, PULocationID == PULocationID)
  Passenger_Count <- Passenger_Count$passenger_count
  Passenger_Count <- as.numeric(Passenger_Count)
  
  Zip_Data =  Train[ which(Train$PULocationID == PULocationID),]
  
  
  n = length(Passenger_Count) - 30
  #Toy Data
  Pass =  Passenger_Count[1:n]
  Pass = as.data.frame(Pass)
  
  
  #Plot
  #ggplot() +
  #  geom_line(data = Pass, aes(x = c(1:n) ,y = Pass)) + ylab('Cleaned Bicycle Count')
  
  
  #Moving Average
  #Pass$maday = as.vector(ma(Pass$Pass, order=24)) # using the clean count with no outliers
  #Pass$maweek = as.vector(ma(Pass$Pass, order=24* 7)) # using the clean count with no outliers
  #Pass$mamonth = as.vector(ma(Pass$Pass, order=24* 30)) # using the clean count with no outliers
  
  #ggplot() +
  #  geom_line(data = Pass, aes(x = c(1:n) ,y = Pass, colour = "Ridership")) + 
  #  geom_line(data = Pass, aes(x = c(1:n) ,y = maday,colour = "Daily Moving Average")) + 
  #  geom_line(data = Pass, aes(x = c(1:n) ,y = maweek ,colour = "Weekly Moving Average")) +
  #  geom_line(data = Pass, aes(x = c(1:n) ,y = mamonth ,colour = "Monthly Moving Average"))
  
  #Regression (pass ~ weekday + month + hour + interactions)
  #(pass ~   month  + interactions)
  #lm1 <- lm(passenger_count ~ ( Hour  + Day_of_week + Month), data = Data)
  #resids = lm1$residuals
  #plot(resids)
  
  lm2 <- lm(passenger_count ~ ( Hour  + Day_of_week + Month), data = Train)
  resids = lm2$residuals
  #plot(resids2, type = 'l')
  
  #lm3 <- lm(passenger_count ~ (Hour  + Day_of_week + Month)^3, data = Data)
  #resids3 = lm3$residuals
  #plot(resids3, type = 'l')

#Arima
  count_ma = ts(resids, frequency=3)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  #plot(decomp)
  
  
  
  #Acf
  #Acf(count_ma, main='')
  #Pacf(count_ma, main='')
  #Spikes at 1 and high 20s
  
  
  #subtract 1
  #count_d1 = diff(deseasonal_cnt, differences = 1)
  #plot(count_d1)
  #Acf(count_d1, main='PACF for Differenced Series')
  #better? Not sure about spike at 1.
  
  #ARIMA
  assign(paste0("fit", PULocationID), auto.arima(deseasonal_cnt, seasonal=TRUE))
  
  #tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
  
}  
  
  
  #Forcast
  fcast <- forecast(fit1, h=24)
  #plot(fcast)
  predict(fcast)
  
  
#How good is the forcast?s
  
  
  #Do a VAR on everythin
  
  
  
  
  
  