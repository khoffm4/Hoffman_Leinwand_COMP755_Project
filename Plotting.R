#temporary plotss

library('ggplot2')
library('forecast')
library('tseries')


# ARIMA Model
setwd("~/Documents/Github2/Hoffman_Leinwand_COMP755_Project")
#setwd("~/Github/Hoffman_Leinwand_COMP755_Project")


#Data <-read.csv('Demand_Data3_Train_CV_2017.csv')
 

OData4 = OData[OData$PULocationID ==  4,]

plot(OData4$passenger_count[1:7000], type = 'l', main = 'Ridership in Zone 4 Over the Year without Preprocessing', xlab = 'Time Block', ylab= 'Number of Pickups from taxis')
plot(OData4$OLS_CV_Prediction[1:7000], type = 'l', main = 'Ridership in Zone 4 Approximated with Linear Regression', xlab = 'Time Block', ylab= 'Number of Pickups from taxis')
plot((OData4$passenger_count - OData$OLS_fitted_value)[1:7000], ylim = c(-50,50), type = 'l', main = 'Residuals from Linear Regression Preprocessing', xlab = 'Time Block', ylab = 'Number of Pickups from taxis')

OData <- read.csv('neighbors_2017_combined_train_and_test_with_predictions_fixed_with_zeros.csv')
OData2 = OData[OData$PU_DO ==  "100_164",]

plot(OData2$time_delta[1:7000], type = 'l', main = 'Time to go from Zone 100 to Zone 164 Over the Year', xlab = 'Time Block', ylab= 'Time')
plot(OData2$OLS_fitted_value[1:7000], type = 'l', main = 'Time to go from Zone 100 to Zone 164 Approximated with Linear Regression', xlab = 'Time Block', ylab= 'Time')
plot((OData2$time_delta - OData2$OLS_fitted_value)[1:7000] , type = 'l', main = 'Residuals from Linear Regression Preprocessing', xlab = 'Time Block', ylab= 'Time')
