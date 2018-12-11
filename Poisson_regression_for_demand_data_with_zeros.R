library(readr)
demand_data <- read_csv("UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/demand_data2_2017_with_zeros.csv")

library(data.table)

#Put November and December as the test set, all previous months as training set
d_train=subset(demand_data, Month<11)
d_test = subset(demand_data, Month>10)

#get all locations, put in list
locations =  unique(d_train$PULocationID)

#initialize matrix for output of training data and test data results
d_train_out = data.frame(matrix(NA, nrow=dim(d_train)[1], ncol=dim(d_train)[2]+2))
colnames(d_train_out) = c(colnames(d_train), "OLS_CV_Prediction", "OLS_fitted_value")
d_test_out = data.frame(matrix(NA, nrow=dim(d_test)[1], ncol=dim(d_test)[2]+2))
colnames(d_test_out) = c(colnames(d_test)[1:6], "True_Month", "Month", "OLS_Prediction")

start=Sys.time()
#Fit a regression for each location code
for(i in 1:length(locations)){
 
   print(i)
#use only the current zip code from training and test sets
current_train=subset(d_train, PULocationID==locations[i])
current_test=subset(d_test, PULocationID==locations[i])
#validation poisson regression only on Jan-sept
valid_dfit = glm(passenger_count~ (factor(Hour)+factor(Day_of_week)+ factor(Month))^2, family = poisson, data= subset(current_train, Month <10))

#Run another poisson regression to get fit for Jan-Sept for the test set

big_dfit = glm(passenger_count~ (factor(Hour)+factor(Day_of_week)+ factor(Month))^2, family = poisson, data= current_train)



#create a fake dataset where October values take on september values for prediction using validation regression
fake_train=current_train
fake_train$Month[fake_train$Month==10] = 9
#Based on the model fit without october, calculate the prediction of the true value of october, assuming it is similar to september
current_train[,"OLS_CV_Prediction"]=predict.lm(object = valid_dfit, newdata = fake_train)


current_train[, "OLS_fitted_value"]= big_dfit$fitted.values





colnames(current_test)[7]="True_Month"
current_test=cbind(current_test, rep(10, dim(current_test)[1]) )
colnames(current_test)[8]="Month"
current_test[,"OLS_Prediction"]=predict.lm(object = big_dfit, newdata = current_test)

d_train_out[((i-1)*dim(current_train)[1]+1):(i*dim(current_train)[1]),] = current_train
d_test_out[((i-1)*dim(current_test)[1]+1):(i*dim(current_test)[1]),] = current_test
end=Sys.time()
}
end-start

#write the new data with the predictions from 10 fold validation to file 
write.csv(d_train_out, file = "C:/Users/blein/Documents/UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/Demand_Data3_Train_CV_2017_with_zeros.csv")


write.csv(d_test_out, file = "C:/Users/blein/Documents/UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/Demand_Data3_Test_Predictions_2017_with_zeros.csv")

