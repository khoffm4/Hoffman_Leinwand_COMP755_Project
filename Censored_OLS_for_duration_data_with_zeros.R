
library(readr)

neighbors_data <- read_csv("UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/neighbors_data_2017_fixed_with_zeros.csv")
#neighbors_data = neighbors_data[,c(11:16)]

#Put November and December as the test set, all previous months as training set
n_train=subset(neighbors_data, month<11)
n_test = subset(neighbors_data, month>10)

n_valid_train = subset(neighbors_data, month<10)

#initialize matrix for output of training data and test data results
n_train_out = data.frame(matrix(NA, nrow=dim(n_train)[1], ncol=dim(n_train)[2]+2))
colnames(n_train_out) = c(colnames(n_train), "OLS_CV_Prediction", "OLS_fitted_value")
n_test_out = data.frame(matrix(NA, nrow=dim(n_test)[1], ncol=dim(n_test)[2]+2))
colnames(n_test_out) = c(colnames(n_test)[1:4], "True_Month", colnames(n_test)[6:8], "month", "OLS_Prediction")



    

valid_nfit_lm =lm(time_delta~ PU_DO+ factor(PU_DO)*factor(hour) +(factor(hour)+factor(day_of_week)+ factor(month))^2, data= subset(n_train[, c(3,4,5,6,8)], month<10))


valid_nfit_lm =lm(time_delta~ PU_DO+ factor(PU_DO) +(factor(hour)+factor(day_of_week))^2+ factor(month), data= n_valid_train)

fake_train=n_train
fake_train$month[fake_train$month=="10"] = "09"


n_train[,"OLS_CV_Prediction"]=predict(object = valid_nfit_lm, newdata = fake_train)

big_nfit_lm =lm(time_delta~ PU_DO+ factor(PU_DO) +(factor(hour)+factor(day_of_week))^2+ factor(month), data= n_train)


n_train[,"OLS_fitted_value"]=predict(object = big_nfit_lm, newdata = n_train)


colnames(n_test)[5]="True_Month"
n_test=cbind(n_test, rep(10, dim(n_test)[1]) )
colnames(n_test)[9]="month"
n_test[,"OLS_Prediction"]=predict.lm(object = big_nfit_lm, newdata = n_test)


##CENSORING AT 0

n_train$OLS_CV_Prediction[n_train$OLS_CV_Prediction<0] = 0
n_train$OLS_fitted_value[n_train$OLS_fitted_value<0] = 0

n_test$OLS_Prediction[n_test$OLS_Prediction<0] = 0

write.csv(n_test, file = "C:/Users/blein/Documents/UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/Neighbors_Data3_Test_Predictions_2017_fixed_with_zeros.csv")


write.csv(n_train, file = "C:/Users/blein/Documents/UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/Neighbors_Data3_Train_CV_2017_fixed_with_zeros.csv")


