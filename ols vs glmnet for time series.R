library(readr)
demand_data <- read_csv("UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/Demand_Data2.csv")


#Put June as the test set, all previous months as training set
train=subset(demand_data, Month<6)
test = subset(demand_data, Month==6)

#Cut the training set into 10 folds
train["Fold"] = ceiling(runif(dim(train)[1], min = 0, max=10))

#Fit a model on 9 out of 10 folds 10 times
for(i in 1:10){
assign(paste0("fit", i), lm(passenger_count~pickup_zip + factor(Hour)*factor(Day_of_week)+ factor(Hour)*factor(Month)+ factor(Day_of_week)*factor(Month)+ factor(Hour)*factor(Day_of_week)*factor(Month), data= subset(train, Fold !=i)))
       
       }


#Based on the model fit without the left out fold, calculate the prediction of the true value of the holdout fold
for(j in 1:dim(train)[1]){
train[j,"OLS_CV_Prediction"]=predict.lm(object = eval(parse(text=paste0("fit", train[j, "Fold"] ))), newdata = train[j, ])
}


write.csv(train, file = "C:/Users/blein/Documents/UNC/Classes 2018 Fall/COMP 755 -Machine Learning/Project/Demand_Data3_Train.csv")

###########################################################


