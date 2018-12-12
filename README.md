Hoffman_Leinwand_COMP755_Project

Instructions:
The report is in Combining Train and Test set with predictions for neighbors data.ipynb

Description of Files:

*Adjacency_2017.csv – Our updated adjacency matrix

*Adding Zero Hours to groupby.ipynb – adds any locations/time combinations where there were no trips to the datasets, recording all pertinent information as zeros

*Demand_Data3_Total_CV_2017_with_zeros.csv - Data for Demand in each Pickup Zone within lower Manhatten. The data is from 2017 and the "with_zeros" means that if a time was missing from the dataset, then we have declared that noone was picked up at the time. Also contains the fit from the linear regression preprocessing.

*neighbors_2017_combined_train_and_test_with_predictions_fixed_with_zeros.csv - Data for Trip Duration to each neighboring Pickupzone in 2017 with zeros imputed like above. Also contains data from the poisson regression preprocessing

*Combining Train and Test set with predictions for neighbors data.ipynb - The report.

*Demand_and_Time_Matrix_Creator.ipynb- Takes in the LSTMS for each zone and outputs a Demand and Time matrix.

*Discounter.ipynb - Takes in the Demand and Time Matrix, computes the discounting, and computes the accuracy based on our method.

*Predictive_Model_Demand.ipynb- Takes in the Demand_Data3_Total_CV_2017_with_zeros.csv and outputs the LSTM models for demand and their MAD and RMSE for every Pickupzone in Manhatten

*Predictive_Model_Neighbors.ipynb- Takes in the neighbors_2017_combined_train_and_test_with_predictions_fixed_with_zeros.csv and outputs the LSTM models for trip duration and their MAD and RMSE for every Pickupzone in Manhatten

*Plotting.R - Function for creating the R plots for the report

*Predictive_Model_Demand.R- Takes in and outputs the ARIMA models for demand and their MAD and RMSE for every Pickupzone in Manhatten

*Predictive_Model_Neighbors.R- Takes in and outputs the ARIMA models for time and their MAD and RMSE for every Pickupzone in Manhatten

*Hour_Day_Month_Seperator.ipynb - Takes in the unprocessed data and creates factors for Hour, Day, and Month


*Fixed_groupby_2017.ipynb – Takes the downloaded data, converts from each row being a trip to each row being an hour and location combination, aggregating our data and making it more usable.



*Poisson_regression_for_demand_data_with_zeros.R – runs poisson regression for demand data, different models for validation and test sets


*Censored_OLS_for_duration_data_with_zeros.R - runs OLS regression for duration data censoring at 0, different models for validation and test sets

*Combining Train and Test set with predictions for neighbors data.ipynb – puts train and test sets after prediction into 1 file for easier use

*
