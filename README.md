Hoffman_Leinwand_COMP755_Project


Description of Files:
Fixed_groupby_2017.ipynb – Takes the downloaded data, converts from each row being a trip to each row being an hour and location combination, aggregating our data and making it more usable.
Adjacency_2017.csv – Our updated adjacency matrix
Adding Zero Hours to groupby.ipynb – adds any locations/time combinations where there were no trips to the datasets, recording all pertinent information as zeros
Poisson_regression_for_demand_data_with_zeros.R – runs poisson regression for demand data, different models for validation and test sets
Censored_OLS_for_duration_data_with_zeros.R - runs OLS regression for duration data censoring at 0, different models for validation and test sets
Combining Train and Test set with predictions for neighbors data.ipynb – puts train and test sets after prediction into 1 file for easier use