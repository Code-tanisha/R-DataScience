rm(list = ls(all = T))
setwd("C:\\Edwisor\\R")

# load libraries
library(rpart)
library(MASS)
library(DMwR)
# load practice data
df = read.csv("birthwt.csv", header = T, na.strings = c(" ", "", "NA"))

# divide the data in train and test 
# below line means take a random sammple from each observations and 80% of the saple is selected

train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]
# desicion tree for regression
# rpart for regression
# bwt ~. means take all the variables as independent varibale except babyweight
# if we are builting a regression model we need to write model = anova , if we would be building a decision tree moden then model = class

fit = rpart(bwt ~., data = train, method = "anova")

# predict for new test cases
# -10 means i am removing my target variable which is my 10th variable and providing the remaining variables to apply on the model

predictions_DT = predict(fit, test[,-10])

#####################################
# Error Metrics
# There are 4 types of error metrics which helps us to analyse the performance of any analytical regressor model
# MAE, MAPE, MSE , RMSE
# Calculate MAPE
mape = function(y, yhat){     # here we have defined a function
           mean(abs((y - yhat)/y)) * 100
}
mape(test[,10], predictions_DT)  # Here is calling of function
# MAPE = 13.49
# Alternate method to improve the accuracy
# it calculates all the metrics together
regr.eval(test[,10], predictions_DT, stats = c('mae', 'rmse', 'mape', 'mse'))
