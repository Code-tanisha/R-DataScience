rm(list = ls())
setwd("C:\\Edwisor\\R")

# Load libraries 

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "MASS", "rpart", "gbm", "ROSE", "e1071", "Information")

# install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# read the data
marketing_train = read.csv("marketing_tr.csv", header = T, na.strings = c(" ", "", "NA"))

# explore the data
str(marketing_train)

# missing values analysis
# Create dataframe with missing percentage
# we have written 2 coz we are doing column level operation
# down we need to imputr the missing values on each column of the function thats why we created a function
missing_val = data.frame(apply(marketing_train, 2, function(x){sum(is.na(x))}))

# down we are just converting data into proper shape
#Convert row names into columns
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
# rename the variable name
names(missing_val)[1] = "Missing_percentage"

# Calculate percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(marketing_train)) * 100

#Arrange in descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]

## Rearranging the columns
missing_val = missing_val[,c(2,1)]

# now we will store this data for our further analysis means write the output back into disk
#row.names = F means we dont want the index
write.csv(missing_val, "Missing_perc.csv", row.names = F)

# plot the grapH for missing values
# ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage), y = Missing_percentage)) + geom_bar(stat =  "identity", fill = "grey")+xlab("Parameter") + ggtitle("Missing data percentage(Train)")+theme_bw()

#impute missing values
# here we will create a missing values and see which method is best suitable
# marketing_train[71,1], this is a randomly taken from 71 row no and 1st column and its actial value is 21. now let us cheq with methods
# Actual value = 29
# mean = 40.01
# medin = 38
# KNN = 34.11
# by above methods we will cheq which method gived closest value to the actual value and the we will freeze that method

# mean method
# down in sq brackets we write condition ie if it is is.na means if it is missing value then compute mean
marketing_train$custAge[is.na(marketing_train$custAge)] = mean(marketing_train$custAge, na.rm = T)
# Median method
marketing_train$custAge[is.na(marketing_train$custAge)] = median(marketing_train$custAge, na.rm = T)

# KNN Imputation
marketing_train = knnImputation(marketing_train, k = 5)
sum(is.na(marketing_train))