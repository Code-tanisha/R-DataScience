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

##########################
# OUTLIER ANALYSIS

# before doing outlier analysis 1st we will do datamanipulation
# datamanipulation: convert string categories into factor numeric means we will convert unique string variables to numeric
# we are dealing with column level operation so ncol
# down is marketing train column variable is factor then we will run inside loop ie:
# under factor 1st variable is the dataset and then labels under which we provide labels to each category of the categorical variable
# levels is a function which helps to provide the levels to each of the categorical variables present in the variable

for (i in 1:ncol(marketing_train)) {
  if(class(marketing_train[,i]) == 'factor')
    marketing_train[,i] = factor(marketing_train[,i], labels = 1: length(levels(factor(marketing_train[,i]))))
}

#outlier analysis
# boxplot method
# 1st we need to seperate numerical and categorical variables coz we can find outliers only for the numerical variable
# with the help of the sapply we are extrcting only the index of the numerical variables names

numeric_index = sapply(marketing_train,is.numeric) # selecting only numeric
numeric_data = marketing_train[,numeric_index]
cnames = colnames(numeric_data)
# assign function is used generally in the loop to iteratively assign a particular name to the output results
# if i is one the gn will be 1 so what paste do is paste will add the 2 arguments in 1st iteration i=1, gn=1 
for(i in 1: length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "responded"), data = subset(marketing_train))+
    stat_boxplot(geom = "errorbar", width = 0.5) + geom_boxplot(outlier.colour = "red", fill = "grey", outlier.shape = 18, outlier.size = 1, notch = FALSE) +
    theme(legend.position = "bottom") +
    labs(y = cnames[i], x = "responded") +
    ggtitle(paste("Box plot of responded for", cnames[i])))
}

# plotting plots together
# gridextra is my library name 
# gridarrange will arrange blox plot side by side ie down we are arranging gn1, gn2, gn5 noxplot side by side and no of column = 3
gridExtra::grid.arrange(gn1, gn5, gn2, ncol = 3)
gridExtra::grid.arrange(gn6, gn7, ncol = 2)
gridExtra::grid.arrange(gn8, gn9, ncol = 2)

# Remove outliers using boxplot method
df = marketing_train
marketing_train = df
# below we are deleting those observation which have outliers
# boxplot.stat helps us detect and remove the outliers
# %n% - it is a search operation it will search the right hand sided values in the left hand sided variable is previous

val = marketing_train$previous[marketing_train$previous %in% boxplot.stats(marketing_train$previous)$out]
# below is the function to delete the whole row or observation which contains the outlier
marketing_train = marketing_train[which(!marketing_train$previous %in% val),]

# loop to remove from all variables
# the above lines was to remove the outliers in a single variable but is if have outliers for 10 or 12 variables then we cant write the abbove lines 10 times so:-

for (i in cnames) {
  print(i)
  val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
  #print(length(val))
  marketing_train = marketing_train[which(!marketing_train[,i] %in% val),]  
}

# Replace all outlier with NA and impute
# create NA on "custage"
# we will use the same boxplot method again to detect the outliers and replace it with NA
for (i in cnames) {
  val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
  #print(length(val))
  marketing_train[,i][marketing_train[,i] %in% val] = NA 
}
marketing_train = knnImputation(marketing_train, k = 3)

##########################
# FEATURE SELECTION
# Correlation plot
corrgram(marketing_train[,numeric_index], order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## chi square test
factor_index = sapply(marketing_train,is.factor)
factor_data = marketing_train[,factor_index]
# down we are creating a contigency table
# 1:10 coz we have jus 10 variables to iterate to check this : names(factor_data)

for (i in 1:10) {
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$responded, factor_data[,i])))
}
# from the above chi sq test is the p-value is having value greater than 0.05 thats mean the variable is independent and independent variable does not give much info to develop our model
# down we have housing and loan the p value of them is greater than 0.05 that means they are independent of each othr and we will reject the variale loan and housing amd day-of-week for our further analysis

# Dimention Reduction
# subset command will helps us to exclude variables which are not wanted
# sign of(-) indicated that select all variables except those mentioned in the brackets
marketing_train_deleted = subset(marketing_train, select = -c(pdays, emp.var.rate, day_of_week, loan, housing))
