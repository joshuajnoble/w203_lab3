
library(tidyverse)
library(effsize)
library(plyr)
library(reshape2)

library(caret)
library(glmnet)

options(scipen=999)
setwd("C:\\Users\\winbase\\MIDS\\w203\\w203_lab3")
crime_data = read.csv("crime_v2.csv")

#clean data, dropping all NA rows
crime_data = crime_data[which(crime_data$county != 'NA'),]

# what is wrong with prbconv?
class(crime_data$prbconv[1][1]) #r thinks it's a factor?

# let's clean these up
crime_data$prbconv = as.numeric(levels(crime_data$prbconv)[as.integer(crime_data$prbconv)])


##########################################################################################
# Compute R^2 from true and predicted values
eval_results = function(true, predicted, df) {
  SSE = sum((predicted - true)^2)
  SST = sum((true - mean(true))^2)
  R_square = 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}
##########################################################################################

#keep .75 of original 
sample_size = floor(0.75 * nrow(crime_data))
training_index = sample(seq_len(nrow(crime_data)), size = sample_size)
#split into a training and testing
train = crime_data[training_index, ]
test = crime_data[-training_index, ]

#now build a matrix of our set
x = model.matrix(crmrte~., train)[,-1]# Response
y = train$crmrte

x_test = model.matrix(crmrte~., test)[,-1]# Response
y_test = test$crmrte

#first create a ridge model
cv.ridge = cv.glmnet(x, y, alpha = 0)
print(cv.ridge$lambda.min) #what's our L?
model.ridge = glmnet(x, y, alpha = 0, lambda = cv.ridge$lambda.min)
ridgecoefs = coef(model.ridge) #what are our coefficients

#try on our training data
predictions_train <- predict(cv.ridge, s = cv.ridge$lambda.min, newx = x)
eval_results(y, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(cv.ridge, s = cv.ridge$lambda.min, newx = x_test)
eval_results(y_test, predictions_test, test) #awful rsquared, why?

# now work up a lasso
cv.lasso = cv.glmnet(x, y, alpha = 1)
print(cv.lasso$lambda.min) #what's our L?
model.lasso = glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min)
lassocoefs = coef(model.lasso)

lasso_predictions_train = predict(cv.lasso, s = cv.lasso$lambda.min, newx = x)
eval_results(y, lasso_predictions_train, train)

lasso_predictions_test = predict(cv.lasso, s = cv.lasso$lambda.min, newx = x_test)
eval_results(y_test, lasso_predictions_test, test) #similarly awful?


enet_model = train(crmrte ~., data = train, method = "glmnet",trControl = trainControl("cv", number = 10), tuneLength = 10)
print(enet_model$bestTune)

coef(enet_model$finalModel, enet_model$bestTune$lambda)

enet_train = predict(enet_model, s = enet_model$bestTune$lambda, newx = x)
eval_results(y, enet_train, train)

enet_predictions = predict(enet_model, x_test)
eval_results(y_test, enet_predictions, test) #also bad. Hmm

data.frame( RMSE.net = RMSE(predictions.net, test$crmrte), Rsquare.net = R2(predictions.net, test$crmrte))
