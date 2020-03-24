
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


#keep .75 of original 
sample_size = floor(0.75 * nrow(crime_data))
training_index = sample(seq_len(nrow(crime_data)), size = sample_size)
#split into a training and testing
train = crime_data[training_index, ]
test = crime_data[-training_index, ]

#now build a matrix of our set
x = model.matrix(crmrte~., train)[,-1]# Response
y = train$crmrte

#first create a ridge model
cv.r = cv.glmnet(x, y, alpha = 0)
print(cv.r$lambda.min) #what's our L?
model.ridge = glmnet(x, y, alpha = 0, lambda = cv.r$lambda.min)
ridgecoefs = coef(model.ridge) #what are our coefficients

cv.l = cv.glmnet(x, y, alpha = 1)
print(cv.l$lambda.min) #what's our L?
model.lasso = glmnet(x, y, alpha = 1, lambda = cv.l$lambda.min)
lassocoefs = coef(model.lasso)

x.test.lasso = model.matrix(crmrte ~., test)[,-1]
predictions.lasso = model.lasso
lasso_predictions = predict(predictions.lasso, x.test.lasso)
RMSE( as.vector(lasso_predictions), crime_data$crmrte)

model.net = train(crmrte ~., data = train, method = "glmnet",trControl = trainControl("cv", number = 10), tuneLength = 10)
print(model.net$bestTune)

coef(model.net$finalModel, model.net$bestTune$lambda)
x.test.net = model.matrix(crmrte ~., test)[,-1]
#predictions.net = model.net %>% predict(x.test.net)
predictions.net = predict(model.net, x.test.net)

data.frame( RMSE.net = RMSE(predictions.net, test$crmrte), Rsquare.net = R2(predictions.net, test$crmrte))

