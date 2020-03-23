#####################################################################
# Program Name: lab3_Rfile_RS.R
# Analyst     : Ryan Sawasaki
#
#####################################################################

# Setup

setwd("~C:/Users/rsawa/OneDrive/Desktop/lab3_working")
getwd()

library(car) # not working. issues with 'rlang' package
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyverse) # not working. issues with 'rlang' package

#####################################################################

# Analysis of Crime Data

# load data
crime_data = read.csv("crime_v2.csv")

# view data
head(crime_data)

# remove NA
crime_data = crime_data[which(crime_data$county != 'NA'),]

# prbconv is factor with 92 levels, convert to numeric, probability
crime_data$prbconv = as.numeric(crime_data$prbconv)/100

# view dataframe
str(crime_data)

# view variable correlation with crmrte 
corr_matrix <- cor(crime_data)
round(corr_matrix[3,], 2)

#####################################################################

# Bivariate Model (review individual cases)

var1 = crime_data$prbconv
crime_data$prbarr/crime_data$prbconv

# Examine Variable
summary(var1)
hist(var1,main = "Variable", xlab = NULL)

# Model variable vs crime rate

# fit the linear model 
(model_biv = lm(crime_data$crmrte ~ var1, data = crime_data))

# create the scatterplot / regression plot
plot(jitter(var1), jitter(crime_data$crmrte), xlab = "X Value", ylab = "Y Value", main = "X versus Y")
abline(model_biv)

# summary
summary(model_biv)
plot(model_biv)

#####################################################################

# model 1
model1 = lm(crime_data$crmrte ~ crime_data$density, data = crime_data)
plot(model1)

# model 2
model2 = lm(crime_data$crmrte ~ crime_data$density 
                              + crime_data$polpc
                              + crime_data$prbarr
                              + crime_data$prbconv
                               ,data = crime_data)
plot(model2)
coefficients(model2)

# model 3
model3 = lm(crime_data$crmrte ~ crime_data$density 
                              + crime_data$polpc
                              + crime_data$prbarr
                              + crime_data$prbconv
                              + crime_data$pctmin80
                              + crime_data$pctymle
                             # + crime_data$
                             # + crime_data$
                               ,data = crime_data)
plot(model3)

#####################################################################

stargazer(model1, model2, model3, type = "text",star.cutoffs = c(0.05,0.01,0.001))
AIC(model1)
AIC(model2)
AIC(model3)


#####################################################################
  
# Checking Influential Cases
plot(model1, which=4)
plot(model2, which=4) 
plot(model3, which=4)

#####################################################################

# Testing for zero conditional mean and heteroskedasticity
# get the residual vs. fitted value
plot(model1, which=1)
plot(model2, which=1)
plot(model3, which=1)

# Testing for heteroskedasticity
# scale-location plot
plot(model1, which=3)
plot(model2, which=3)
plot(model3, which=3)

# Testing for heteroskedasticity
# Breusch-Pagan test.
bptest(model1)
bptest(model2)
bptest(model3)

#####################################################################

# Testing for Normality
# Q-Q plot 
plot(model1, which=2)
plot(model2, which=2)
plot(model3, which=2)

# Residual plot
hist(model1$residuals, breaks = 50)
hist(model2$residuals, breaks = 50)
hist(model3$residuals, breaks = 50)


#Shapiro-Wilk test of normality.
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)

#####################################################################

# to address zero conditional mean and/or heteroskedasticity
# try changing the functional form
model3_log = lm(log(crime_data$crmrte) ~ crime_data$density, data = crime_data)
plot(model3_log)

# Check Tests 
# Testing for zero conditional mean and heteroskedasticity
# get the residual vs. fitted value
plot(model3_log, which=1)

# Testing for heteroskedasticity
# scale-location plot
plot(model3_log, which=3)

# Testing for heteroskedasticity
# Breusch-Pagan test.
bptest(model3_log)

#####################################################################

# To address heteroskedasticity, we use robust standard errors.
coeftest(model3_log, vcov = vcovHC)
vcovHC(model3_log)

# display the model with robust standard errors
(se.model3_log = sqrt(diag(vcovHC(model3_log))))

stargazer(model3_log, type = "text",
          se = list(se.model3_log ),
          star.cutoffs = c(0.05, 0.01, 0.001))
