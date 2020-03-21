# Variable Descriptions: 
# 
# 1    county|               county identifier
# 2      year|                            1987
# 3    crmrte|     crimes committed per person
# 4    prbarr|         'probability' of arrest
# 5   prbconv|     'probability' of conviction
# 6   prbpris| 'probability' of prison sentence
# 7    avgsen|             avg. sentence, days
# 8     polpc|               police per capita
# 9   density|             people per sq. mile
# 10    taxpc|          tax revenue per capita
# 11     west|           =1 if in western N.C.
# 12  central|           =1 if in central N.C.
# 13    urban|                   =1 if in SMSA
# 14 pctmin80|            perc. minority, 1980
# 15     wcon|       weekly wage, construction
# 16     wtuc|    wkly wge, trns, util, commun
# 17     wtrd| wkly wge, whlesle, retail trade
# 18     wfir|    wkly wge, fin, ins, real est
# 19     wser|      wkly wge, service industry
# 20     wmfg|         wkly wge, manufacturing
# 21     wfed|         wkly wge, fed employees
# 22     wsta|       wkly wge, state employees
# 23     wloc|        wkly wge, local gov emps
# 24      mix| offense mix: face-to-face/other
# 25  pctymle|              percent young male

library(sandwich)
library(car)
library(lmtest)
library(stargazer)

library(tidyverse)
library(effsize)
library(plyr)
library(reshape2)
library(ggiraphExtra)

setwd("C:\\Users\\winbase\\MIDS\\w203\\w203_lab3")
crime_data = read.csv("crime_v2.csv")

#Checking the Summary of the data
summary(crime_data)

#Chekcing the structure of the data
nrow(crime_data)
length(crime_data)
str(crime_data)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Cleaning the data

#1.Removing the last 6 rows of the data which are NA
crime_data = crime_data[which(crime_data$county != "NA"),]
# summary(crime_data)

#2. Converting Pbrconv from factor to numeric and then to probability
summary(crime_data$prbconv)
plot(crime_data$prbconv)

crime_data$prbconv = as.numeric(crime_data$prbconv)/100
hist(crime_data$prbconv)

str(crime_data)
+++++++++++++++++++++++++++++++++++++++++++++++++
#Correlation checks
  
corr_matrix <- cor(crime_data)
round(corr_matrix[3,], 2)

"""
Which are the variable with strong corr ( positive or Neg)

1. prbarr -39%
2. prbconv -40% 
3. Density +73%
4. Taxpc +45%
5. Urban +62%
6. Wcon +39%
7. Wtrd +41%
8. Wfed +49%
"""


++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Trying out some regressions models

model1 = lm(crmrte ~ density, data = crime_data)
plot(model1)

model2 = lm(crmrte ~ density+urban+taxpc, data = crime_data)
plot(model2)


model3 = lm(crmrte ~ density+density+urban+taxpc+ prbarr + prbconv, data = crime_data)
plot(model3)
hist(model2$residuals, breaks =50)

stargazer(model1,model2,model3, type = "text", star.cutoffs = c(0.05,0.01,0.001))




coeftest(model1,vcov=vcovHC)
