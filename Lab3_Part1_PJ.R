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

#setwd("~/Desktop/W203/w203_lab3-master")
# crime_data = read.csv("crime_v2.csv")
# crime_data = crime_v2

crime_v2 <- read_csv("Home Works and Lab/LAB 3/crime_v2.csv")
crime_data = crime_v2


#Checking the Summary of the data
summary(crime_data)

#Chekcing the structure of the data
nrow(crime_data)
length(crime_data)
str(crime_data)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Cleaning the data

#1.Removing the last 6 rows of the data which are NA
crime_data = crime_data[which(crime_data$county != 'NA'),]
summary(crime_data)
str(crime_data)

#2. Converting Pbrconv from factor to numeric and then to probability
summary(crime_data$prbconv)
plot(crime_data$prbconv)

crime_data$prbconv = as.numeric(crime_data$prbconv)/100
hist(crime_data$prbconv)

str(crime_data)
summary(crime_data)
+++++++++++++++++++++++++++++++++++++++++++++++++
#Correlation checks
  
corr_matrix <- cor(crime_data)
round(corr_matrix[3,], 2)


# Which are the variable with strong corr ( positive or Neg)
# 
# 1. prbarr -39%
# 2. prbconv -40% 
# 3. Density +73%
# 4. Taxpc +45%
# 5. West -0.35
# 6. Urban +62%
# 7. Wcon +39%
# 8. Wtrd +41%
# 9. Wfed +49%
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  


###########################################################################################
#
# building models - Adding transformations
# 
###########################################################################################

#really basic model
# arr_and_conv = lm(log(crmrte) ~ crime_data$prbconv + crime_data$prbarr, data = crime_data)
# ggplot(crime_data,aes(y=prbarr,x=prbconv,color=crmrte)) + 
#   geom_point(size=2) +
#   stat_smooth(method="lm",se=FALSE) +
#   scale_color_gradient(low = "green", high = "red", space = "Lab" )
# plot(arr_and_conv)
# summary(arr_and_conv)

#Model of deterrant variables
deterrent_model = lm(crmrte ~ prbconv + prbarr + prbpris+ avgsen+ polpc, data = crime_data)
coeftest(deterrent_model, vcov. = vcovHC, type = "HC1") #Prbconv, prbarr, polpc are signif with White SE
summary(deterrent_model) #adjusted rsquared is 52%,
plot(deterrent_model)#low ZCM, slight skew, little Hetrosked, minimal outliers 51

log_deterrent_model = lm(log(crmrte) ~ prbconv + prbarr + prbpris+ avgsen+ polpc, data = crime_data)
coeftest(log_deterrent_model, vcov. = vcovHC, type = "HC1")
summary(log_deterrent_model) #adjusted rsquared is 54.4%, (Prbconv, prbarr, polpc are signif)
plot(log_deterrent_model) #very low ZCM, No Skew, little Hetrosked, minimal outliers just 51

log_deterrent_model = lm(log(1+100*crmrte) ~ prbconv + prbarr + prbpris+ avgsen+ polpc, data = crime_data)
coeftest(log_deterrent_model, vcov. = vcovHC, type = "HC1")
summary(log_deterrent_model) #adjusted rsquared is 54.4%, (Prbconv, prbarr, polpc are signif)
plot(log_deterrent_model) #low ZCM, little Hetrosked, minimal outliers 


#Model of demogrpahic variables
demogrpahic_model = lm(crmrte ~  density + taxpc + pctmin80+mix+ pctymle , data = crime_data)
coeftest(demogrpahic_model, vcov. = vcovHC, type = "HC1")
summary(demogrpahic_model) #adjusted rsquared is 68.5%, (All var except mix are signif)
plot(demogrpahic_model) #NO ZCM, high skew, High Hetrosked, 2 outliers 25, 59

log_demogrpahic_model = lm(log(crmrte) ~  density + taxpc + pctmin80+mix+ pctymle , data = crime_data)
coeftest(log_demogrpahic_model, vcov. = vcovHC, type = "HC1")
summary(log_demogrpahic_model) #adjusted rsquared is 54%, ()
plot(log_demogrpahic_model) #Has ZCM, No skew, High Hetrosked, 2 outliers 25, 59

log_demogrpahic_model = lm(log(1+100*crmrte) ~  density + taxpc + pctmin80+mix+ pctymle , data = crime_data)
coeftest(log_demogrpahic_model, vcov. = vcovHC, type = "HC1")
summary(log_demogrpahic_model) #adjusted rsquared is 54%, ()
plot(log_demogrpahic_model) #Has ZCM, No skew, High Hetrosked, 2 outliers 25, 59


#Model of Wage variables
wage_model = lm(crmrte ~ wcon + wtuc + wtrd + wfir + wser + wmfg + wfed + wsta + wloc, data = crime_data)
coeftest(wage_model, vcov. = vcovHC, type = "HC1")
summary(wage_model) # Adj R-squared of 25% ( Jointly signific run F-test)
plot(wage_model) #NO ZCM, high Skew, High HetroSked, one outlier

log_wage_model = lm(log(1+100*crmrte) ~ wcon + wtuc + wtrd + wfir + wser + wmfg + wfed + wsta + wloc, data = crime_data)
coeftest(log_wage_model, vcov. = vcovHC, type = "HC1")
summary(log_wage_model) # Adj R-squared of 27% (wfed signif + Jointly signific run F-test)
plot(log_wage_model) #NO ZCM, low Skew, High HetroSked, one outlier 84


########### FROM JOSH ##########################


#mixing them seems to do best
mixed_model = lm(crime_data$crmrte ~ crime_data$pctymle + crime_data$density + crime_data$taxpc + crime_data$pctmin80 + crime_data$prbarr + crime_data$prbconv, data = crime_data)
summary(mixed_model) #Adj R-squared of 75%, all signif
plot(mixed_model) #High ZCM, High skew, massive Hetrosked, many outliers

log_mixed_model = lm(log(1+100*crmrte) ~ crime_data$pctymle + crime_data$density + crime_data$taxpc + crime_data$pctmin80 + crime_data$prbarr + crime_data$prbconv, data = crime_data)
summary(log_mixed_model) #Adj R-squared of 72%, all signif
plot(log_mixed_model) #High ZCM, some skew, massive Hetrosked, many outliers


# compute standardized residuals
mm_standard = rstandard(mixed_model)
id = order(mm_standard)
mm_standard[id[1]] #biggest overestimate is -2.23 SDs over regression line

#this graph shows the two big outliers
std_resid = rstandard(mixed_model)  #map all of the residuals
cooks_D = cooks.distance(mixed_model) #get the cooks distance
hat_values = hatvalues(mixed_model) #calculate all the hat values
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2,	2),	lty=2)

mmframe = data.frame(resid	= residuals(mixed_model), pred = predict(mixed_model)) 
ggplot(mmframe, aes(pred, abs(resid))) + geom_point() + geom_smooth()

#mixing them with outliers removed seems to doS well
mixed_model_2 = lm(crmrte ~ pctymle + density + pctmin80 + taxpc + prbarr + prbconv + polpc, data = crime_data)
# lets find out about our model
summary(mixed_model_2) #Adj R-squared of 81%, all signif
# this gives us 4 charts
plot(mixed_model_2)#low ZCM, some skew, decent Hetrosked, 2 outliers 25,51

#mixing them with outliers removed seems to doS well
log_mixed_model_2 = lm(log(1+100*crmrte) ~ pctymle + density + pctmin80 + taxpc + prbarr + prbconv + polpc, data = crime_data)
# lets find out about our model
summary(mixed_model_2) #Adj R-squared of 81%, all signif
# this gives us 4 charts
plot(mixed_model_2)#low ZCM, low skew, decent Hetrosked, 2 outliers 25,51


###########################################################################################
#
# Checking parsimony of models
# 
###########################################################################################


# extract the Aikike information criterion for each model, Lower AIC is better
extractAIC(mixed_model)
AIC(mixed_model, k=2)

extractAIC(mixed_model_2)
AIC(mixed_model_2, k=2)

extractAIC(wage_model)
extractAIC(log_wage_model)

extractAIC(demogrpahic_model)
extractAIC(log_demogrpahic_model)

extractAIC(deterrent_model)
extractAIC(log_deterrent_model)

# extract the Bayes information criterion for each model
BIC(mixed_model_2)
BIC(mixed_model)
BIC(wage_model)
BIC(demogrpahic_model)
BIC(deterrent_model)

# examine the effect of each element
base = lm(crmrte ~ 1, data = crime_data)
mm_step = step(base, scope = formula(mixed_model_2), direction = "forward")

library(lmtest)
library(sandwich)
library(stargazer)

vcov = vcovHC(mixed_model_2, type = "HC1")
errors = coeftest(mixed_model_2, vcov)
stargazer(mixed_model_2, errors, type = "text", star.cutoffs = c(0.05,0.01,0.001))

#map all of the residuals
std_resid = rstandard(mixed_model_2)
#get the cooks distance
cooks_D = cooks.distance(mixed_model_2)
#calculate the 'hat values' aka leverage for each observation
hat_values = hatvalues(mixed_model_2)
#now plot our residuals by our leverages, we can see the obs that have extremely high leverage
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2,	2),	lty=2)

#find how far each obs is from the SDs as a whole
mm2_standard = rstandard(mixed_model_2)
#sort them
id = order(mm2_standard)
mm2_standard[id[1]] #biggest overestimate is -2.35 SDs over regression line

#now let's see how much our residuals vary
mmframe2 = data.frame(resid	= residuals(mixed_model_2), pred = predict(mixed_model_2)) 
ggplot(mmframe2, aes(pred, abs(resid))) + geom_point() + geom_smooth()


# from Robert

######################


all_in_model = lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density
                  + taxpc + west + central + urban + pctmin80 + wcon
                  + wtuc + wtrd + wfir + wser + wmfg + wfed + wsta + wloc
                  + mix + pctymle,
                  data = crime_data)
summary(all_in_model) # 81% Adj R^2 (prbarr, prbconv, polpc, density, pctmin80 signif+pctymle low signif) )
plot(all_in_model) # Low ZCM, High Skew, some Hetrosked and few outliers 84, 25 

log_all_in_model = lm(log(1+100*crmrte) ~ prbarr + prbconv + prbpris + avgsen + polpc + density
                  + taxpc + west + central + urban + pctmin80 + wcon
                  + wtuc + wtrd + wfir + wser + wmfg + wfed + wsta + wloc
                  + mix + pctymle,
                  data = crime_data)
summary(log_all_in_model) # 81% Adj R^2 (prbarr, prbconv, polpc, density, pctmin80 signif+pctymle low signif) )
plot(log_all_in_model) # Low ZCM, low Skew, decent Hetrosked and few outliers 84, 25 
#coeftest(log_all_in_model, vcov=vcovHC
#vcov = vcovHC(mixed_model_2, type = "HC1")

mm_step = step(base, scope = formula(all_in_model), direction = "forward")

BIC(all_in_model)
BIC(mixed_model_2)
extractAIC(all_in_model)
extractAIC(mixed_model_2)

summary(mixed_model_2)
summary(all_in_model)


terse_model = lm(crmrte ~ pctmin80 + polpc + density + prbarr + prbconv, data=crime_data)
summary(terse_model)

######################

stargazer(terse_model,log_mixed_model_2,log_all_in_model, type = "text", star.cutoffs = c(0.05,0.01,0.001))

######################

# Predicted R - Squared function

######################


PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}


###################### Testing predicted R s-quared

