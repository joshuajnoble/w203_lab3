# Note to self: 
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


library(tidyverse)
library(effsize)
library(plyr)
library(reshape2)
library(ggiraphExtra)

setwd("C:\\Users\\winbase\\MIDS\\w203\\w203_lab3")
crime_data = read.csv("crime_v2.csv")

#clean data, dropping all NA rows
crime_data = crime_data[which(crime_data$county != 'NA'),]

# what is wrong with prbconv?
class(crime_data$prbconv[1][1]) #r thinks it's a factor?

# let's clean these up
crime_data$prbconv = as.numeric(levels(crime_data$prbconv)[as.integer(crime_data$prbconv)])

###########################################################################################
# removing outliers?
# probably not necessary
###########################################################################################

#very initial looking for correlations
cor(crime_data$crmrte, crime_data$taxpc) #0.45, pretty strongly correlated
cor(crime_data$crmrte, crime_data$wmfg) #0.35, also pretty strongly correlated
cor(crime_data$crmrte, crime_data$prbarr) #-0.39, pretty neg strong


###########################################################################################
# removing outliers?
# probably not necessary
###########################################################################################
remove_outliers_sd = function(d, field, numDevs) {
  s = numDevs * sd(d[[field]])
  m = mean(d[[field]])
  return( d[ which( d[[field]] > (m-s) & d[[field]] < (m+s)),])
}

#predict crmrte with tax income
outliers_removed_tax = remove_outliers_sd(crime_data, 'taxpc', 2.0)
crime_tax = lm(outliers_removed_tax$crmrte ~ outliers_removed_tax$taxpc)
plot( jitter(outliers_removed_tax$crmrte), jitter(outliers_removed_tax$taxpc), ylab = "Crime Rate", xlab = "Tax receipts")
abline(crime_tax)
#summary(crime_tax)

#predict crmrte with conviction rate
outliers_removed_convictions = remove_outliers_sd(crime_data, 'prbconv', 2.0)
crime_conviction = lm(outliers_removed_convictions$crmrte ~ outliers_removed_convictions$prbconv)
plot(jitter(outliers_removed_convictions$prbconv), jitter(outliers_removed_convictions$crmrte), xlab = "Crime Rate", ylab = "Probability of conviction")
abline(crime_conviction) #doesn/t seem to show much

#predict crmrte with police presence
outliers_removed_prbarr = remove_outliers_sd(crime_data, 'prbarr', 2.0)
crime_arrest = lm(outliers_removed_prbarr$crmrte ~ outliers_removed_prbarr$prbarr)
plot(jitter(outliers_removed_prbarr$prbarr), jitter(outliers_removed_prbarr$crmrte), ylab = "Crime Rate", xlab = "Probability of Arrest")
abline(crime_arrest)

#predict crmrte with prison sentence
outliers_removed_prison = remove_outliers_sd(crime_data, 'prbpris', 2.0)
crime_prison = lm(outliers_removed_prison$crmrte ~ outliers_removed_prison$prbpris)
plot(jitter(outliers_removed_prison$prbpris), jitter(outliers_removed_prison$crmrte), ylab = "Crime Rate", xlab = "Probability of Prison")
abline(crime_prison)

#maybe remove the outliers for polpc?
crime_police = lm(outliers_removed_polpc$crmrte ~ outliers_removed_polpc$polpc, data = outliers_removed_polpc)
plot(jitter(outliers_removed_polpc$polpc), jitter(outliers_removed_polpc$crmrte), ylab = "Crime Rate", xlab = "Police Presence")
abline(crime_police)

#
crime_ym = lm(crime_data$crmrte ~ crime_data$pctymle, data = crime_data)
plot(jitter(crime_data$pctymle), jitter(crime_data$crmrte), ylab = "Crime Rate", xlab = "Young Men")
abline(crime_ym)

#
crime_dens = lm(crime_data$crmrte ~ crime_data$density, data = crime_data)
plot(jitter(crime_data$density), jitter(crime_data$crmrte), ylab = "Crime Rate", xlab = "Pop Dens")
abline(crime_dens)


###########################################################################################
#
# building models
# 
###########################################################################################

arr_and_conv = lm(crime_data$crmrte ~ crime_data$prbconv + crime_data$prbarr, data = crime_data)
ggplot(crime_data,aes(y=prbarr,x=prbconv,color=crmrte)) + 
  geom_point(size=2) +
  stat_smooth(method="lm",se=FALSE) +
  scale_color_gradient(low = "green", high = "red", space = "Lab" )

#looking at all the crime does pretty well
arr_conv_prbpris = lm(crime_data$crmrte ~ crime_data$prbconv + crime_data$prbarr + crime_data$prbpris, data = crime_data)
summary(arr_conv_prbpris) #adjusted rsquared is 0.3, not great
plot(arr_conv_prbpris)

#but also looking at the population stats of the county does well too
dens_ym_taxpc = lm(crime_data$crmrte ~ crime_data$pctymle + crime_data$density + crime_data$taxpc + crime_data$pctmin80, data = crime_data)
summary(dens_ym_taxpc) #adjusted rsquared is 0.68, better
plot(dens_ym_taxpc)

#looking at wages doesn't do so well
wage_model = lm(crime_data$crmrte ~ crime_data$wcon + crime_data$west + crime_data$wfed + crime_data$wfir + crime_data$wloc + crime_data$wmfg + crime_data$wser + crime_data$wsta + crime_data$wtrd + crime_data$wtuc, data = crime_data)
summary(wage_model)
plot(wage_model)

#mixing them seems to do best
mixed_model = lm(crime_data$crmrte ~ crime_data$pctymle + crime_data$density + crime_data$taxpc + crime_data$pctmin80 + crime_data$prbarr + crime_data$prbconv, data = crime_data)
summary(mixed_model) #rsqyared of 0.7494
plot(mixed_model)

# compute standardized residuals
mm_standard = rstandard(mixed_model)
id = order(mm_standard)
mm_standard[id[1]] #biggest overestimate is -2.23 SDs over regression line

#this graph shows the two big outliers
std_resid = rstandard(mixed_model) 
cooks_D = cooks.distance(mixed_model) 
hat_values = hatvalues(mixed_model) 
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2,	2),	lty=2)


#mixing them with outliers removed seems to do realy well
mixed_model_2 = lm(crmrte ~ pctymle + density + taxpc + pctmin80 + prbarr + prbconv, data = crime_data[-c(25, 51),])
summary(mixed_model_2) #rsqyared of 0.8084
plot(mixed_model_2)

std_resid = rstandard(mixed_model_2) 
cooks_D = cooks.distance(mixed_model_2) 
hat_values = hatvalues(mixed_model_2) 
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2,	2),	lty=2)

mm2_standard = rstandard(mixed_model_2)
id = order(mm2_standard)
mm2_standard[id[1]] #biggest overestimate is -2.29 SDs over regression line

