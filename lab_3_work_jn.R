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
library(ggcorrplot)

setwd("C:\\Users\\winbase\\MIDS\\w203\\w203_lab3")
crime_data = read.csv("crime_v2.csv")

crime_panel_data = read.csv("crime4_corrected.csv")


#clean data, dropping all NA rows
crime_data = crime_data[which(crime_data$county != 'NA'),]
# what is wrong with prbconv?
class(crime_data$prbconv[1][1]) #r thinks it's a factor?

# let's clean these up
crime_data$prbconv = as.numeric(levels(crime_data$prbconv)[as.integer(crime_data$prbconv)])


crime_data = distinct(crime_data)


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

#really basic model
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
std_resid = rstandard(mixed_model)  #map all of the residuals
cooks_D = cooks.distance(mixed_model) #get the cooks distance
hat_values = hatvalues(mixed_model) #calculate all the hat values
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2,	2),	lty=2)

mmframe = data.frame(resid	= residuals(mixed_model), pred = predict(mixed_model)) 
ggplot(mmframe, aes(pred, abs(resid))) + geom_point() + geom_smooth()


mixed_model_2 = lm(crmrte ~ pctymle + density + pctmin80 + taxpc + prbarr + prbconv + polpc, data = crime_data)

# lets find out about our model
summary(mixed_model_2)
# this gives us 4 charts
plot(mixed_model_2, 1)
plot(mixed_model_2, 3)
plot(mixed_model_2, 5)

hist(mixed_model_2$residuals, breaks=20)

# extract the Aikike information criterion for each model
extractAIC(mixed_model_2)

# extract the Bayes information criterion for each model
BIC(mixed_model_2)

# examine the effect of each element
base = lm(crmrte ~ 1, data = crime_data)
mm_step = step(base, scope = formula(mixed_model_2), direction = "forward")

mm2 = data.frame( resid(mixed_model_2), rstandard(mixed_model_2), rstudent(mixed_model_2), hatvalues(mixed_model_2), cooks.distance(mixed_model_2))

library(lmtest)
library(sandwich)
library(stargazer)

vcov = vcovHC(mixed_model_2, type = "HC1")
errors = coeftest(mixed_model_2, vcov)
stargazer(mixed_model_2, errors)

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

all_in_model = lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density
                   + taxpc + west + central + urban + pctmin80 + wcon
                   + wtuc + wtrd + wfir + wser + wmfg + wfed + wsta + wloc
                   + mix + pctymle,
                   data = crime_data)

mm_step = step(base, scope = formula(all_in_model), direction = "forward")

BIC(all_in_model)
BIC(mixed_model_2)
extractAIC(all_in_model)
extractAIC(mixed_model_2)

summary(mixed_model_2)
summary(all_in_model)

######################

terse_model = lm(crmrte ~ polpc + density + pctmin80 + prbconv + prbarr, data=crime_data)
summary(terse_model)

######################

library(lmtest)

mixed_model_3 = lm(crmrte ~ pctymle + density + pctmin80 + prbarr + prbconv + polpc, data = crime_data)
summary(mixed_model_3)

plot(mixed_model_3, 1)
plot(mixed_model_3, 3)
plot(mixed_model_3, 5)

linearHypothesis(mixed_model_3, "wcon = 0", vcov = vcovHC)


######################
# correlation work
######################

correlation.matrix = cor(crime_data %>%
                           select(-one_of(c("county_loc","west","central","urban","county","year"))))
cp = ggcorrplot(correlation.matrix, colors = c("#0000ff", "white", "#ff0000"),
           tl.cex = 8, outline.color = "white", legend.title = "Correlation") +
  theme(legend.position="top", legend.direction = "horizontal", plot.margin = margin(0,0,0,0,"pt"),
        legend.text=element_text(size=8), legend.title=element_text(size=8))
cp

mat_no_wage = cor(crime_data %>%
                           select(-one_of(c("county","west","central","urban","county","year", "wcon", "wloc", "wfed", "wmfg", "wser", "wfir", "wtrd", "wsta", "wtuc", "wcon"))))
cp_no_wage = ggcorrplot(mat_no_wage, colors = c("#0000ff", "#ffffff", "#ff0000"),
                tl.cex = 8, outline.color = "white", legend.title = "Correlation") +
  theme(legend.position="top", legend.direction = "horizontal", plot.margin = margin(0,0,0,0,"pt"),
        legend.text=element_text(size=8), legend.title=element_text(size=8))

#cp_no_wage + scale_fill_gradient(limit = c(-1,1), low = "blue", high =  "red", mid = "green", midpoint = 1.0)
cp_no_wage