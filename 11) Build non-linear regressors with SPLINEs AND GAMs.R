library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(data.table)
library(DMwR)
library(carData)
data("Prestige")
library(leaps)
library(car)
library(boot)
library(splines)
library(mgcv)

d1<-tbl_df(Prestige)
d1<-na.omit(d1)
sapply(d1,mean)
sapply(d1,sd)
summary(d1)

#Training
train<-sample_n(d1,.7*nrow(d1))
test<-setdiff(d1,train)

#Use smooth.spline cv=T to determine optimum degrees of freedom for regressor income
spbest<-smooth.spline(x=train$income,y=train$prestige,cv=T)
spbest$df  #shows 4 is best number of degrees of freedom
#now use best spbest to predict
pred1<-predict(spbest,test$income)$y

#determines accuracy
DMwR::regr.eval(test$prestige,pred1)
#mape=.2526 - note lower is better

#spline's and GAM's

head(splines::ns(d1$income,df=3))
fitgam1<-mgcv::gam(prestige~ns(income,3)+ns(education,4)+type,data = train)
fitgam1
pred2<-predict(fitgam1,test)
DMwR::regr.eval(test$prestige,pred2)
#mape=.1302   note lower is better and this is lower than spbest


#Challenge - https://goo.gl/Acblpx


# Purpose: Prepare artificial cars dataset and plot

# Prepare data

cars1 <- cars[1:30, ]  # original data

cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.

cars2 <- rbind(cars1, cars_outliers)  # data with outliers.

dcar<-tbl_df(cars2)



# plot

plot(cars2$speed, cars2$dist, pch="*", col="red", main="Dist Vs. Speed", xlab="Speed", ylab="Dist")

abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)



# Fit linear model and plot

lmmod <- lm(dist ~ speed, data=cars2)  # fit model

predicted_lm <- predict(lmmod, cars2)  # predict

DMwR::regr.eval(cars2$dist, predicted_lm)  # Errors

#       mae        mse       rmse       mape 

#  34.58156 1815.21612   42.60535    1.83111 



# Challenge:

# 1. Fit a GAM model to predict 'dist

# 2. Compare the errors from the lm model and gam model. 

#STEP1 use splines to determine optimum number of df

spbest2<-smooth.spline(x=cars2$speed,y=cars2$dist,cv=T)
spbest2$df  #shows six(6) is best number of degrees of freedom


fitgam2<-mgcv::gam(dist~ns(speed,6),data = cars2)
fitgam2
pred2<-predict(fitgam2,cars2)
DMwR::regr.eval(cars2$dist,pred2)

#NOTE mape went down (better) from 1.83 to .456




