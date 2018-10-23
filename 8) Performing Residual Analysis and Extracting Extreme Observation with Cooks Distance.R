library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(data.table)
library(DMwR)
library(carData)
data("Prestige")

d1<-tbl_df(Prestige)
sapply(d1,mean)
sapply(d1,sd)
summary(d1)

#Training
    train<-sample_n(d1,.7*nrow(d1))
    test<-setdiff(d1,train)

#Model
    fit1<-lm(prestige~income+education,train)
    aa<-summary(fit1)
    aa$adj.r.squared
    
    par(mfrow=c(2,2))
    plot(fit1)
    
#test for "Residuals vs Fitted" average values should equal zero
    lmtest::bptest(fit1)
  #Note that the p-value is high thus average residuals vrs fitted 
    #are not 0 and thus the model may behave unpredictably.
    
#test for "Normal Q-Q" Points on graph should all fall on diagonal line
    
#test for "Residuals vrs Leverage" Use "cooks distance"
    #points more than 4 times the mean are bad (too influential or extreme)
    
    
    #Following code makes a new variable "outlier" based on 
    #if an observation's cook number is less than 4 (1 is outlier) 
    ag<-cooks.distance(fit1)
    trainOutlier<-data.frame()
    for(i in 1:length(ag)) {
      trainOutlier<-mutate(train, outlier=ifelse(ag[[i]]>4,1,0))
      }
    
    
    aa<-summary(fit1)
    aa$adj.r.squared
    car::residualPlots(fit1)
    
#note that the last plot above shows significant curve under income
    #therefore rebuild model using log of income
    
    fit2<-lm(prestige~log(income)+education,train)
    aa<-summary(fit2)
    aa$adj.r.squared #note that AdjRsqrd has increased with fit2 - better model fit
    AIC(fit2) #NOTE that AIC is lower for fit2 than for fit1 - better model fit
    car::residualPlots(fit2)
    
#try again with log (default is natural log) of both variables - note that this is worse than fit2
    
    fit3<-lm(prestige~log(income)+log(education),train)
    # cooks.distance(fit3)
    # car::influenceIndexPlot(fit3,n=5)
    aa<-summary(fit3)
    aa$adj.r.squared #note that AdjRsqrd has decreased - worse model fit

    
    
    