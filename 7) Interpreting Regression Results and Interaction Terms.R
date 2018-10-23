library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(data.table)
library(DMwR)
library(carData)
data("Prestige")

library(mice)
d1<-tbl_df(Prestige)
micemod<-mice(d1)
d1a<-tbl_df(complete(micemod,2))#uses impute method


#Added ID so I could test new way of selecting train & test df's
d1a<-mutate(d1a,ID=rownames(d1a))
d1a$ID<-as.integer(d1a$ID)
d1a<-dplyr::select(d1a,ID,everything())

set.seed(123)
traina<-sample_n(d1a,.75*nrow(d1a))
testa<-setdiff(d1a,traina)

#test if above works
gg<-arrange(rbind(traina,testa),ID)
identical(d1a,gg)

#Remove ID from test and train
test<-testa[-1]
train<-traina[-1]

#play with linear regression
library(broom) #allows for tidy below

fit1<-lm(prestige~income + education,train)
summary(fit1)
tidy(fit1)#tidy from "broom"
pred1<-as.vector(predict(fit1,newdata = test))


#low pValue shows a relationship (rejecting H0) but not the STRENGTH of the relationship
#link to article http://bit.ly/1OXBLbk

#since Rsqr increases with the number of predictors, AdjRsqr (which adjusts for # of predictors)
#is the best way to compare models

#Also used are AIC and BIC - the lower the better the model/predictor

AIC(fit1)
BIC(fit1)

#colinear variables can mean multiple variables could be giving same information (redundant)
#Therefore the Variance Inflation Factor (VIF) - high VIF means strong colinear
#A VARIABLE HAVING A VIF OVER 4 IS TYPICALLY CONSIDERED TO HAVE "Multicollinearity"

car::vif(fit1)

#NOTE that below trick increases strength of model (higher adjRsquared and lower AIC&BIC)
# TRICK use an "interaction term" - such as income:education in the model as follows:

fit2<-lm(prestige~income + education + income:education,train)
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
AIC(fit1)
AIC(fit2)
BIC(fit1)
BIC(fit2)


# TRICK use an "I" function - such as "I(education^2)" in the model as follows:
    #note that education^2 is better than fit1 but worst than fit2

fit3<-lm(prestige~income + I(education^2),train)
summary(fit3)$adj.r.squared


#three methods to predict values - all almost identical (different #digits)

aa<-predict(fit2,train)
bb<-fit2$fitted.values
cc<-fitted(fit2)










AIC(fit3)
