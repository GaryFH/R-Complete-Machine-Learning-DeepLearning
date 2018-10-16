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

d1<-tbl_df(Prestige)
d1<-na.omit(d1)
sapply(d1,mean)
sapply(d1,sd)
summary(d1)

#Training
train<-sample_n(d1,.7*nrow(d1))
test<-setdiff(d1,train)

#Model I DON'T LIKE THIS METHOD
reg1<-leaps::regsubsets(prestige~.,train)
par(oma=c(2,0,0,0))
plot(reg1,scale = "bic")

#Stepwise returns the best model (ie lowest AIC) - OK BUT I DON'T LOVE IT
fitbase<-lm(prestige~1,train)
fitall<-lm(prestige~education*income*type*women,train)

fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
              direction = "both",trace=1,steps=1000)

aa<-summary(fitbest)
aa$adj.r.squared


#ANOVA

f1<-lm(prestige~education,train)
f2<-lm(prestige~education +income,train)
f3<-lm(prestige~education + income+ type,train)
f4<-lm(prestige~education + income + type+women,train)

anova(f1,f2,f3,f4) #Note that high pvalue for 4th mod indicates women not needed


#Challenge use fitbest from above and throw out any term with VIF higher than four 


car::vif(fitbest)

