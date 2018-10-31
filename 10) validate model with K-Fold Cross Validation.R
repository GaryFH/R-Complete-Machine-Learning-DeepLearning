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

d1<-tbl_df(Prestige)
d1<-na.omit(d1)
sapply(d1,mean)
sapply(d1,sd)
summary(d1)
d1<-na.omit(d1)


#Training
train<-sample_n(d1,.7*nrow(d1))
test<-setdiff(d1,train)

# #Model I DON'T LIKE THIS METHOD
# reg1<-leaps::regsubsets(prestige~.,train)
# par(oma=c(2,0,0,0))
# plot(reg1,scale = "bic")

#Stepwise returns the best model (ie lowest AIC) - OK BUT I DON'T LOVE IT
fitbase<-lm(prestige~1,train)
fitall<-lm(prestige~education*income*type*women,train)

fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
              direction = "both",trace=1,steps=1000)

aa<-summary(fitbest)
aa$adj.r.squared

#Try with "+" instead of "*" in fitall - note AdjRsq and AIC show significantly worst model
fitbase2<-lm(prestige~1,train)
fitall2<-lm(prestige~education+income+type+women,train)

fitbest2<-step(fitbase2, scope = list(lower=fitbase2,upper=fitall2),
              direction = "both",trace=1,steps=1000)

aa<-summary(fitbest2)
aa$adj.r.squared






#k-fold cross validation train sample on multiple samples saving the last for a test

#CHALLENGE 1 - WHICH IS BEST MODEL - $delta give error (MSE & adjMSE) lower MSE better model
fitglm1<-glm(prestige ~ income + education,data=d1)
boot::cv.glm(d1,fitglm1,K=5)$delta

fitglm2<-glm(prestige ~ education + type + income,data=d1)
boot::cv.glm(d1,fitglm2,K=5)$delta

fitglm3<-glm(prestige~education+type+income +type:income,data=d1)
boot::cv.glm(d1,fitglm3,K=5)$delta

#Use Anova to find if type:income term is needed anova don't work with glm

f1<-lm(prestige ~ income + education,data=d1)
f2<-lm(prestige ~ education + type + income,data=d1)
f3<-lm(prestige~education+type+income +type:income,data=d1)

anova(f1,f2,f3)
 #low P-value for type:income means this term is needed


