library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(data.table)
library(DMwR)
library(carData)
data("Prestige")

Prestige_miss<-read.csv('https://raw.githubusercontent.com/selva86/datasets/master/Prestige_miss.csv')
d1<-tbl_df(Prestige_miss)
summary(d1)

#Replace missing values
#FOLLOWING USES IMPUTE METHOD
# d3<-d1
# d3[-6]<-sapply(d1[-6],function(x){impute(x,mean)})
# d4<-tbl_df(as.data.frame(d3))
# summary(d4)
# 
# d4$type<-impute(d4$type,mode)
# summary(d4)

#fOLLOWING USES mice METHOD
library(mice)

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

#Could change prestige values to one of 10 factor levels using:
#quantile(test1$prestige,probs=seq(0,1,.1))

fit1<-lm(prestige~income + education,train)
summary(fit1)
tidy(fit1)#tidy from "broom"
pred1<-as.vector(predict(fit1,newdata = test))

#look at accuracy as follows
test1<-test

  #add predict variable
test1$pred1=pred1

  #add accuracy variable (if within 6% prediction considered good or "1")
for (i in 1:nrow(test1)) {

test1$accurate[i]=ifelse((test1$prestige[i]-test1$pred1)<=.06*test1$prestige[i],1,0)

}
#accuracytest1<-(sum(test1$accurate)/nrow(test1))*100
accuracytest1<-mean(test1$accurate)*100
accuracytest1

  #another way to determine accuracy with Mean Squared Error (MSE)
  #and Mean Absolute Percentage Error

MSE<-mean((test$prestige - pred1)^2) # or mse(test$prestige,pred1)
MAPE<-mean(abs(test$prestige - pred1)/test$prestige) # or
MSE
MAPE

#Alternative way of running regression (regr.eval) with DMwR library

gt<-regr.eval(test$prestige,pred1)
gt[[2]]
gt[[4]]
regr.eval(train$prestige,pred1)


