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
library(mlbench)
library(caret)
library(ROSE)
library(purrr)
library(e1071)
library(klaR)
library(class)
library(ModelMetrics)
#library(BBmisc) #contains normalize function - didn't work below?
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#   KNN NEEDS ALL VARIABLES NUMERIC AND "NORMALIZED" BETWEEN 0 AND 1


#   KNN needs to have a distance matrix
      #ex. use 'iris' data
dd<-tbl_df(iris)


#distance for first 10 rows excluding the response variable "Species" 
dist1<-dist(dd[1:10,c(1:4)]) #this producing an object of "dist" class
#the numbers shown above are the euclidian distances between the observations

##Normalize (convert all variable values to fall between 0 & 1)
#d1<-normalizeb(dd,method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
#above doesn't work


#gg<-lapply(dd[,-5],normalize)
gg<-sapply(dd[-5],normalize)
ddnorm<-tbl_df(data.frame(gg,Species=dd[,5]))


dist2<-dist(ddnorm[1:10,c(1:4)])


#first split to train and test

ddnorm$id<-rownames(ddnorm)
train<-sample_n(ddnorm,.7*nrow(ddnorm))
test<-setdiff(ddnorm,train)
dim(ddnorm)[1]==dim(train)[1]+dim(test)[1]
train<-train[-6]
test<-test[-6]

#KNN use data.frames NOT tbl_df
#NOTE THAT "k" can't be a multiple of the #of regressors - use odd numbers only

trainb<-as.data.frame(train)
testb<-as.data.frame(test)

out<-class::knn(trainb[,-5],testb[,-5],trainb[,5],k=5)
out
testbb<-test
testbb<-mutate(testbb,ACC=ifelse(out==Species,1,0))
accuracy<-mean(na.omit(testbb$ACC)) 
accuracy

fit1<-caret::knn3(trainb[,-5],trainb[,5],k=5)
summary(fit1)
print(fit1)

pred1<-predict(fit1,testb[,-5],type = "class")
testbb<-testb
testbb<-mutate(testbb,ACC=ifelse(pred1==Species,1,0))
accuracy<-mean(na.omit(testbb$ACC)) 
accuracy


# WORK WITH NEW

d2<-tbl_df(Boston) #part of package MASS
d2<-na.omit(d2)
d2norm<-sapply(d2,normalize)
d2norm<-tbl_df(as.data.frame(d2norm))
summary(d2norm)

d2normb<-d2norm
d2normb$id<-rownames(d2norm)
trn<-sample_n(d2normb,.7*nrow(d2norm))
tst<-setdiff(d2normb,trn)

trn<-trn[-15]
tst<-tst[-15]
dim(d2norm)[1]==dim(trn)[1]+dim(tst)[1]

#This works with tbl_df
fit3<-knnreg(trn[-14],trn$medv ,k=5)

print(fit3)

pred2<-predict(fit3,tst[-14])
tstb<-tst
tstb<-mutate(tstb,ACC=ifelse(pred2==medv,1,0))

tstb<-mutate(tstb,Accuracy=ifelse(pred2<1.2*medv&pred2>.8*medv,1,0))
Acc1perc20<-mean(na.omit(tstb$Accuracy))*100
paste("ACCURACY(within 20%) = ",gh(Acc1perc20),"%",sep = "")


DMwR::regr.eval(tst$medv,pred2)

