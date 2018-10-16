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
data("BreastCancer")
dd<-BreastCancer[complete.cases(BreastCancer),]
d1a<-dd

# summary(d1)
# summary(d2)

#delete ID variable
d1b<-d1a[,-1]
d1<-tbl_df(d1b)
#NOTE THIS DOESN'T WORK WITH tbl_df
#Convert ord.factor to numeric (first go to character) - not response variable "Class"

for (i in 1:9)  {
  d1b[,i]<-as.numeric(as.character(d1b[,i]))
  
  
}

d1<-d1b
d2<-tbl_df(d1)

#turn responce variable into 0 & 1
d2<-mutate(d2,Class=ifelse(Class=="malignant",1,0))

table(d2$Class)

#For this to work they want training data or have both class values about equal (currently twice as many 0's")
#To fix - use carat package downsampling
set.seed(111)
trainIndex<-createDataPartition(d2$Class,p=.7,list = F)
train<-d2[trainIndex,]

#now reduce num of 0's to same as num of #1's in the train dataframe (downsampling)
train$Class<-as.factor(train$Class)
trainDown<-downSample(x=dplyr::select(train,-Class),y=train$Class)
summary(trainDown$Class)
trainUp<-upSample(x=dplyr::select(train,-Class),y=train$Class)
summary(trainUp$Class)


# #Training
# d3<-d2
# d3<-mutate(d3,id=row_number())
# trainb<-sample_n(d3,.7*nrow(d3))
# testb<-setdiff(d3,trainb)

#Now build model
fitdown<-glm(Class~Cl.thickness + Cell.size + Cell.shape, family = binomial, data = trainDown)
summary(fitdown)
predDown<-predict(fitdown, newdata= test, type = "response")


#set level for malignant vrs benign at .5

prednum1<- ifelse(predDown>0.5,1,0)
y_pred<-as.factor(prednum1)

Accuracy<-mean(y_pred==as.factor(test$Class))
Accuracy

caret::confusionMatrix(y_pred,as.factor(test$Class),positive="1")

