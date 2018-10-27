library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(data.table)
library(DMwR)
library(carData)
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
library(gridExtra)
library(ggplot2)

gh <- function(x, d=2) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
      
      
data("BreastCancer")
      dd<-BreastCancer[complete.cases(BreastCancer),]
      d1a<-dd
      
      #delete ID variable
      d1b<-d1a[,-1]
      d1<-tbl_df(d1b)
      #Convert ord.factor to numeric (first go to character) - not response variable "Class"
      d1[1:5]<-sapply(d1[1:5],as.character)
      d1<-dplyr::mutate(d1,Class=ifelse(Class=="benign",0,1))
      d1<-tbl_df(as.data.frame(sapply(d1,as.numeric)))
d1<-dplyr::select(d1,Class,everything())
      
      d1a<-d1
      d1a$id<-rownames(d1a)
      train<-sample_n(d1a,.7*nrow(d1))
      test<-setdiff(d1a,train)
      try(if(nrow(d1a)!=nrow(train)+nrow(test)) warning("test/train dim issues"))
      train<-train[-ncol(train)]
      test<-test[-ncol(test)]
      
      
      trainDown<-tbl_df(downSample(x=dplyr::select(train,-Class),y=as.factor(train$Class)))
      summary(trainDown$Class)
      
      fitdown1<-glm(Class~Cl.thickness + Cell.size + Cell.shape, family = binomial, data = trainDown)
      #summary(fitdown)
      predDown1<-predict(fitdown1, newdata= test, type = "response")
      
      #set level for malignant vrs benign at .5
      prednum1<- ifelse(predDown1>0.5,1,0)
      
      trainDown2<-trainDown
      trainDown2$Class<-as.numeric(as.character(trainDown2$Class))
      summary(trainDown2$Class)
      
      
      fitbase<-lm(Class~1,trainDown2)
      fitall<-lm(Class~ 
                   Cl.thickness*Cell.size*Cell.shape*Marg.adhesion*Epith.c.size*Bare.nuclei*
                   Bl.cromatin*Normal.nucleoli*Mitoses ,trainDown2)
      
      fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
                    direction = "both",trace=1,steps=1000)
      predbest<-predict(fitbest,newdata = test)
      prednumbest<- ifelse(predbest>0.5,1,0)
      
      fitdown2<-lm(Class~Cl.thickness + Cell.size + Cell.shape, data = trainDown2)
      #summary(fitdown)
      predDown2<-predict(fitdown2, newdata= test)
      
      #set level for malignant vrs benign at .5
      prednum2<- ifelse(predDown2>0.5,1,0)
      
      #THE TEACHER USED THIS METHOD?
      # y_pred<-as.factor(prednum1)
      # 
      # Accuracy1<-mean(y_pred==as.factor(test$Class))
      
      #I like this method - it is simpler and
      Accuracy1<-mean(ifelse(prednum1==test$Class,1,0))
      Accuracy1
      a1<-summary(fitdown1)
      a1$aic #lower aic suggest better model
      
      Accuracy2<-mean(ifelse(prednum2==test$Class,1,0))
      Accuracy2
      a2<-summary(fitdown2)
      a2$adj.r.squared
      
      Accuracybest<-mean(ifelse(prednumbest==test$Class,1,0))
      Accuracybest
      aBest<-summary(fitbest)
      aBest$adj.r.squared
