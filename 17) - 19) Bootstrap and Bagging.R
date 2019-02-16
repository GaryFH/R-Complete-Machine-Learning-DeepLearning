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
library(randomForest)
library(Boruta)
library(tidyr)
library(doMC)
library(remotes) #install_github("cran/doMC")

#library(BBmisc) #contains normalize function - didn't work below?
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#Bootstrap is random sample - same size as original dataset - since it sampling
# with replacement, some data points can be repeated and some data point might not be picked
#  data not picked is called "out of bag samples"


library(caret)
df1<-tbl_df(segmentationData)
df2<-tidyr::drop_na(df1)
df3<-df2[complete.cases(df2),]

d1a<-df3
train<-sample_n(d1a[-c(1:2)],.7*nrow(d1a))
test<-setdiff(d1a[-c(1:2)],train)

#setup trainControl
ctrl<-trainControl(method = "repeatedcv",  #10 fold cross validation
                   repeats = 5,
                   summaryFunction = multiClassSummary,
                   classProbs = TRUE)

#make sure both the sqrt(ncol) and (ncol)/3 are including within the grid

grid<-expand.grid(mtry=c(2,8,15,20,30))


#doMC::registerDoMC(cores=4)

    system.time({
      parRFmod<-train(Class~., data=train,
                      method="parRF",
                      preProc=c("center","scale"),
                      #metric="ROC",
                      metric="logLoss",
                      tuneGrid=grid,
                      trControl=ctrl)
      
    })
    
    parRFmod
    
    pred<-predict(parRFmod,test)
    aa<-ifelse(pred==test$Class,1,0)
    Accuracy<-mean(aa)*100
    print(paste("The Accuracy of this method is: ",gh(Accuracy),"%",sep=""))
    
    
#Compare to standard lm method    
    train1<-mutate(train,Class=ifelse(Class=="PS",0,1) )
    test1<-mutate(test,Class=ifelse(Class=="PS",0,1) )
    fit<-lm(Class~., data = train1)
    pred<-predict(fit,newdata = test1)
    pred1<-pred
    pred1=ifelse(abs(pred1)<.5,0,1)
    aa<-ifelse(pred1==test1$Class,1,0)
    Accuracy<-mean(aa)*100
    print(paste("The Accuracy of lm method is: ",gh(Accuracy),"%",sep=""))
    


#Bagging is short for bootstrap aggregation (multiple bootstrap runs)