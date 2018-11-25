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

#library(BBmisc) #contains normalize function - didn't work below?
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

d3<-tbl_df(iris)
d3<-dplyr::select(d3,Species,everything())
# d3<-mutate(d3,Species=ifelse(Species=="setosa",0,
#                              ifelse(Species=="versicolor",.5,1)))
d3<-d3[complete.cases(d3),]


train<-sample_n(d3,.7*nrow(d3))
test<-setdiff(d3,train)


#CTree stuff
    library(partykit)
    ctFit<-ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = train)
    
    print(ctFit)
    plot(ctFit)
    
    pred1<-predict(ctFit,test)
    mean(test[[1]]==pred1)
    sum(test[[1]]==pred1)

#Rpart stuff
    rpFit<-rpart::rpart(Species~.,data = train,
              control = rpart.control(minsplit = 5,cp=0,maxdepth = 4))
    
    pred2<-predict(rpFit,test,type = "class")
    pred2
    mean(test[[1]]==pred2)
    
    library(rattle)
    library(RColorBrewer)
    fancyRpartPlot(rpFit)
    
#C5.0 stuff
    c5Fit<-C50::C5.0(Species~.,data=train,
                     control=C50::C5.0Control(winnow=F))
    
    summary(c5Fit)
    plot(c5Fit)
    
    #see importance of variables
    C50::C5imp(c5Fit)
    
    pred3<-predict(c5Fit,test)
    mean(test[[1]]==pred3)
    

#Challenge - TH.data package - GlaucomaM data - predict if patient has glaucoma with ctree
    
    d2<-tbl_df(TH.data::GlaucomaM)
    d2<-dplyr::select(d2,Class,everything())
    d2<-na.omit(d2)
    
    
    trainb<-sample_n(d2,.7*nrow(d2))
    testb<-setdiff(d2,trainb)
    
    fit<-ctree(Class~.,trainb)
    pred<-predict(fit,testb)
    mean(testb[[1]]==pred)
    