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
    
    d2<-tbl_df(TH.data::GlaucomaM)
    d2<-dplyr::select(d2,Class,everything())
    d1a<-na.omit(d2)
  
    
      
    
Borutatest<- function(data) {
      
  d1<-data  
  
  train<-as.data.frame(sample_n(d1,.7*nrow(d1)))
  test<-as.data.frame(setdiff(d1,train))

#Baruta
    library(Boruta)
    
    baFit<-Boruta(Class~.,train,doTrace=2)
    # baFit
    #plot(baFit,cex.axis=.7,las=2)
    baSignif<-getSelectedAttributes(baFit,withTentative = TRUE)
    # print(baSignif)
    
    roughFixFit<-TentativeRoughFix(baFit)
    # plot(roughFixFit,cex.axis=.7,las=2)
    baSignifB<-getSelectedAttributes(roughFixFit,withTentative = TRUE)
    # print(baSignifB)
    
    
    # gg<-attStats(baFit)
    # gg$Names<-rownames(gg)
    # gg<-tbl_df(gg)
    # gg<-dplyr::select(gg,Names,decision,everything())
    # summary(gg$decision)
    
    gf<-attStats(roughFixFit)
    gf$Names<-rownames(gf)
    gf<-tbl_df(gf)
    gf<-dplyr::select(gf,Names,decision,everything())
    # summary(gf$decision)
    
    gfb<-filter(gf,decision=="Confirmed")
    # ggb<-filter(gg,decision=="Confirmed")
    gfbb<-gfb[order(-gfb$meanImp),]
    gfbb<-gfbb[1:3]
    # ggbb<-dplyr::select(arrange(ggb,desc(meanImp)),Names:meanImp)
    # ggbb
    # importScores<-gf[order(-gf$meanImp),]
    # head(importScores,10)
    
    tf<-unique(gfbb$Names)
    tfb<-c("Class",tf)
    tff<-dput(tfb)
    d3<-subset(d2,select=tff)
    # dim(d3)
    # names(d3)
 
    
    #Make Train and test sets
    trainb<-as.data.frame(sample_n(d3,.7*nrow(d3)))
    testb<-as.data.frame(setdiff(d3,trainb))

    
    # Fitting Random Forest Classification to the Training set
    # install.packages('randomForest')
    library(randomForest)
    fitRFall<-randomForest(x=train[,-1],
                        y=train[,1],
                        ntree=200)
    
    #Create RandomForest stats & predicting accuracy%
    predRFall<-predict(fitRFall,newdata=test[,-1])
    predRFall<-as.factor(as.character(predRFall))
    AccRFall<-ifelse(as.factor(as.character(predRFall))==test[[1]],1,0)
    # paste("Accuracy percentage = ",gh(mean(AccRFall)*100),"%",sep="")
    MAE<-gh(mae(test[[1]],predRFall)) #don't think this means much
    ErrRateAll<-tbl_df(as.data.frame(fitRFall$err.rate))
    ErrRateAll$id<-rownames(ErrRateAll)
    ErrRateAll<<-ErrRateAll
    FinalErrRateAllB<-gh(tail(ErrRateAll$OOB,1))
    
    #Create object from RandomForest for below grid.table    
    a2<-c('RandomForest All',MAE,gh(mean(AccRFall)*100),"final err.rate OOB",FinalErrRateAllB)
    # imp<-FeatureImp$new(fitRF,loss="mae")
  

    fitRFbest<-randomForest(x=trainb[,-1],
                        y=trainb[,1],
                        ntree=200)
    
    #Create RandomForest stats & predicting accuracy%
    predRFbest<-predict(fitRFbest,newdata=testb[,-1])
    predRFbest<-as.factor(as.character(predRFbest))
    AccRFbest<-ifelse(as.factor(as.character(predRFbest))==testb[[1]],1,0)
    # paste("Accuracy percentage = ",gh(mean(AccRFbest)*100),"%",sep="")
    MAE<-gh(mae(testb[[1]],predRFbest)) #don't think this means much
    ErrRatebest<-tbl_df(as.data.frame(fitRFbest$err.rate))
    ErrRatebest$id<-rownames(ErrRatebest)
    ErrRatebest<<-ErrRatebest
    FinalErrRatebestB<-gh(tail(ErrRatebest$OOB,1))
    
    #Create object from RandomForest for below grid.table 
    a1<-c('method','MeanAbsolErr','Accuracy','StrengthMethod','StrengthValue')
    a3<-c('RandomForest Best',MAE,gh(mean(AccRFbest)*100),"final err.rate OOB",FinalErrRatebestB)
 
    gfh<-tbl_df(as.data.frame(rbind(a1,a2,a3)))
    grid.table(gfh,rows=NULL,col=NULL)

    
    
    tr<-as.data.frame(rbind(a2,a3))
    tr[2:3]<-sapply(tr[2:3],as.character)
    tr[2:3]<-sapply(tr[2:3],as.numeric)
    tblstat<-tbl_df(tr)
    
    names(tblstat)<-a1
    tblstat<<-tblstat
    grid.table(tblstat,row=NULL )
    
    print('')
    print('')
    print('')
    
    print("All variables")
    print(names(d1[-1]))
    
    print('')
    print('')
    print('')
    
    print("Important variables")
    print(names(d3[-1]))
    
    print('')
    print('')
    print('')
    
    
    print("Not Important variables")
    print(setdiff(names(d1),names(d3)))

    
    
}  




#get average accuracy for 10 runs of various predictive methods
All<-c()
Best<-c()
RFalla<-c(0,0)
RFbesta<-c(0,0)
for(i in 1:10) {
  Borutatest(d1a)
  RFallavg<-c(tblstat[[1,2]],tblstat[[1,3]])
  RFalla<<-(RFallavg+RFalla)
  Alla<-tblstat[[1,3]]
  All<<-c(All,Alla)
  RFbestavg<<-c(tblstat[[2,2]],tblstat[[2,3]])
  RFbesta<<-(RFbestavg+RFbesta)
  Besta<-tblstat[[2,3]]
  Best<<-c(Best,Besta)
}


buall<-c(RFalla,sd(All))
bubest<-c(RFbesta,sd(Best))
bu<-tbl_df(as.data.frame(rbind(buall,bubest)))

names(bu)<-c("Avg_MAE","Avg_Accuracy","SD_of averages")

gf<-tbl_df(cbind(tblstat[1],bu))
gf2<-mutate(gf,Avg_MAE = Avg_MAE/10,Avg_Accuracy=Avg_Accuracy/10)

grid.table(gf2,row=NULL )



