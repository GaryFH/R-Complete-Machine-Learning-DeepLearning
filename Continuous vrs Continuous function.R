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
#library(BBmisc) #contains normalize function - didn't work below?
gh <- function(x, d=2) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#THREE TYPES OF CASES
#Case1 - Continous vrs Continuos
#Case2 - Continous vrs Categorical
#Case3 - Categorical vrs Categorical


#IMPORTANT NOTES FOLLOW:
  #1) If you don't want observations with NA's removed then use MICE or impute to convert NA's
  #2) REMOVE all NA's: d1<-na.omit(d1)
  #3) Insure that all observations are complete cases: d1<-d1[complete.cases(d1),]
  #4) MAKE RESPONCE VARIABLE the first variable:  dplyr::select(d1,responce,everything())
  #5) Make all variables numeric:  

#Example inputs for function below:
  d1<-tbl_df(Prestige)
  d1<-na.omit(d1)
  d1<-d1[complete.cases(d1),]
  d1<-dplyr::mutate(d1,type=ifelse(type=="prof",1,ifelse(type=="bc",2,3)))
  d1<-tbl_df(as.data.frame(sapply(d1,as.numeric)))
  d1<-dplyr::select(d1,income,everything())
  #ErrLimit=.2(for good prediction if within 20%)
  
  
 #  data("BreastCancer")
 #  dd<-BreastCancer[complete.cases(BreastCancer),]
 # dd<-tbl_df(dd)


  
  ContvrsCont<- function(data,ErrLimit) {

    d1<-data
    #Insure no NA's and all observations are "complete.cases"
    d1<-na.omit(d1)
    d1<-d1[complete.cases(d1),]
    
    #Make all variables NUMERIC VARIABLE
    d1<-tbl_df(as.data.frame(sapply(d1,as.numeric)))
    
    d1$id<-as.integer(rownames(d1))
    train<-sample_n(d1,.7*nrow(d1))
    test<-setdiff(d1,train)
    ag<-nrow(d1)==(nrow(train)+nrow(test))
    try(if(ag!="TRUE") warning ("train/test problem"))
    train<-train[-ncol(train)]
    test<-test[-ncol(test)]
    d1<-d1[-ncol(d1)]
    
    predictor<-as.numeric(unlist(train[1]))
    predictortest<-as.numeric(unlist(test[1]))
    # fitall1<-lm(predictor~.,train[-1])
    b1<-as.list(names(train[-1]))
    b2<-paste(b1,collapse = " * ")
    
    fitbase<-lm(predictor~1,train)
    fitall<-lm(data=train,as.formula(paste(names(train)[1],"~",b2)))
    
fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
                direction = "both",trace=1,steps=1000)
    aa1<-summary(fitbest)
    
#make object callb from fitbest to make bestfit formula for glm use
    callb<-as.list(aa1$call[2])
    callb<-paste(callb,collapse = "")
    
    ifelse(n_distinct(predictor)!=2,
fitglm<-glm(data = train, family = gaussian,as.formula(callb)),  
          fitglm<-glm(data = train, family = gaussian,as.formula(callb)))
    
predlm<<-predict(fitbest,newdata = test)
predglm<<-predict(fitglm,newdata = test)
    
    #lm bestfit stats
    testlm<<-test
    testlm<-mutate(testlm,Accuracy=ifelse(predlm<(1+ErrLimit)*predictortest&predlm>(1-ErrLimit)*predictortest,1,0))
    Acc1perc<-mean(na.omit(testlm$Accuracy))*100
    aa<-summary(fitbest)
    MAE<-gh(mae(predictortest,predlm))
    
    # Statslm<<-paste("MeanAbsoluteError=",MAE,"  ACCURACY(within", ErrLimit*100,"%) = ",gh(Acc1perc),
    #       "%  -  AdjR=",gh(aa$adj.r.squared), " Stepwise bestfit variables(from stats package)",sep = "")
    a1<-c('method','MeanAbsolErr','%+-AccRnge','Accuracy','StrengthMethod','StrengthValue')
    a2<-c('lm',MAE,ErrLimit*100,gh(Acc1perc),"AdjRsq", gh(aa$adj.r.squared))
    
    
    
    #glm bestfit stats
    testglm<<-test
    testglm<-mutate(testglm,Accuracy=ifelse(predglm<(1+ErrLimit)*predictortest&predglm>(1-ErrLimit)*predictortest,1,0))
    Acc1perc<-mean(na.omit(testglm$Accuracy))*100
    aa<-summary(fitglm)
    MAE<-gh(mae(predictortest,predglm))
    
    # Statsglm<<-paste("MeanAbsoluteError=",MAE,"  ACCURACY(within", ErrLimit*100,"%) = ",gh(Acc1perc20),
    #                 "%  -  AdjR=",gh(aa$adj.r.squared), " stats::stepwise bestfit variables",sep = "")
    a3<-c('glm',MAE,ErrLimit*100,gh(Acc1perc),"AIC",gh(aa$aic))
    
    tblstat<-tbl_df(as.data.frame(rbind(a2,a3)))
    names(tblstat)<-a1
    tblstat<<-tblstat
    
    byt<-testlm[1]
    names(byt)<-"ActualValue"
    byt$ActualValue<-as.numeric(byt$ActualValue)
    
    ty<-as.data.frame(predlm)
    names(ty)<-"PredictedValue"
    ty$PredictedValue<-as.numeric(ty$PredictedValue)
    
    ty2<-as.data.frame(100*(predlm-testlm[1])/testlm[1])
    names(ty2)<-"PercentError"
    ty2$PercentError<-gh(ty2$PercentError)
    ty2$PercentError<-as.numeric(ty2$PercentError)
    
    tbl1<<-cbind(ty,byt,ty2)
    #saveRDS(tbl1,file = "tbl1.rds")
    
    } 
    
    
    grid.table(tblstat,row=NULL )
    
    tbl<-tbl_df(tbl1)
    tbl$id<-as.integer(rownames(tbl))
    tbl[-ncol(tbl)]<-sapply(tbl[-ncol(tbl)],gh)
    tbl[-ncol(tbl)]<-sapply(tbl[-ncol(tbl)],as.numeric)
    #grid.table(tbl,row=NULL )
    
    tblPred<-tbl[-2:-3]
    names(tblPred)<-c("Value","id")
    tblAct<-tbl[2:4]
    tblAct<-tblAct[-2]
    names(tblAct)<-c("Value","id")
    
    #Actual value vrs predicted value chart
    p1<-ggplot()+
    geom_point(data=tblPred,aes(y=Value,x=id),color="blue",size=3,alpha=.7)+
    
    geom_point(data=tblAct,aes(y=Value,x=id),color="red",size=3,alpha=.7)
    
    p1
    
    #Percent error graph
    p2<-ggplot()+
    geom_point(data=tbl,aes(y=abs(PercentError),x=id),color="green",size=5,alpha=.7)
    p2
    
    
    # #Case2 - Continous vrs Categorical
    # #Use ANOVA
    # #H0 - all populatiion means are equal
    # #H1 - at least one population mean is different
    # #F-Ratio = Mean Between Groups Sum of Squares divided by Mean within Groups Sum of Squares
    # #In Anova "type" Mean Sq is between groups and "Residuals" is within groups
    # #Therefore higher F_Ratio(and low P-val) means reject H0
    # 
    # #ex - boxplot shows means are different
    # boxplot(predictor~type,d1,
    #         main="predictor vs Occupation",
    #         ylab="predictor")
    # 
    # #ex - anova shows high F-Value and low P-val - thus reject HO
    d23<-d1
    d23[-6]<-scale(d23[-6])#not needed
    d23<-tbl_df(as.data.frame(d23))
    d23$type<-as.factor(d23$type) #not needed
    
    fit<-aov(prestige~type,d23)
    anova(fit)
    testaov<-mutate(test,Accuracy=ifelse(pred<(1+ErrLimit)*income&pred>(1-ErrLimit)*income,1,0))
    Acc1perc<-mean(na.omit(testaov$Accuracy))*100
    aa<-anova(fit)
    MAE<-gh(mae(testaov$prestige,pred))
    pVal<-aa$`Pr(>F)`[1]
    MAE
    pVal

