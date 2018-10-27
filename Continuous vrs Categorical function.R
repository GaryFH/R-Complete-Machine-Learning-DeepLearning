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
  #6) Categorical dependent variable is limited to 7 levels

#Example inputs for function below:
  data("BreastCancer")
  d1<-tbl_df(BreastCancer)
  d1<-na.omit(d1)
  d1<-d1[complete.cases(d1),]
  d1<-d1[-1] #Remove not needed id variable
  d1<-mutate(d1,Class=ifelse(Class=="benign",0,1))
  #Change "ord" class variables to character in preporation for changing to numeric
  d1[1:5]<-as.data.frame(sapply(d1[1:5],as.character))
  #Change all variables to numeric
  d1<-sapply(d1,as.numeric)
  d1<-tbl_df(as.data.frame(d1))
  #put dependent variable first
  d1<-dplyr::select(d1,Class,everything())
  d2<-tbl_df(scale(d1))

    
    
      
ContvrsCateg<- function(data) {
    
#Insure no NA's and all observations are "complete.cases"
    d1<-na.omit(d1)
    d1<-d1[complete.cases(d1),]
    
#Make all variables NUMERIC VARIABLE
    d1<-tbl_df(as.data.frame(sapply(d1,as.numeric)))
    d1$id<-as.integer(rownames(d1))
    
#Create and test TRAIN & TEST sets
    train<-sample_n(d1,.7*nrow(d1))
    test<-setdiff(d1,train)
    ag<-nrow(d1)==(nrow(train)+nrow(test))
    try(if(ag!="TRUE") warning ("train/test problem"))
    train<-train[-ncol(train)]
    test<-test[-ncol(test)]
    d1<-d1[-ncol(d1)]
    
#dependenttrain is identical to train$"dependent variable" - needed inside custom function
    dependenttrain<-as.numeric(unlist(train[1]))

#dependenttest is identical to test$"dependent variable" - needed inside custom function
    dependenttest<-as.numeric(unlist(test[1]))

    
#make list of all predictor names and make usable in fitall below
    b1<-as.list(names(train[-1]))
    b2<-paste(b1,collapse = " * ")
    
#Create fitbest model with stats::Stepwise Algorithm
    fitbase<-lm(dependenttrain~1,train)
    fitall<-lm(data=train,as.formula(paste(names(train)[1],"~",b2)))
    
#1) LM - model using fitbest model
    fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
                  direction = "both",trace=1,steps=1000)
    aa1<-summary(fitbest)
    
#make object callb from fitbest to make predictor formula for glm
      callb<-as.list(aa1$call[2])
      callb<-paste(callb,collapse = "")
      
#2) GLM - model using fitbest predictors 
    ifelse(n_distinct(dependenttrain)!=2,
        fitglm<-glm(data = train, family = gaussian,as.formula(callb)),  
            fitglm<-glm(data = train, family = binomial,as.formula(callb)))
    
#3) Logistic Regression - model using fitbest predictors
    trainLog<-as.data.frame(train)
    trainLog[,1]<-as.factor(trainLog[[1]])
    #Note trainDown moves dependent variable to last.
    #Also note that "downSample doesn't like tbl_df
    trainDown<-downSample(x=trainLog[,-1],y=trainLog[,1])
    trainDown[,ncol(trainDown)]<-as.character(trainDown[[ncol(trainDown)]])
    trainDown[,ncol(trainDown)]<-as.numeric(trainDown[[ncol(trainDown)]])
    trainDown<-tbl_df(trainDown)
    #Make vector with perferred order of variables
    a9<-c(ncol(trainDown),c((1:(ncol(trainDown)-1))))
    #Put indepent variable back at beginning to be compatable with rest of fuction
    trainDown<-trainDown[,a9]
    
    #dependenttrain is identical to train$"dependent variable" - needed inside custom function
    #dependenttestDown<-as.numeric(unlist(testDown[1]))
    dependenttrainDown<-as.numeric(unlist(trainDown[1]))

# 
#     #Create fitbest model with stats::Stepwise Algorithm
# 
    fitbase2<-lm(dependenttrainDown~1,trainDown)
    fitall2<-lm(data=trainDown,as.formula(paste(names(trainDown)[1],"~",b2)))

    #1) LM - model using fitbest model
    fitbest2<-step(fitbase2, scope = list(lower=fitbase2,upper=fitall2),
                  direction = "both",trace=1,steps=1000)
    aa1<-summary(fitbest2)

    #make object callc from fitbest2 to make predictor formula for Logistic
    callc<-as.list(aa1$call[2])
    callc<-paste(callc,collapse = "")
    
    ifelse(n_distinct(dependenttrainDown)!=2,
           fitdown<-glm(data = trainDown, family = gaussian,as.formula(callc)),  
           fitdown<-glm(data = trainDown, family = binomial,as.formula(callc)))
    

    #Random Forest Method      
    # Feature Scaling
    trainb<-as.data.frame(train)
    trainb[,1]<-as.factor(trainb[,1])
    trainb[,-1]<-scale(trainb[,-1])
    testb<-as.data.frame(test)
    testb[,1]<-as.factor(testb[,1])
    testb[,-1]<-scale(testb[,-1])
    
    
# Fitting Random Forest Classification to the Training set
    # install.packages('randomForest')
    library(randomForest)
    fitRF<-randomForest(x=trainb[,-1],
                        y=trainb[,1],
                        ntree=500)

#Create RandomForest stats & predicting accuracy%
    predRF<-predict(fitRF,newdata=testb[-1])
    AccRF<-ifelse(predRF==testb[,1],1,0)
    paste("Accuracy percentage = ",gh(mean(AccRF)*100),"%",sep="")
    MAE<-gh(mae(dependenttest,predRF)) #don't think this means much
    ErrRate<-tbl_df(as.data.frame(fitRF$err.rate))
    ErrRate$id<-rownames(ErrRate)
    ErrRate<<-ErrRate
    FinalErrRateOOB<-gh(tail(ErrRate$OOB,1))
    
#Create object from RandomForest for below grid.table    
    a5<-c('RandomForest',MAE,gh(mean(AccRF)*100),"final err.rate OOB",FinalErrRateOOB)
    # imp<-FeatureImp$new(fitRF,loss="mae")
    
    #Create Naive Bayes model with stats
    trainb<-train
    trainb[1]<-as.factor(as.numeric(unlist(trainb[1])))
    testb<-test
    testb[1]<-as.factor(as.numeric(unlist(testb[1])))
    trainb<-as.data.frame(trainb)
    testb<-as.data.frame(testb)
    
    dependenttrainNaive<-as.factor(as.numeric(unlist(train[1])))
    dependenttestNaive<-as.factor(as.numeric(unlist(test[1])))
    fitBayes<-NaiveBayes(dependenttrainNaive~.,data = trainb)
    predBayes<-predict(fitBayes,testb)
    AccBayes<-mean(ifelse(predBayes$class==dependenttestNaive,1,0))
    MAE<-gh(mae(dependenttestNaive,predBayes$class)) 
    a6<-c('NaiveBayes',MAE,gh(mean(AccBayes)*100),"NA","NA")
    
#Create predictions from above models
    predlm<<-predict(fitbest,newdata = test)
    predglm<<-predict(fitglm,newdata = test, type = "response")
    predLogistic<<-predict(fitdown,newdata = test, type = "response")
   

    
#lm bestfit stats
  #MAX of 7 levels for dependent variable
    #try(if(n_distinct(dependenttest)>7) stop ("dependent variable has more than 7 levels - modify program"))
    #try(if(8>7) stop ("dependent variable has more than 7 levels - modify program"))
    
  #add "Accuracy" variable to test lm model
    testlm<-test
    testlm<-mutate(testlm,Accuracy=ifelse(predlm<.5,0,
                ifelse(predlm<1.5&predlm>=.5,1,
                ifelse(predlm<2.5&predlm>=1.5,2,
                ifelse(predlm<3.5&predlm>=2.5,3,
                ifelse(predlm<4.5&predlm>=3.5,4,
                ifelse(predlm<5.5&predlm>=4.5,5,6)))))))
    Acc1<-ifelse(testlm$Accuracy==dependenttest,1,0)
    Acc1perc<-mean(na.omit(Acc1))*100
    aa<-summary(fitbest)
    MAE<-gh(mae(dependenttest,predlm))
    
#Create objects for below grid.table
    a1<-c('method','MeanAbsolErr','Accuracy','StrengthMethod','StrengthValue')
    a2<-c('lm',MAE,gh(Acc1perc),"AdjRsq", gh(aa$adj.r.squared))
    
#glm bestfit stats add "Accuracy" variable to test model
    testglm<-test
    testglm<-mutate(testglm,Accuracy=ifelse(predglm<.5,0,
              ifelse(predglm<1.5&predglm>=.5,1,
              ifelse(predglm<2.5&predglm>=1.5,2,
              ifelse(predglm<3.5&predglm>=2.5,3,
              ifelse(predglm<4.5&predglm>=3.5,4,
              ifelse(predglm<5.5&predglm>=4.5,5,6)))))))

    Acc1<-ifelse(testglm$Accuracy==dependenttest,1,0)
    Acc1perc<-mean(na.omit(Acc1))*100
    aa<-summary(fitglm)
    MAE<-gh(mae(dependenttest,predglm))
    #Create object for below grid.table    
    a3<-c('glm',MAE,gh(Acc1perc),"AIC",gh(aa$aic))    
    
#Logistic bestfit stats add "Accuracy" variable to test model
    testLogistic<-test
    testLogistic<-mutate(testLogistic,Accuracy=ifelse(predLogistic<.5,0,
              ifelse(predLogistic<1.5&predLogistic>=.5,1,
              ifelse(predLogistic<2.5&predLogistic>=1.5,2,
              ifelse(predLogistic<3.5&predLogistic>=2.5,3,
              ifelse(predLogistic<4.5&predLogistic>=3.5,4,
              ifelse(predLogistic<5.5&predLogistic>=4.5,5,6)))))))
    
    Acc1<-ifelse(testLogistic$Accuracy==dependenttest,1,0)
    Acc1perc<-mean(na.omit(Acc1))*100
    aa<-summary(fitdown)
    MAE<-gh(mae(dependenttest,predLogistic))

  #Create object for below grid.table    
    a4<-c('Logistic',MAE,gh(Acc1perc),"AIC",gh(aa$aic))
    
#Create gridtable of stats from above methods
    tblstat<-tbl_df(as.data.frame(rbind(a2,a3,a4,a5,a6)))
    names(tblstat)<-a1
    tblstat<<-tblstat
    grid.table(tblstat,row=NULL )
    
#Create tbl_df of predictions from fitbest lm method for future use
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

    } 
    
#Shows comparison stats on all methods used
    #grid.table(tblstat,row=NULL )
    
    tbl<-tbl_df(tbl1)
    tbl$id<-as.integer(rownames(tbl))
    tbl[-ncol(tbl)]<-sapply(tbl[-ncol(tbl)],gh)
    tbl[-ncol(tbl)]<-sapply(tbl[-ncol(tbl)],as.numeric)
#Makes grid of predictor stats from fitbest/lm model
    #grid.table(tbl,row=NULL ) #make
    
#Makes plot showing actual vrs predicted values for bestfit/lm
    tblPred<-tbl[-2:-3]
    names(tblPred)<-c("Value","id")
    tblAct<-tbl[2:4]
    tblAct<-tblAct[-2]
    names(tblAct)<-c("Value","id")
    
    p1<-ggplot()+
      geom_point(data=tblPred,aes(y=Value,x=id),color="blue",size=6,alpha=.4)+
    
      geom_point(data=tblAct,aes(y=Value,x=id),color="red",size=2,alpha=.4)+
      
      geom_hline(yintercept = .5,color="black",size=1, linetype="dashed" )
      
    p1
    

    
#RandomForest error Rate as the trees progress
    ErrRate$id<-as.numeric(ErrRate$id)
    names(ErrRate)<-c("OOB","zero","one","id")
    
    p2<-ggplot()+
      geom_point(data=ErrRate,aes(y=one,x=id),color="dark green",size=3,alpha=.6)
    
    p2
    
    
    
    
#Case2 - Continous vrs Categorical
  #Use ANOVA
  #H0 - all populatiion means are equal
  #H1 - at least one population mean is different
  #F-Ratio = Mean Between Groups Sum of Squares divided by Mean within Groups Sum of Squares
  #In Anova "type" Mean Sq is between groups and "Residuals" is within groups
  #Therefore higher F_Ratio(and low P-val) means reject H0

#ex - boxplot shows means are different
    # boxplot(predictor~type,d1,
    #         main="predictor vs Occupation",
    #         ylab="predictor")

# ex - anova shows high F-Value and low P-val - thus reject HO
    # d23<-d1
    # #d23[-6]<-scale(d23[-6])#not needed
    # d23<-tbl_df(as.data.frame(d23))
    # #d23$type<-as.factor(d23$type) #not needed
    # 
    # fit<-aov(prestige~type,d23)
    # anova(fit)
    # testaov<-mutate(test,Accuracy=ifelse(pred<(1+ErrLimit)*income&pred>(1-ErrLimit)*income,1,0))
    # Acc1perc<-mean(na.omit(testaov$Accuracy))*100
    # aa<-anova(fit)
    # MAE<-gh(mae(testaov$prestige,pred))
    # pVal<-aa$`Pr(>F)`[1]
    # MAE
    # pVal
    
