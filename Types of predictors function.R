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
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#THREE TYPES OF CASES
#Case1 - Continous vrs Continuos
#Case2 - Continous vrs Categorical
#Case3 - Categorical vrs Categorical


#IMPORTANT NOTES FOLLOW:
#If you don't want observations with NA's removed then use MICE or impute to convert NA's
#MAKE RESPONCE VARIABLE NUMBER 1 i.e.- dplyr::select(d1,income,everything())

#i.e data=d1,  ErrLimit=.2(for good prediction if within 20%)

ContvrsCont<- function(data,ErrLimit) {


d1<-data
d1<-na.omit(d1)
ErrLimit<<-ErrLimit

#FORCE RESPONCE INTO A NUMERIC VARIABLE
gg<-as.numeric(unlist(as.list(d1[1])))
d1[1]<-gg

d1$id<-rownames(d1)
train<-sample_n(d1,.7*nrow(d1))
test<-setdiff(d1,train)
ag<-nrow(d1)==(nrow(train)+nrow(test))
try(if(ag!="TRUE") warning ("train/test problem"))
train<-train[-ncol(train)]
test<-test[-ncol(test)]
d1<-d1[-ncol(d1)]

predictor<-as.numeric(unlist(train[1]))
fitall1<-lm(predictor~.,train[-1])

b1<-as.list(names(train[-1]))
b2<-paste(b1,collapse = " * ")

fitbase<-lm(predictor~1,train)
fitall<-lm(data=train,as.formula(paste(names(train)[1],"~",b2)))

fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
              direction = "both",trace=1,steps=1000)


pred1<<-predict(fitbest,newdata = test)
test1<-test
test1<-mutate(test1,Accuracy=ifelse(pred1<(1+ErrLimit)*income&pred1>(1-ErrLimit)*income,1,0))
Acc1perc20<-mean(na.omit(test1$Accuracy))*100
aa<-summary(fitbest)
MAE<-gh(mae(test$income,pred1))

bbbn<<-paste("MeanAbsoluteError=",MAE,"  ACCURACY(within", ErrLimit*100,"%) = ",gh(Acc1perc20),
      "%  -  AdjR=",gh(aa$adj.r.squared)," Stepwise bestfit variables(from stats package)",sep = "")

byt<-test[1]
names(byt)<-"ActualValue"
byt$ActualValue<-as.numeric(byt$ActualValue)

ty<-as.data.frame(pred1)
names(ty)<-"PredictedValue"
ty$PredictedValue<-as.numeric(ty$PredictedValue)

ty2<-as.data.frame(100*(pred1-test[1])/test[1])
names(ty2)<-"PercentError"
ty2$PercentError<-gh(ty2$PercentError)
ty2$PercentError<-as.numeric(ty2$PercentError)

tbl1<<-cbind(ty,byt,ty2)

} 
pred1<-pred1
test1<-test1
tbl1<-tbl1
ErrLimit<-ErrLimit
grid.table(tbl1,row=NULL )
bbbn<-bbbn
bbbn

tbl<-tbl_df(tbl1)
tbl$id<-as.integer(rownames(tbl))
tblPred<-tbl[-2:-3]
names(tblPred)<-c("Value","id")
tblAct<-tbl[2:4]
tblAct<-tblAct[-2]
names(tblAct)<-c("Value","id")

p1<-ggplot()+
  geom_point(data=tblPred,aes(y=Value,x=id),color="blue",size=3,alpha=.7)+

  geom_point(data=tblAct,aes(y=Value,x=id),color="red",size=3,alpha=.7)


# 
# pred1<-predict(fit1b,newdata = test)
# test1<-test
# test1<-mutate(test1,Accuracy=ifelse(pred1<1.2*income&pred1>.8*income,1,0))
# Acc1bperc20<-mean(na.omit(test1$Accuracy))*100
# aa<-summary(fit1b)
# MAE<-gh(mae(test$income,pred1))
# 
# paste("MeanAbsoluteError=",MAE," ACCURACY(within 20%) = ",gh(Acc1bperc20),"%  -  AdjR=",gh(aa$adj.r.squared)," education only variable",sep = "")
# 
# pred1<-predict(fit1all,newdata = test)
# test1<-test
# test1<-mutate(test1,Accuracy=ifelse(pred1<1.2*income&pred1>.8*income,1,0))
# Acc1allperc20<-mean(na.omit(test1$Accuracy))*100
# aa<-summary(fit1all)
# MAE<-gh(mae(test$income,pred1))
# 
# paste("MeanAbsoluteError=",MAE,"  ACCURACY(within 20%) = ",gh(Acc1allperc20),"%  -  AdjR=",gh(aa$adj.r.squared)," all variables",sep = "")
# 
# pred1<-predict(fitbest,newdata = test)
# test1<-test
# test1<-mutate(test1,Accuracy=ifelse(pred1<1.2*income&pred1>.8*income,1,0))
# Acc1bestperc20<-mean(na.omit(test1$Accuracy))*100
# aa<-summary(fitbest)
# MAE<-gh(mae(test$income,pred1))
# 
# paste("MeanAbsoluteError=",MAE,"  ACCURACY(within 20%) = ",gh(Acc1bestperc20),"%  -  AdjR=",gh(aa$adj.r.squared)," bestfit variables",sep = "")
# 
# 
# 
# 
# #Case2 - Continous vrs Categorical
# #Use ANOVA
# #H0 - all populatiion means are equal
# #H1 - at least one population mean is different
# #F-Ratio = Mean Between Groups Sum of Squares divided by Mean within Groups Sum of Squares
# #In Anova "type" Mean Sq is between groups and "Residuals" is within groups
# #Therefore higher F_Ratio(and low P-val) means reject H0
# 
# #ex - boxplot shows means are different
# boxplot(income~type,d1,
#         main="Income vs Occupation",
#         ylab="Income")
# 
# #ex - anova shows high F-Value and low P-val - thus reject HO
# fit<-aov(income~type,d1)
# summary(fit)
# 
# 
# 
# #Case3 - Categorical vrs Categorical
# #Use Chi-Sq 
# #chi^2=Chi-Squared Stat
# #Oi=number of observations of type i
# #Ei=the expected Frequency of type i
# #n=number of cells in the table
# 
# #Add a variable to d1 called "income_cat" with four categories using dplyr's ntile function
# d1$income_cat<-ntile(d1$income,4)
# table(d1$income_cat,d1$type)
# chisq.test(y=d1$income_cat, x=d1$type)
# #low P-val & high chi^2 means reject H0
# 
# 
# #CHALLENGE: determine which var in Prestige (d1) are stat sig to "income" var
# 
# #case1 variables (edu,women,prestige, census)
# eduCor<-cor.test(d1$income,d1$education)
# womenCor<-cor.test(d1$income,d1$women)
# prestigeCor<-cor.test(d1$income,d1$prestige)
# censusCor<-cor.test(d1$income,d1$census)
# 
# eduCor$p.value
# womenCor$p.value
# prestigeCor$p.value
# censusCor$p.value
# 
# #case2 variable (type)
# fit<-aov(income~type,d1)
# summary(fit)
# 
# 
# 
