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
data("BreastCancer")
dd<-BreastCancer[complete.cases(BreastCancer),]
d1a<-dd

# summary(d1)
# summary(d2)

#delete ID variable
d1b<-d1a[,-1]
d1<-tbl_df(d1b)
#Convert ord.factor to numeric (first go to character) - not response variable "Class"
d1[1:5]<-sapply(d1[1:5],as.character)
d1<-dplyr::mutate(d1,Class=ifelse(Class=="benign",0,1))
d1<-tbl_df(as.data.frame(sapply(d1,as.numeric)))




#NOTE THIS DOESN'T WORK WITH tbl_df
#Convert ord.factor to numeric (first go to character) - not response variable "Class"
# #ONLY WORKS WITH
# for (i in 1:9)  {
#   d1b[,i]<-as.numeric(as.character(d1b[,i]))
#   
#   
# }
#d1<-d1b

d2<-d1

#This is scaling - NOT NEEDED maybe lm takes care of scaling for you.
# d22<-d2
# d22[1:9]<-tbl_df(as.data.frame(scale(d22[1:9])))



table(d2$Class)

#For this to work they want training data or have both class values about equal (currently twice as many 0's")
#To fix - use carat package downsampling
set.seed(111)
# trainIndex<-createDataPartition(d2$Class,p=.7,list = F)
# train<-d2[trainIndex,]
# test<-d2[-trainIndex,]

#Training
d3<-d2
d3<-mutate(d3,id=row_number()) #some rows are identical therefore "id"
              # was added to allow all rows to be unique for setdiff to work
train<-sample_n(d3,.7*nrow(d3))
test<-setdiff(d3,train)
train<-dplyr::select(train,-id)
test<-dplyr::select(test,-id)

#now reduce num of 0's to same as num of #1's in the train dataframe (downsampling)
#note that downSample & upSample require a factor Responce variable
trainDown<-downSample(x=dplyr::select(train,-Class),y=as.factor(train$Class))
summary(trainDown$Class)
trainUp<-upSample(x=dplyr::select(train,-Class),y=as.factor(train$Class))
summary(trainUp$Class)


#play with stepwise to find best model
#Stepwise returns the best model (ie lowest AIC)
trainb<-train
testb<-test
fitbase<-lm(Class~1,trainb)
fitall<-lm(Class~ 
 Cl.thickness*Cell.size*Cell.shape*Marg.adhesion*Epith.c.size*Bare.nuclei*Bl.cromatin*Normal.nucleoli*Mitoses ,trainb)

fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
              direction = "both",trace=1,steps=1000)

aa<-summary(fitbest)
AdjRsq_lmAcc<-aa$adj.r.squared
aa$call
pred1<-predict(fitbest,newdata = testb)
pred1<-as.numeric(pred1)
Acc<-ifelse(pred1<.5,0,1)
Acc2<-ifelse(Acc==testb$Class,1,0)
lmAcc<-mean(Acc2)*100
lmAcc


#look for strongest of "fitbest" predictors
aaa<-tbl_df(as.data.frame(rownames(aa$coefficients)))
names(aaa)="Predictors"
bbb<-tbl_df(as.data.frame(aa$coefficients))
names(bbb)=c("Est","StdErr","Tvalue","pVal")
qualityPredictors<-tbl_df(cbind(aaa,bbb))
qualityPredictors<-qualityPredictors[-1,]
# bestPredictors<-dplyr::filter(qualityPredictors,pVal<.05)
# bestPredictors<-bestPredictors[-1,]

      gf<-0
        gf3<-c()
        gf4<-c()
        gf2<-0
        trainb<-train[-10]
        uu<-intersect(names(trainb),as.character(qualityPredictors$Predictors))
    for (i in 1:length(uu)) {

      gf<<-as.character(uu[i])
      
      gfh2<-data.frame()
      for (i in 1:length(uu)) {
      if(gf==uu[i]){gfh2<<-trainb[i]}
      }
      pred<-as.numeric(unlist(gfh2))
      
 
      fit<-lm(Class~pred,train)
      aa<-summary(fit)
      gf2<<-gh(aa$adj.r.squared)
      gf2b<-as.data.frame(aa$coefficients)
      gf2b<-gf2b[2,4]
      gf3<<-c(gf,gf2,gf2b)
      gf4<<-rbind(gf4,gf3)
      gf4<-tbl_df(gf4)
      names(gf4)<-c("Predictor","AdjRsq","Pval")
      gf4<-arrange(gf4,desc(AdjRsq))
  
        }

        
        grid.table(gf4,row=NULL)
        






#make dataframe of bestfit predictors to help allow for automatic use of predictors

bb<-summary(fitbest)
cc<-as.data.frame(bb$coefficients)
Names<-rownames(cc) #make character vector of rownames of bb$coeficients
ee<-tbl_df(cbind(Names,cc))
ff<-rename(ee,"Pval"="Pr(>|t|)")  #get rid of odd characters
gg<-filter(ff,Pval<.05) #Remove bestfit predictors with high Pvalues
regressornames<-as.character(gg$Names[-1])
rr<-regressornames

#use low Pval regressor/predictor names from above 
#to make a new model (fitbest2) with fewer predictor variables
#NOTE, FOR lm: using "+" predictor connector instead of "*" connector 
#gives significantly lower AdjRsq (worse model)

gg<-c()
hh<-0
for (i in 1:length(rr)) {
  hh<-paste("*",rr[i],sep = "")
  gg<-paste(gg,hh,sep="")
  }
ggg<-substring(gg,2)

#NOTE following is easier than above for loop and identical results
                    # - note it only works with a list
rrr<-paste(rr,collapse="*")
identical(ggg,rrr) #=True

fitbest2<-lm(data = trainb,formula = as.formula(paste("Class~",ggg)))
aa1<-summary(fitbest2)
aa1$adj.r.squared
aa1$call



car::vif(fitbest) #A Regressor with a VIF = 1 - (not correlated)
                  #A Regressor VIF = 1 TO 5 - (moderately correlated)
#High VIF doesn't make a model bad but shows weakness in a single predictor
#Note that the VIF's for bestfit regressors/predictors range from 6.5 to 107



  #Now build model per course outline 
# predictors given no mention of their origin
fitdown<-glm(Class~Cl.thickness + Cell.size + Cell.shape, family = binomial, data = trainDown)
#summary(fitdown)
predDown<-predict(fitdown, newdata= test, type = "response")

#set level for malignant vrs benign at .5
prednum1<- ifelse(predDown>0.5,1,0)

#THE TEACHER USED THIS METHOD?
# y_pred<-as.factor(prednum1)
# 
# Accuracy1<-mean(y_pred==as.factor(test$Class))

#I like this method - it is simpler and
Accuracy1<-mean(ifelse(prednum1==test$Class,1,0))

Accuracy1
a1<-summary(fitdown)
a1$aic #lower aic suggest better model
#car::residualPlots(fitdown)



#For fun try fitup
fitup<-glm(Class~Cl.thickness + Cell.size + Cell.shape, family = binomial, data = trainUp)
#summary(fitup)
predup<-predict(fitup, newdata= test, type = "response")
prednum2<- ifelse(predup>0.5,1,0)
y_pred2<-as.factor(prednum2)

Accuracy2<-mean(y_pred2==as.factor(test$Class))
Accuracy2

a2<-summary(fitup)
a2$aic  #lower aic suggest better model
car::residualPlots(fitup)


#for fun use bestfit regressors/predictors (from summary's "call") from above for fitdown

#NOTE, FOR glm: using "+" predictor connector instead of "*" connector 
#gives significantly higher AIC (better model)
#FOR lm it is the opposite - therefore "*" is best to use for lm and "+" is best for glm

fitdown2<-glm(Class ~ Cell.size + Bare.nuclei + Cl.thickness +
                Normal.nucleoli + Bl.cromatin + Mitoses + Cell.shape + Marg.adhesion +
                Cell.size:Bare.nuclei + Cell.size:Mitoses + Cell.size:Cell.shape +
                Normal.nucleoli:Bl.cromatin + Bare.nuclei:Marg.adhesion +
                Cl.thickness:Marg.adhesion + Bare.nuclei:Cl.thickness + Bare.nuclei:Mitoses +
                Cl.thickness:Normal.nucleoli + Cell.size:Bare.nuclei:Mitoses +
                Bare.nuclei:Cl.thickness:Marg.adhesion,family = binomial, data = trainDown)

# trainDown2b<-trainDown
#           trainDown2b$Class<-as.numeric(trainDown2b$Class)
# fitdown2<-lm(Class ~ Cell.size + Bare.nuclei + Cl.thickness +
#                 Normal.nucleoli + Bl.cromatin + Mitoses + Cell.shape + Marg.adhesion +
#                 Cell.size:Bare.nuclei + Cell.size:Mitoses + Cell.size:Cell.shape +
#                 Normal.nucleoli:Bl.cromatin + Bare.nuclei:Marg.adhesion +
#                 Cl.thickness:Marg.adhesion + Bare.nuclei:Cl.thickness + Bare.nuclei:Mitoses +
#                 Cl.thickness:Normal.nucleoli + Cell.size:Bare.nuclei:Mitoses +
#                 Bare.nuclei:Cl.thickness:Marg.adhesion, data = trainDown2b)


# fitdown2<-glm(Class ~ Cell.size*Bare.nuclei*Normal.nucleoli*Bl.cromatin*Mitoses*
#           Cell.shape*Marg.adhesion*Cell.size:Bare.nuclei*Cell.size:Mitoses*
#           Cell.size:Cell.shape*Normal.nucleoli:Bl.cromatin*Bare.nuclei:Cl.thickness*
#           Bare.nuclei:Mitoses*Cell.size:Bare.nuclei:Mitoses*
#           Bare.nuclei:Cl.thickness:Marg.adhesion,family = binomial, data = trainDown)


# fitdown2<-lm(Class ~ Cell.size*Bare.nuclei*Normal.nucleoli*Bl.cromatin*Mitoses*
#                Cell.shape*Marg.adhesion*Cell.size:Bare.nuclei*Cell.size:Mitoses*
#                Cell.size:Cell.shape*Normal.nucleoli:Bl.cromatin*Bare.nuclei:Cl.thickness*
#                Bare.nuclei:Mitoses*Cell.size:Bare.nuclei:Mitoses*
#                Bare.nuclei:Cl.thickness:Marg.adhesion, data = trainDown2b)



predDown3<-predict(fitdown2, newdata= test, type = "response")

#set level for malignant vrs benign at .5
prednum3<- ifelse(predDown3>0.5,1,0)
y_pred3<-as.factor(prednum3)

Accuracy3<-mean(y_pred3==as.factor(test$Class))
Accuracy3
a3<-summary(fitdown2)
a3$aic  #lower aic suggest better model
car::residualPlots(fitdown2)

#for fun use bestfit call from above for fitup/GLM

fitup2<-glm(Class ~ Cell.size + Bare.nuclei + Cl.thickness + 
                Normal.nucleoli + Bl.cromatin + Mitoses + Cell.shape + Marg.adhesion + 
                Cell.size:Bare.nuclei + Cell.size:Mitoses + Cell.size:Cell.shape + 
                Normal.nucleoli:Bl.cromatin + Bare.nuclei:Marg.adhesion + 
                Cl.thickness:Marg.adhesion + Bare.nuclei:Cl.thickness + Bare.nuclei:Mitoses + 
                Cl.thickness:Normal.nucleoli + Cell.size:Bare.nuclei:Mitoses + 
                Bare.nuclei:Cl.thickness:Marg.adhesion,family = binomial, data = trainUp)
predup4<-predict(fitup2, newdata= test, type = "response")

#set level for malignant vrs benign at .5
prednum4<- ifelse(predup4>0.5,1,0)
y_pred4<-as.factor(prednum4)

Accuracy4<-mean(y_pred4==as.factor(test$Class))
Accuracy4

a4<-summary(fitup2)
a4$aic  #lower aic suggest better model

#1)glm fitdown; 2)glm fitup; 3)glm "best" fitdown2; 4)glm "best" fitup2;  5)lm best 


Accuracy1
a1$aic
Accuracy3
a3$aic

Accuracy2
a2$aic
Accuracy4
a4$aic

AdjRsq_lmAcc
lmAcc


car::residualPlots(fitup2)



# caret::confusionMatrix(y_pred,as.factor(test$Class),positive="1")


#Following code makes a new variable "outlier" based on 
#if an observation's cook number is less than 4 (1 is outlier) 
#Note that "trainb" is data.frame used to make fitbest with lm function
# - note that no outliers were found
  ag<-cooks.distance(fitbest)
  trainOutlier<-data.frame()
    for(i in 1:length(ag)) {
    trainOutlier<-mutate(trainb, outlier=ifelse(ag[[i]]>4,1,0))
      }

 #1)is fitdown; 2)is fitup; 3)is fitdown2; 4)is fitup2 
  AdjRsq<-aa$adj.r.squared
  AccuratePerc1
  
  Accuracy1
  a1$aic
  Accuracy3
  a3$aic
  
  Accuracy2
  a2$aic
  Accuracy4
  a4$aic