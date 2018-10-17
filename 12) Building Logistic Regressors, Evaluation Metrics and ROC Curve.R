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
#NOTE THIS DOESN'T WORK WITH tbl_df
#Convert ord.factor to numeric (first go to character) - not response variable "Class"

#ONLY WORKS WITH
for (i in 1:9)  {
  d1b[,i]<-as.numeric(as.character(d1b[,i]))
  
  
}

d1<-d1b
d2<-tbl_df(d1)

#turn responce variable into 0 & 1
d2<-mutate(d2,Class=ifelse(Class=="malignant",1,0))
d2$Class<-as.factor(d2$Class)

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
trainDown<-downSample(x=dplyr::select(train,-Class),y=train$Class)
summary(trainDown$Class)
trainUp<-upSample(x=dplyr::select(train,-Class),y=train$Class)
summary(trainUp$Class)


#play with stepwise to find best model
#Stepwise returns the best model (ie lowest AIC)
trainb<-train
trainb$Class<-as.integer(trainb$Class)
fitbase<-lm(Class~1,trainb)
fitall<-lm(Class~ 
 Cl.thickness*Cell.size*Cell.shape*Marg.adhesion*Epith.c.size*Bare.nuclei*Bl.cromatin*Normal.nucleoli*Mitoses ,trainb)

fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
              direction = "both",trace=1,steps=1000)

aa<-summary(fitbest)
aa$adj.r.squared
aa$call
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

gg<-c()
hh<-0
for (i in 1:length(rr)) {
  hh<-paste("+",rr[i],sep = "")
  gg<-paste(gg,hh,sep="")
  }
ggg<-substring(gg,2)

#NOTE following is easier than above for loop and identical
rrr<-paste(rr,collapse="+")
identical(ggg,rrr) #=True

fitbest2<-lm(data = trainb,formula = as.formula(paste("Class~",ggg)))
aa1<-summary(fitbest2)
aa1$adj.r.squared
aa1$call


# anova(f1,f2,f3,f4) #Note that high pvalue for 4th mod indicates women not needed


#Challenge use fitbest from above and throw out any term with VIF higher than four 


car::vif(fitbest)


#Now build model
fitdown<-glm(Class~Cl.thickness + Cell.size + Cell.shape, family = binomial, data = trainDown)
#summary(fitdown)
predDown<-predict(fitdown, newdata= test, type = "response")

#set level for malignant vrs benign at .5
prednum1<- ifelse(predDown>0.5,1,0)
y_pred<-as.factor(prednum1)

Accuracy1<-mean(y_pred==as.factor(test$Class))
Accuracy1
a1<-summary(fitdown)
a1$aic #lower aic suggest better model



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


#for fun use bestfit call from above for fitdown

fitdown2<-glm(Class ~ Cell.size + Bare.nuclei + Cl.thickness + 
                Normal.nucleoli + Bl.cromatin + Mitoses + Cell.shape + Marg.adhesion + 
                Cell.size:Bare.nuclei + Cell.size:Mitoses + Cell.size:Cell.shape + 
                Normal.nucleoli:Bl.cromatin + Bare.nuclei:Marg.adhesion + 
                Cl.thickness:Marg.adhesion + Bare.nuclei:Cl.thickness + Bare.nuclei:Mitoses + 
                Cl.thickness:Normal.nucleoli + Cell.size:Bare.nuclei:Mitoses + 
                Bare.nuclei:Cl.thickness:Marg.adhesion,family = binomial, data = trainDown)
predDown3<-predict(fitdown2, newdata= test, type = "response")

#set level for malignant vrs benign at .5
prednum3<- ifelse(predDown3>0.5,1,0)
y_pred3<-as.factor(prednum3)

Accuracy3<-mean(y_pred3==as.factor(test$Class))
Accuracy3
a3<-summary(fitdown2)
a3$aic  #lower aic suggest better model


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



# caret::confusionMatrix(y_pred,as.factor(test$Class),positive="1")

