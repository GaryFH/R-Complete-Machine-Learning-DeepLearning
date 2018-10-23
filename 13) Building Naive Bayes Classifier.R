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
library(klaR)
library(ModelMetrics)
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 
data("Vehicle")
d1a<-tbl_df(Vehicle)
d11<-dplyr::select(d1a,Class,everything())
d11b<-d11[-1]
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 

train<-sample_n(d11,.7*nrow(d11))
test<-setdiff(d11,train)
trainb<-train[-1]
testb<-test[-1]


#look at box plots for each variable based on each of 
#four levels for the only factor variable "Class"
caret::featurePlot(d11b,d11$Class,plot = "box")
#note that only one predictor variable has significant differences
#between the means for each level - this means a weak model (Accuraccy is only 44%)


#Train a model with the klaR Naive Bayes package - 
#accuracy is only 45% - not uncommon for a factor with 4 levels
fit1<-NaiveBayes(Class~.,data = train)
pred1<-predict(fit1,test)
accuracy1<-mean(ifelse(pred1$class==test$Class,1,0))
accuracy1

#Challenge predict class of "Species' from the "iris" dataset (from "datasets package")

data("iris")
d2<-tbl_df(iris)

train2<-sample_n(d2,.7*nrow(d2))
test2<-setdiff(d2,train2)

caret::featurePlot(d2[-5],d2$Species,plot = "box")
#note that all predictor variables have significant differences
#between the means for each level - this means a strong model (Accuraccy is 95%)

fit2<-NaiveBayes(Species~.,data=train2)
pred2<-predict(fit2,test2)
accuracy2<-mean(ifelse(pred2$class==test2$Species,1,0))
accuracy2 #93.3%


#For fun, let's try using lm

#play with stepwise to find best model
#Stepwise returns the best model (ie lowest AIC)
train2b<-train2
train2b<-mutate(train2b,Species=ifelse(Species=="setosa",1,
                            ifelse(Species=="versicolor",2,3)))
test2b<-test2
test2b<-mutate(test2b,Species=ifelse(Species=="setosa",1,
                                     ifelse(Species=="versicolor",2,3)))

#Make list of names for fitall below
#manually copy and past bb to make summary call look good
bba<-as.list(names(train2b[-5]))
bba<-paste(bba,collapse = " * ")
bba

fitbase<-lm(Species~1,train2b)
fitall<-lm(Species~Sepal.Length * Sepal.Width * Petal.Length * Petal.Width,
             train2b)

fitbest<-step(fitbase, scope = list(lower=fitbase,upper=fitall),
              direction = "both",trace=1,steps=1000)

aa<-summary(fitbest)
aa$adj.r.squared
aa$call

pred3<-predict(fitbest,newdata = test2b)
pred3b<-ifelse(pred3<1.5,1,
               ifelse(pred3<2.5,2,3))
Accuracy3<-mean(ifelse(pred3b==test2b$Species,1,0))
Accuracy3 #97.7%
aa<-(summary(fitbest))
AdjR3<-aa$adj.r.squared


#make dataframe of bestfit predictors to help allow for automatic use of predictors
#note that all predictors have have low pValue - thus will keep them all
bb<-summary(fitbest)
cc<-as.data.frame(bb$coefficients)
Names<-rownames(cc) #make character vector of rownames of bb$coeficients
ee<-tbl_df(cbind(Names,cc))
ff<-rename(ee,"Pval"="Pr(>|t|)")  #get rid of odd characters
gg<-filter(ff,Pval<.05) #Remove bestfit predictors with high Pvalues
regressornames<-as.character(gg$Names[-1])
rr<-regressornames


#Try GLM for fun - remember using + instead of * is best for GLM

bc<-as.list(names(gg[-1]))
bc<-paste(bc,collapse = "+")
bc
fit3<-glm(Species~Petal.Width+Petal.Length+Sepal.Length+Sepal.Width+
        Petal.Width:Petal.Length+Sepal.Length:Sepal.Width+Petal.Length:Sepal.Width,
          family = gaussian, data = train2b)

aa2<-summary(fit3)
AIC(fit3)
aa2$call

pred4<-predict(fit3,newdata = test2b)
pred4b<-ifelse(pred4<1.5,1,
               ifelse(pred4<2.5,2,3))
Accuracy4<-mean(ifelse(pred4b==test2b$Species,1,0))
Accuracy4
aa<-(summary(fit3))
AdjR4<-aa$adj.r.squared

fitnova<-aov(Species~Petal.Width+Petal.Length+Sepal.Length+Sepal.Width+
               Petal.Width:Petal.Length+Sepal.Length:Sepal.Width+
               Petal.Length:Sepal.Width,data = train2)
pred5<-predict(fitnova, newdata = test2)
pred5b<-ifelse(pred5<1.5,1,
               ifelse(pred5<2.5,2,3))
Accuracy5<-mean(ifelse(pred5b==test2b$Species,1,0))
Accuracy5


fitnovab<-aov(Species~Petal.Width*Petal.Length*Sepal.Length+Sepal.Width*
               Petal.Width:Petal.Length*Sepal.Length:Sepal.Width*
               Petal.Length:Sepal.Width,data = train2)
pred6<-predict(fitnovab, newdata = test2)
pred6b<-ifelse(pred6<1.5,1,
               ifelse(pred6<2.5,2,3))
Accuracy6<-mean(ifelse(pred6b==test2b$Species,1,0))
Accuracy6




#Note all five methods are very close
#IF YOU RUN A NUMBER OF TIMES THE ANSWERS VARY 
#BUT ALL METHODS CAN BE THE BEST, THE SAME OR THE WORST

accuracy2 #Naive Bayes
Accuracy3 #LM 
Accuracy4 #GLM 
Accuracy5 #aov with "+" as separator
Accuracy6 #aov with "*" as separator
