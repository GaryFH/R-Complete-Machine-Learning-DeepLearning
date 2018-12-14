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
      

#Make test and training sets
      # d1a<-d1
      # d1a$id<-rownames(d1a)
      # train<-sample_n(d1a,.7*nrow(d1a))
      # test<-setdiff(d1a,train)
      # try(if(nrow(d1a)!=nrow(train)+nrow(test)) warning("test/train dim issues"))
      # train<-train[-ncol(train)]
      # test<-test[-ncol(test)]
      
      
#Correlation coding stuff
    
    
    df<-tbl_df(mtcars)
    tt2<-data.frame()[1:(ncol(df)-1),]
    
    for (i in 1:ncol(df))  {

    tt<-as.data.frame(cor(df[-i],df[i]))
    tt[paste("Var",i,sep="")]<-rownames(tt)
    tt[1]<-as.data.frame(apply(tt[1],2,gh))
    tt[1]<-as.numeric(as.character(tt[[1]]))
    tt[2]<-as.factor(as.character(tt[[2]]))
    tt[1]<-sort(abs(tt[[1]]),decreasing = TRUE)
    tt<<-tt
    tt2<<-cbind(tt2,tt)
    }
gfh<-tbl_df(tt2)

grid.table(gfh,row=NULL)
      


