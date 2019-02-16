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
library(tm)

#library(BBmisc) #contains normalize function - didn't work below?
gh <- function(x, d=3) sprintf(paste0("%1.",d,"f"), x) 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
  }


#Read Text
vec1<-readLines("https://raw.githubusercontent.com/selva86/datasets/master/yoga_wiki.txt")


#Creat Corpus

cp<-Corpus(VectorSource(vec1))
cp

inspect(cp)












