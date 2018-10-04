#(Prestige,package = "car")
library(dplyr)

d1<-tbl_df(Prestige)
sapply(d1,mean)
sapply(d1,sd)
summary(d1)
#describe from Hmisc package
library(Hmisc)
describe(d1)
library(pastecs)
stat.desc(d1)

#challenge  find coef of variation & 95th percentile for "price" in Cars93
library(MASS)
d2<-tbl_df(Cars93)
stat.desc(d2)
describe(d2)
#Answers .4951096  & 36.74