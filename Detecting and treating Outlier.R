#OUTLIERS - typically 3 time IQR or 1.5 times 75th percentile

library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)

d1<-tbl_df(Prestige)
sapply(d1,IQR,na.rm=T)
d2<-d1$income
quan<-quantile(d2)
quan75<-quan[4]
iqr<-IQR(d2)
#therefore outliers could be defined as 1.5*quan75 or 3*iqr
1.5*quan75
3*iqr

#outliers listed/idenified
d3<-d2[d2>3*iqr]
d3
d4<-d2[d2>1.5*quan75]
d4

#one way of dealing with outliers is to "cap" by replacing all values higher
    #than 95% with the 95% value and similar with those less than 5%

d2<-d1$income
d2[d2>quantile(d2,0.95)]<-quantile(d2,0.95)
d2[d2<quantile(d2,0.05)]<-quantile(d2,0.05)

summary(d2)

#CHALLENGE: Do above for 99th and 1st percentiles

d2<-d1$income
d2[d2>quantile(d2,0.99)]<-quantile(d2,0.99)
d2[d2<quantile(d2,0.01)]<-quantile(d2,0.01)

summary(d2)
