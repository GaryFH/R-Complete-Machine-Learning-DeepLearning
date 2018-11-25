
library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(car)
library(carData)
data("Prestige")

Prestige_miss<-read.csv('https://raw.githubusercontent.com/selva86/datasets/master/Prestige_miss.csv')
d1<-tbl_df(Prestige_miss)
summary(d1)

#If datasets are large enough then considered removing observations with missing values
d2b<-na.omit(d1)
summary(d2b)
d2b


#If a variable has a large % of missing values - then consider removing that variable
#CODE BELOW IS FOR N/A'S not MISSING VALUES: "NA"
# aa<-0
# bb<-c()
# dd<-.006 #dd is percent na's you allow in a column
# gfh<-d1
# gfhb<-d1
# for(i in 1:ncol(gfh))  {
#   
#   aa<-nrow(filter(gfh,gfh[,i]=="N/A"|gfh[,i]=="NA"))
#   #aa<-nrow(filter(gfh,gfh$Upstream.lane.0=="N/A")) - this also works
#   #not that
# 
#   
#   if (aa>dd*nrow(gfh)){
#     
#     gfhb<<-gfhb[-i]
#     
#   } else {
#     next
#   }
#    
# }
# ncol(gfh)
# ncol(gfhb)
# setdiff(names(gfh),names(gfhb))
# d11<-gfhb
# 
#CODE BELOW IS FOR MISSING VALUES: "NA" - NOT FOR N/A'S - doesn't work on factor variables
aa<-0
bb<-c()
dd<-.05 #dd is percent na's you allow in a column
# gfh<-tbl_df(sapply(gfh,as.character))
gfh<-d1[-6]
for(i in 1:ncol(gfh))  {
  
  a1<-complete.cases(gfh[i])
  gg<-subset(gfh[i],a1==FALSE)


  aa<-nrow(gg)
  print(aa)
  #aa<-nrow(filter(gfh,gfh$Upstream.lane.0=="N/A")) - this also works
  #not that


  if (aa>dd*nrow(gfh)){

    bb<-c(bb,i)
    

  } else {
    next
  }

}
print(bb)
gfhb<-gfh[-bb]
ncol(gfh)
ncol(gfhb)
setdiff(names(gfh),names(gfhb))


#impute function part of Hmisc or e1071
#Note for continuous use mean for others use mode

d2<-as.data.frame(d1)
d2[1]<-e1071::impute(d2[1],what="mean")
summary(d2$education)

d3<-as.data.frame(d1)
d3[-6]<-sapply(d3[-6],function(x){e1071::impute(d3[-6],what="mean")})
d4<-as.data.frame(d3)
summary(d4)

d4$type<-Hmisc::impute(d4$type,mode)
summary(d4)

#note that mice funtion does lots of missing value stuff automatically 
#PREDICTIVE MEANS MATCHING IS MOST COMMON METHOD USED IN mice
library(mice)

micemod<-mice(d1)
d5<-tbl_df(complete(micemod,1)) 
summary(d5)
d6<-tbl_df(complete(micemod,2))#uses impute method
summary(d6)

#CHALLENGE  impute Cars93_miss


dd1<-tbl_df(read.csv("https://raw.githubusercontent.com/selva86/datasets/master/Cars93_miss.csv"))

library(microbenchmark)
microbenchmark(
  gfa<-dplyr::select_if(dd1, is.numeric),
  gfb<-Filter(is.numeric,dd1))  #Filter is much faster than select_if
  

  dd2<-gfb
  gfc<-Filter(is.factor,dd1)
  dim(gfc)
  dim(gfb)
  dim(dd1)


dd3<-gfc

#works with numeric/integer variables
micemod2<-mice(dd2)
dd5<-tbl_df(complete(micemod2,1)) 

#DOES NOT WORK with factor variables
# micemod3<-mice(dd3)
# dd6<-tbl_df(complete(micemod3,1)) 

#Trying impute method from above for factor variables
dd4<-sapply(dd3,function(x){Hmisc::impute(x,mode)})
dd4<-tbl_df(as.data.frame(dd4))
summary(dd4)

ddd1<-tbl_df(cbind(dd5,dd4))
setcolorder(ddd1,names(dd1)) #setcolorder is in library(data.table)
names(ddd1)==names(dd1)




