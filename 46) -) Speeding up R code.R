library (dplyr)
library (ggplot2)
library(microbenchmark)


#make data.frame and test different coding practices that speed up a for loop

col1<-runif(12^4,0,2)
col2<-rnorm(12^4,0,2)
col3<-rpois(12^4,3)
col4<-rchisq(12^4,2)
df<-tbl_df(data.frame(col1,col2,col3,col4))
df
d1<-df
#this is sample for loop using above data - we will look at ways to make it faster

df<-d1

a1<-Sys.time()
for (i in 1:nrow(df)){
  if ((df[i,1]+df[i,2]+df[i,3]+df[i,"col4"])>4)
  {df[i,"newVariable"]<-"lgr 4"
  } else {
    df[i,"newVariable"]<-"smlr 4"
  }
  
}
a2<-Sys.time()
a1-a2  #this takes 13 seconds

#summary(as.factor(df$newVariable))


#now initialize/preallocate the new df variable

df<-d1

newvar<-character(nrow(df))

a1<-Sys.time()
for (i in 1:nrow(df)){
  if ((df[i,1]+df[i,2]+df[i,3]+df[i,"col4"])>4)
  {
    newvar[i]<-"lgr 4"
  } else {
   newvar[i]<-"smlr 4"
  }
  df$newvar<-newvar
}
a2<-Sys.time()
a1-a2  #this takes 12.5 seconds

      #summary(as.factor(df$newvar))

#use apply instead of for loop

      df<-d1
      a1<-Sys.time()

      GFHfunc<- function(x) {
        ifelse((x[1]+x[2]+x[3]+x["col4"])>4,"lgr4","smlr4")
       }

      newvar<-apply(df[,c(1:4)],1,FUN=GFHfunc)
      
      df$newvar<-newvar
      
      a2<-Sys.time()
      a1-a2  #This only took .25 seconds - MUCH FASTER
      summary(as.factor(df$newvar))
      
      
#use compiler & apply instead of for loop
      library(compiler)
      df<-d1
      a1<-Sys.time()
      
      GFHfunc<- function(x) {
        ifelse((x[1]+x[2]+x[3]+x["col4"])>4,"lgr4","smlr4")
      }
      compGFHfunc<-cmpfun(GFHfunc)
      
      newvar<-apply(df[,c(1:4)],1,FUN=compGFHfunc)
      
      df$newvar<-newvar
      
      a2<-Sys.time()
      a1-a2  #Same speed as not complied in byte code
      summary(as.factor(df$newvar))
      
      
      
  #Take Condition outside of loop 
      
      df<-d1
      a1<-Sys.time()
      condition<-((df[1]+df[2]+df[3]+df["col4"])>4)
      newvar<-character(nrow(df))
      
      for (i in 1:nrow(df)){
        if (condition[i])
        {
          newvar[i]<-"lgr 4"
        } else {
          newvar[i]<-"smlr 4"
        }
        df$newvar<-newvar
      }
      a2<-Sys.time()
      a1-a2  #this takes 1.2 seconds very fast for loop  
      summary(as.factor(df$newvar))
      
  #Use ifelse
      
      df<-d1
      
      a1<-Sys.time()
      condition<-((df[1]+df[2]+df[3]+df["col4"])>4)
      newvar<-character(nrow(df))
      
      for (i in 1:nrow(df)){
        newvar[i]<-ifelse(condition[i],"lgr4","smlr4")
           }
        df$newvar<-newvar
      
      a2<-Sys.time()
      a1-a2  #this takes .08 seconds very fast for loop  
      summary(as.factor(df$newvar))
   
         
  #Also consider using which statement like:
      
      df<-d1
      a1<-Sys.time()
      newvar<-rep("smlr4",times=nrow(df))
      # df["newvar"]<-"smlr4"
      more<-which(rowSums(df)>4)
      newvar[more]="lgr4"
      df$newvar<-newvar
      a2<-Sys.time()
      a1-a2  #this takes .03 seconds the fastest yet 
      summary(as.factor(df$newvar))
      
      
      
