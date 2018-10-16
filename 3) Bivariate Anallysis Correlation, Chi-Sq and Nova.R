# d1<-tbl_df(Prestige)
# sapply(d1,mean)
# sapply(d1,sd)
# summary(d1)
# #describe from Hmisc package
# library(Hmisc)
# describe(d1)
# library(pastecs)
# stat.desc(d1)


library(dplyr)
library(Hmisc)
library(pastecs)
library(MASS)
library(car)
library(carData)
data("Prestige")


#THREE TYPES OF CASES
  #Case1 - Continous vrs Continuos
  #Case2 - Continous vrs Categorical
  #Case3 - Categorical vrs Categorical



#Case1 - Continous vrs Continuos
  #Use Correlation (correlation is not causation)
    #Cor near 1 is strong +ve relationship
    #Cor near -1 is strong -ve relationship
    #Cor near 0 is weak or no relationship

cor(Prestige$income,Prestige$education) #answer: .5775802
cor.test(Prestige$income,Prestige$education) #low P-value indicates statistical significance
plot(y=Prestige$income,x=Prestige$education)
z<-lm(income~education,data = Prestige)
abline(z,col="blue")
d1<-tbl_df(Prestige)



#Case2 - Continous vrs Categorical
  #Use ANOVA
    #H0 - all populatiion means are equal
    #H1 - at least one population mean is different
        #F-Ratio = Mean Between Groups Sum of Squares divided by Mean within Groups Sum of Squares
            #In Anova "type" Mean Sq is between groups and "Residuals" is within groups
            #Therefore higher F_Ratio(and low P-val) means reject H0

#ex - boxplot shows means are different
boxplot(income~type,d1,
        main="Income vs Occupation",
        ylab="Income")

#ex - anova shows high F-Value and low P-val - thus reject HO
fit<-aov(income~type,d1)
summary(fit)



#Case3 - Categorical vrs Categorical
  #Use Chi-Sq 
    #chi^2=Chi-Squared Stat
    #Oi=number of observations of type i
    #Ei=the expected Frequency of type i
    #n=number of cells in the table

#Add a variable to d1 called "income_cat" with four categories using dplyr's ntile function
d1$income_cat<-ntile(d1$income,4)
table(d1$income_cat,d1$type)
chisq.test(y=d1$income_cat, x=d1$type)
  #low P-val & high chi^2 means reject H0


#CHALLENGE: determine which var in Prestige (d1) are stat sig to "income" var

  #case1 variables (edu,women,prestige, census)
eduCor<-cor.test(d1$income,d1$education)
womenCor<-cor.test(d1$income,d1$women)
prestigeCor<-cor.test(d1$income,d1$prestige)
censusCor<-cor.test(d1$income,d1$census)

eduCor$p.value
womenCor$p.value
prestigeCor$p.value
censusCor$p.value

  #case2 variable (type)
fit<-aov(income~type,d1)
summary(fit)


