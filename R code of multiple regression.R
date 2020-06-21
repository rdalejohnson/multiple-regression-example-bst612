###########################################
#Title: Multiple Regression for BST 612 ###
# Date: Feb 28, 201                     ###
##########################################

  
  
#####Research Question:  Is there a relationship among a patient's gender, race, and age in predicting their aldosterone level? 
#####Procedure:
  
#Step 1:  Conduct Univariate Analyses:  Descriptive Statistics and Cleaning the Data

#Step 2:  Conduct Bivariate Analyses

#Step 3:  Conduct the Multivariate Analyses

#Step 4:  Assess the Model Assumptions

#Step 5:  Examine Model Fit Diagnostics

#Step 6:  Examine Data for Influential Observations

#Step 7:  State a Conclusion

####Step 1:  Univariate Analyses:  
##### Descriptive Statistics and Cleaning the Data
#Read in dataset
lab=read.csv("Binge_Socializing.csv",na.strings=c("NA",""))
#lab=read.csv("C:\\Users\\Steve\\Downloads\\Binge_socializing.csv", na.strings=c("NA",""))
# Remember to include na.strings=c("NA","") as used above
############################################################################################################
#Take a snapshot of data
head(lab)
#notes names and position in data set
names(lab)
lab=lab[,c(1,3,5,6,7,8,9)]
#Renaming
colnames(lab)=c("age","gender","Black","White","Other","time_at_activity_socialize","Students_binge_in_College")



install.packages("rms")
library(rms)
library(knitr)
table=describe(lab)
table


#OR you can use summary function
summary(lab)

##### Age:  Mean and median are very similar at 21 years of age (21.16 for the mean and 21.00 for the median) and ranges from 17 to 26 years of age.  Based on the frequency table, there does not appear to be any outliers.  The histogram shows a normal curve; however, the spike in older students may inflate the mean.


age.freq=table(lab$age)
age.prop=prop.table(age.freq)
age.table=cbind(age.freq,age.prop)
colnames(age.table)=c("Frequency","Percent")

round(age.table, 3)

library(ggplot2)
n=1394
mean=21.16
sd =2.4
binwidth=1

ggplot(data=lab, aes(lab$age)) + 
  geom_histogram( breaks = seq(14, 27, binwidth),col="black",fill="green",alpha = 0.2) +
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")+
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd) * n * bw
    },args = c(mean = mean, sd = sd, n = n, bw = binwidth))

# Gender

gender.freq=table(lab$gender)
gender.prop=prop.table(gender.freq)
gender.table=as.data.frame(cbind(gender.freq,gender.prop))
colnames(gender.table)=c("Frequency","Fraction")



#Show results for gender; we know there are 6 missings
gender.table

Black.freq=table(lab$Black)
Black.prop=prop.table(Black.freq)
Black.table=as.data.frame(cbind(Black.freq,Black.prop))
colnames(Black.table)=c("Frequency","Fraction")

#show results for back or others
Black.table


White.freq=table(lab$White)
White.prop=prop.table(White.freq)
White.table=as.data.frame(cbind(White.freq,White.prop))
colnames(White.table)=c("Frequency","Fraction")

#Show results for whites and others
White.table

Other.freq=table(lab$Other)
Other.prop=prop.table(Other.freq)
Other.table=as.data.frame(cbind(Other.freq,Other.prop))
colnames(Other.table)=c("Frequency","Fraction")

#Show results for  others and Black/White
Other.table

library(knitr)
kable(gender.table,caption = "Gender Recoded",digits = 3)


##### Gender:  There are more females (57.9%).



library(knitr)
kable(Black.table,caption = "Black",digits = 5)

kable(White.table,caption = "White",digits = 5)

kable(Other.table,caption = "Other",digits = 5)


##### The majority are White (79.6%), followed by Other (12.8%), and then Black (7.7%).


time.freq=table(lab$time_at_activity_socialize)
time.prop=prop.table(time.freq)
time.table=as.data.frame(cbind(time.freq,time.prop))
colnames(time.table)=c("Frequency","Fraction")



library(knitr)
kable(time.table,caption = "time at activity:socialize",digits = 5)
library(ggplot2)

n=1379
mean=2.95
sd =2.06
binwidth=1

ggplot(data=lab, aes(lab$time_at_activity_socialize)) + 
  geom_histogram( breaks = seq(-2, 10, binwidth),col="black",fill="green",alpha = 0.2) +
  labs(title="Histogram for time at activity:socialize") +
  labs(x="time at activity:socialize", y="Count")+
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd) * n * bw
    },args = c(mean = mean, sd = sd, n = n, bw = binwidth))



##### Socializing: Looks a little skewed, the mean is 2.95 compared to the median of 2 (from statistics output on 1st page).  The minimum is 0 and maximum is 8 and there are no outliers.  There does not appear to be any data entry errors.


binge.freq=table(lab$Students_binge_in_College)
binge.prop=prop.table(binge.freq)
binge.table=as.data.frame(cbind(binge.freq,binge.prop))
colnames(binge.table)=c("Frequency","Percent")



library(knitr)
kable(binge.table,caption = "Students binge in College",digits = 5)


#####Binge Drinking:  The majority never binged (58.7%)
#####Sample size: N=1400 and 60 missing data or 4.3%


#### Step 2:  Conduct Bivariate Analyses

##### Students binge in College by gender, Black, White & Other


table1= table(lab$Students_binge_in_College,lab$gender)
freq.table1=cbind(table1,margin.table(table1, 1))
freq.table1=rbind(freq.table1,c(margin.table(table1, 2), sum(table1)))


colnames(freq.table1)[3]<-"Total"
freq.table1

#Computing coulun percentages
 sweep(table1, MARGIN=2,STATS=colSums(table1),FUN="/")*100

 
 #Chi-square test
library(MASS)
chisq.test(table1,correct=F)
# phi coefficient
install.packages("sjstats")
library(sjstats)
phi(table1)

##### Gender is statistically significant (Pearson Chi-Square or Likelihood Ratio = p<0.0001).  
##### For Table 1, we want to use the % within gender recoded because we need to standardize the numbers across our dependent variable.  In this case, we are interested in the differences between Binging and Not Binging.
##### Gender:  Males report more binge drinking than females (48.3% compared to 36.3%). The phi indicates that this is a weak relationship (-0.121). 




table2= table(lab$Students_binge_in_College,lab$Black)
freq.table2=cbind(table2,margin.table(table2, 1))
freq.table2=rbind(freq.table2,c(margin.table(table2, 2),1323))

library(knitr)
kable(freq.table2,caption = "Students binge in College by Black",digits = 5)

#Computing coulun percentages
kable(sweep(table2, MARGIN=2,STATS=colSums(table2),FUN="/")*100)

library(MASS)
chisq.test(table2,correct=F)

# phi coefficient
phi(table2)

##### Race: Binge drinking by race indicates that 10.2% of Black students report binge drinking.  The phi indicates that this is a weak relationship 


table3= table(lab$Students_binge_in_College,lab$White)
freq.table3=cbind(table3,margin.table(table3, 1))
freq.table3=rbind(freq.table3,c(margin.table(table3, 2),1323))

library(knitr)
kable(freq.table3,caption = "Students binge in College by White",digits = 5)


#Computing coulun percentages
kable(sweep(table3, MARGIN=2,STATS=colSums(table3),FUN="/")*100)

library(MASS)
chisq.test(table3,correct=F)
# phi coefficient
phi(table3)

##### When we look at Whites compared to Black/Other Races, 48.6% of White students report binge drinking. This is a slightly stronger relationship with a phi of 0.211.


table4= table(lab$Students_binge_in_College,lab$Other)
freq.table4=cbind(table4,margin.table(table4, 1))
freq.table4=rbind(freq.table4,c(margin.table(table4, 2),1323))

library(knitr)
kable(freq.table4,caption = "Students binge in College by Other",digits = 5)

#Computing coulun percentages
kable(sweep(table4, MARGIN=2,STATS=colSums(table4),FUN="/")*100)

library(MASS)
chisq.test(table4,correct=F)
# phi coefficient
phi(table4)

##### When we look at Others compared to Black/White Races, 27.1%  of Other races report binge drinking. This is a slightly weaker relationship with a phi of 0..113.


Age=lab$age
Students_binge_in_College=lab$Students_binge_in_College

#Levenes test for equality of variance
var.test(Age~Students_binge_in_College)

#Unequal variance t test
t.test(Age~Students_binge_in_College, var.equal=F)

#Equal variance t test
t.test(Age~Students_binge_in_College, var.equal=T)


#subset for only Binge 1+X for ages
AgeBingeX=lab[lab$Students_binge_in_College=="Binge 1+ x", "age"] 
boxplot(AgeBingeX)
qqnorm(AgeBingeX);qqline(AgeBingeX)

#Normality test

shapiro.test(AgeBingeX)
ks.test(AgeBingeX, "pnorm")


#subset for only students binge in college =0
AgeBinge0=lab[lab$Students_binge_in_College=="Never binge", "age"] 
#AgeBinge0
boxplot(AgeBinge0)
qqnorm(AgeBinge0);qqline(AgeBinge0)

#Normality test

shapiro.test(AgeBinge0)
ks.test(AgeBinge0, "pnorm") # Results does not match results in SAS




##### With age, we see that the assumption for homogeneity of variance was violated as noted by the significant p-values for the Levine's Test.  
####  We need to report the statistics for the line "Equal Variances Not Assumed".  
##### We see that there is a significant difference between those who binge and those who don't.  


time_at_activity_socialize=lab$time_at_activity_socialize
Students_binge_in_College=lab$Students_binge_in_College

#Levenes test for equality of variance
var.test(time_at_activity_socialize~Students_binge_in_College)
var.test(lab$time_at_activity_socialize~lab$Students_binge_in_College)


#Unequal variance t test
t.test(time_at_activity_socialize~Students_binge_in_College, var.equal=F)

#Equal variance t test
t.test(time_at_activity_socialize~Students_binge_in_College, var.equal=T)


taasx=lab[lab$Students_binge_in_College=="Binge 1+ x", "time_at_activity_socialize"] 
boxplot(taasx)
qqnorm(taasx);qqline(taasx)

#Normality test

shapiro.test(taasx)
ks.test(taasx, "pnorm")


taas0=lab[lab$Students_binge_in_College=="Never binge", "time_at_activity_socialize"] 
boxplot(taas0)
qqnorm(taas0);qqline(taas0)

#Normality test

shapiro.test(taas0)
ks.test(taas0, "pnorm")




#Code for summary of variables to complete Table 1.
library(dplyr)
library(plyr)
options(digits=8)
#Summary for age
lab%>%select(Students_binge_in_College, age)%>%group_by(Students_binge_in_College)%>%
  summarise_all(funs(mean(.,na.rm=T), median(.,na.rm=T), std=sqrt(var(.,na.rm=T)),min(.,na.rm=T),
                     max(.,na.rm=T),N=n(),Missing=sum(is.na(.))))

#Summary for time at activity for socializing            
lab%>%select(Students_binge_in_College, time_at_activity_socialize)%>%group_by(Students_binge_in_College)%>%
  summarise_all(funs(mean(.,na.rm=T), median(.,na.rm=T), std=sqrt(var(.,na.rm=T)),min(.,na.rm=T),
                     max(.,na.rm=T),N=n(),Missing=sum(is.na(.))))




#Step 2 bivariate Analysis : Predictors Against Predictors
# Categorical by categorical


table5= table(lab$gender,lab$Black)
freq.table5=cbind(table5,margin.table(table5, 1))
freq.table5=rbind(freq.table5,c(margin.table(table5, 2), sum(table5)))

library(knitr)
kable(freq.table5,caption = "Table of gender by Black",digits = 5)
#Computing row percentages
kable(sweep(table5, MARGIN=1,STATS=rowSums(table5),FUN="/")*100)

library(MASS)
chisq.test(table5,correct=F) 
phi(table5)


##### Here we see that Gender is only significant with the racial category of Black vs White/Other.  The majority of black students were female (9.5%) compared to black males (5.2%). (Previous page).

##### Comparing X with X Variables (gender and White):
################### CHI SQUARE ##############################


table6= table(lab$gender,lab$White)
freq.table6=cbind(table6,margin.table(table6, 1))
freq.table6=rbind(freq.table6,c(margin.table(table6, 2), sum(table6)))

colnames(freq.table6)[3]<-"Totals"
rownames(freq.table6)[3]<-"Totals"

library(knitr)
kable(freq.table6,caption = "Table of gender by White",digits = 5)
#Computing row percentages
kable(sweep(table6, MARGIN=1,STATS=rowSums(table6),FUN="/")*100)

library(MASS)
chisq.test(table6,correc=F)

#phicoefficient
phi(table6)



#Gender and other



table7= table(lab$gender,lab$Other)
freq.table7=cbind(table7,margin.table(table7, 1))
freq.table7=rbind(freq.table7,c(margin.table(table7, 2), sum(table7)))

colnames(freq.table7)[3]<-"Totals"
rownames(freq.table7)[3]<-"Totals"

library(knitr)
kable(freq.table7,caption = "Table of gender by White",digits = 5)
#Computing row percentages
kable(sweep(table7, MARGIN=1,STATS=rowSums(table6),FUN="/")*100)

library(MASS)
chisq.test(table7,correc=F)
phi(table7)

#####################################################################
#Step 2 : Bivariate Analysis Predictors Against Predictors  #########
######################################################################
# Categorical by Continuous


Age=lab$age
Gender=lab$gender

#Levenes test for equality of variance
var.test(Age~Gender)

#Unequal variance t test
t.test(time_at_activity_socialize~Students_binge_in_College, var.equal=F)

#Equal variance t test
t.test(Age~Gender, var.equal=T)

############################
#Continuous by continuous  #
############################
library(rms)
with(lab, rcorr(x=age,y=time_at_activity_socialize))


#######################################################################################
#### Steps 3, 4 and 5 : Conduct the Multivariate Analysis, Assess the Assumptions,  ###
#### Examin model fit                                                               ### 
#######################################################################################


library(rms)

#install.packages("car")
library(car)

#################################################################
# Change reference level of categorical variable to your choice ##
#################################################################

lab1=within(lab, {
gender= relevel(gender,ref="Male")
Students_binge_Co=relevel(Students_binge_in_College,ref="Never binge")
Black=relevel(Black,ref="White or Other")
Other=relevel(Other,ref="White or Black")


})

#mod1 and mod2 are same from different R packages both are helpful in different ways 
# in extracting other statistics hence the fit of the two models

mod1=ols(time_at_activity_socialize~Students_binge_Co+age+gender+Black+Other,data=lab1)
mod=lm(time_at_activity_socialize~Students_binge_Co+age+gender+Black+Other,data=lab1)

mod1

anova(mod1)

Anova.mod=anova(mod1)

Anova.mod

Corrected_Total=Anova.mod[6,"d.f."] + Anova.mod[7,"d.f."]

Corrected_Total
 
  # R does not have a function that does lack of fit 
  AIC(mod1)
  BIC(mod)
  Tolerance=1/vif(mod)
  Tolerance
  
  CooksD=cooks.distance(mod)
  #CooksD[CooksD> 0.0031]
  #Count number > 0.0031
  # 61 were identified
  sum(CooksD>0.0031)
  
  #Hat values
  hatvalues(mod)
  
  plot(hatvalues(mod),type="h")
  plot(rstudent(mod),type="h")
  #Influence plot
  influencePlot(mod,main="Influence Plot",sub="Circle size is proportional to Cook's distance")
  plot(mod,which=1)
  plot(mod,which=2)
  plot(mod, which=3)
  plot(mod,which=4)
  plot(mod,which=5)
  plot(mod,which=6)
  
  
  library(MASS)
  
  #leverage(mod), will not run
  plot(resid(mod))
  mod1
  confint(mod1)
  sum( rstudent(mod) < (-2) | rstudent(mod) > 2 )
  
 ll=rstudent(mod) <= -2
 RR=rstudent(mod) >= 2  
  sum(ll | RR)
  #How do you get PRESS
  install.packages("qpcR")
  library(qpcR)
  
  PRESS(mod)
  
 plot(mod)
 h=hatvalues(mod)
 Leverage=h/(1-h)
 
 sum(Leverage> 0.0092)
 
 plot(hatvalues(mod), type = "h")
 
 
 residuals(mod1)
 
 ######################################
 # How do I get standardized betas's ##
 ######################################
 
 install.packages("lm.beta")
 library(lm.beta)
 lm.beta(mod)
 