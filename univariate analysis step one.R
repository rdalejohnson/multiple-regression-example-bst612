###Step 1:  Univariate Analyses:  
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



#install.packages("rms")
library(rms)
library(knitr)
table=Hmisc::describe(lab)
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

