############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################

library(summarytools)

lab=read.csv("BMI_Sleep.csv",na.strings=c("NA",""))

#######################################################################
# Removing the identifier column so it doesn't enter into any analyses
#######################################################################

names(lab)
lab=lab[,c(2,3,4,5,6)]
names(lab) 

colnames(lab)=c("age","race_ethnicity", "sleep_quality", "sleep_duration", "bmi")
names(lab)

######################################################################
# Continuous Variables   
######################################################################

numericals.summary <- summarytools::descr(lab, round.digits=4)

numericals.summary

#Is AGE normally distributed?
age.shapiro <- shapiro.test(lab$age)
qqnorm(lab$age);qqline(lab$age)
age.shapiro

#Is Sleep Duration normally distributed?
sleep_duration.shapiro <- shapiro.test(lab$sleep_duration)
qqnorm(lab$sleep_duration);qqline(lab$sleep_duration)
sleep_duration.shapiro

#Is BMI normally distributed?
bmi.shapiro <- shapiro.test(lab$bmi)
qqnorm(lab$bmi);qqline(lab$bmi)
bmi.shapiro


######################################################################
# Categorical Variables will get counts from summary  
######################################################################

summary(lab)



