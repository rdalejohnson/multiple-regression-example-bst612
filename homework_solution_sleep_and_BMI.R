############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
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

#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************



##### REMOVE ROWS WITH INVALID DATA


#lab <- lab[-which(is.na(lab$sleep_quality) | is.na(lab$sleep_duration) |  lab$sleep_duration >= 30 ), ]

#summary(lab)

lab$sleep_duration[lab$sleep_duration == 30] <- NA


#################################### STEP TWO: BIVARIATE ANALYSES *********************************************
#################################### STEP TWO: BIVARIATE ANALYSES *********************************************
#################################### STEP TWO: BIVARIATE ANALYSES *********************************************
#################################### STEP TWO: BIVARIATE ANALYSES *********************************************
#################################### STEP TWO: BIVARIATE ANALYSES *********************************************

library(rms)
library(car)

###PRIMARY PREDICTOR BY ALL OTHER VARIABLES
###PRIMARY PREDICTOR: SLEEP DURATION

###############################   SLEEP DURATION BY AGE
###############################   CONTINUOUS BY CONTINUOUS

scatterplot(sleep_duration ~ age, data = lab)
plot(x=lab$age,y=lab$sleep_duration)
Hmisc::rcorr(x=lab$age,y=lab$sleep_duration, type=c("spearman"))
Hmisc::rcorr(x=lab$sleep_duration,y=lab$age, type=c("spearman"))

Hmisc::rcorr(x=lab$age,y=lab$sleep_duration, type=c("pearson"))








###SLEEP DURATION BY BMI
###CONTINUOUS BY CONTINUOUS


scatterplot(sleep_duration ~ bmi, data = lab)
plot(x=lab$bmi,y=lab$sleep_duration)
Hmisc::rcorr(x=lab$bmi,y=lab$sleep_duration, type=c("spearman"))
Hmisc::rcorr(x=lab$bmi,y=lab$sleep_duration, type=c("pearson"))





########################### SLEEP QUALITY BECOMES THREE DICHOTOMOUS COLUMNS (bad/not bad, good/not good, ok/not ok)
########################### SLEEP QUALITY BECOMES THREE DICHOTOMOUS COLUMNS (bad/not bad, good/not good, ok/not ok)
########################### SLEEP QUALITY BECOMES THREE DICHOTOMOUS COLUMNS (bad/not bad, good/not good, ok/not ok)
########################### SLEEP QUALITY BECOMES THREE DICHOTOMOUS COLUMNS (bad/not bad, good/not good, ok/not ok)


library (dplyr)

lab <- mutate(lab, bad.sleep.quality=ifelse(as.character(sleep_quality)=="Bad", "Bad", "Not Bad") )
lab <- mutate(lab, ok.sleep.quality=ifelse(as.character(sleep_quality)=="OK", "OK", "Not OK") )
lab <- mutate(lab, good.sleep.quality=ifelse(as.character(sleep_quality)=="Good", "Good", "Not Good") )





###SLEEP DURATION BY SLEEP QUALITY    ##### BAD SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$sleep_duration ~ lab$bad.sleep.quality)

#unequal variances t-test
t.test(lab$sleep_duration ~ lab$bad.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$sleep_duration ~ lab$bad.sleep.quality, var.equal=T)
     

###SLEEP DURATION BY SLEEP QUALITY    ##### GOOD SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$sleep_duration ~ lab$good.sleep.quality)

#unequal variances t-test
t.test(lab$sleep_duration ~ lab$good.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$sleep_duration ~ lab$good.sleep.quality, var.equal=T)


###SLEEP DURATION BY SLEEP QUALITY    ##### OK SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$sleep_duration ~ lab$ok.sleep.quality)

#unequal variances t-test
t.test(lab$sleep_duration ~ lab$ok.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$sleep_duration ~ lab$ok.sleep.quality, var.equal=T)








###AGE BY SLEEP QUALITY    ##### BAD SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$age ~ lab$bad.sleep.quality)

#unequal variances t-test
t.test(lab$age ~ lab$bad.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$age ~ lab$bad.sleep.quality, var.equal=T)


###AGE BY SLEEP QUALITY    ##### GOOD SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$age ~ lab$good.sleep.quality)

#unequal variances t-test
t.test(lab$age ~ lab$good.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$age ~ lab$good.sleep.quality, var.equal=T)


###AGE BY SLEEP QUALITY    ##### OK SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$age ~ lab$ok.sleep.quality)

#unequal variances t-test
t.test(lab$age ~ lab$ok.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$age ~ lab$ok.sleep.quality, var.equal=T)





###AGE BY RACE
###CONTINUOUS BY CATEGORICAL

#Levene's test for equality of variance
var.test(lab$age ~ lab$race)

#unequal variances t-test
t.test(lab$age ~ lab$race, var.equal=F)

#equal variances t-test
t.test(lab$age ~ lab$race, var.equal=T)



###AGE BY BMI
###CONTINUOUS BY CONTINUOUS


scatterplot(age ~ bmi, data = lab)
plot(x=lab$bmi,y=lab$age)
Hmisc::rcorr(x=lab$bmi,y=lab$age, type=c("spearman"))
Hmisc::rcorr(x=lab$bmi,y=lab$age, type=c("pearson"))




###RACE BY BMI
###CONTINUOUS BY CATEGORICAL

#Levene's test for equality of variance
var.test(lab$bmi ~ lab$race)

#unequal variances t-test
t.test(lab$bmi ~ lab$race, var.equal=F)

#equal variances t-test
t.test(lab$bmi ~ lab$race, var.equal=T)



###RACE BY GOOD QUALITY SLEEP
###CATEGORICAL BY CATEGORICAL

library(MASS)

table2by2 = table(lab$race, lab$good.sleep.quality)
freq.table2by2 = cbind(table2by2, margin.table(table2by2, 1))
freq.table2by2 = rbind(freq.table2by2, c(margin.table(table2by2, 2), 1355) )

chisq.test(table2by2, correct = F)
                       
