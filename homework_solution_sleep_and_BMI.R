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

#complete.obs means only complete rows, ignore NA values
cor(lab$age, lab$sleep_duration, use = "complete.obs", method = "pearson")

cor.test(lab$age, lab$sleep_duration)






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
var.test(lab$age ~ lab$race_ethnicity)

#unequal variances t-test
t.test(lab$age ~ lab$race_ethnicity, var.equal=F)

#equal variances t-test
t.test(lab$age ~ lab$race_ethnicity, var.equal=T)



###AGE BY BMI
###CONTINUOUS BY CONTINUOUS


scatterplot(age ~ bmi, data = lab)
plot(x=lab$bmi,y=lab$age)
Hmisc::rcorr(x=lab$bmi,y=lab$age, type=c("spearman"))
Hmisc::rcorr(x=lab$bmi,y=lab$age, type=c("pearson"))




###RACE BY BMI
###CONTINUOUS BY CATEGORICAL

#Levene's test for equality of variance
var.test(lab$bmi ~ lab$race_ethnicity)

#unequal variances t-test
t.test(lab$bmi ~ lab$race_ethnicity, var.equal=F)

#equal variances t-test
t.test(lab$bmi ~ lab$race_ethnicity, var.equal=T)



###RACE BY GOOD QUALITY SLEEP
###CATEGORICAL BY CATEGORICAL
###CHI SQUARE

library(MASS)

table.race.by.good.sleep = table(lab$race_ethnicity, lab$good.sleep.quality)
Xsq <- chisq.test(table.race.by.good.sleep)
library(vcd)
assocstats(table.race.by.good.sleep)
Xsq$expected


###RACE BY OK QUALITY SLEEP
###CATEGORICAL BY CATEGORICAL
###CHI SQUARE

table.race.by.ok.sleep = table(lab$race_ethnicity, lab$ok.sleep.quality)
Xsq <- chisq.test(table.race.by.ok.sleep)
library(vcd)
assocstats(table.race.by.ok.sleep)
Xsq$expected


###RACE BY BAD QUALITY SLEEP
###CATEGORICAL BY CATEGORICAL
###CHI SQUARE

table.race.by.bad.sleep = table(lab$race_ethnicity, lab$bad.sleep.quality)
Xsq <- chisq.test(table.race.by.bad.sleep)
library(vcd)
assocstats(table.race.by.bad.sleep)
Xsq$expected
                       





###BMI BY SLEEP QUALITY    ##### BAD SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$bmi ~ lab$bad.sleep.quality)

#unequal variances t-test
t.test(lab$bmi ~ lab$bad.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$bmi ~ lab$bad.sleep.quality, var.equal=T)


###BMI BY SLEEP QUALITY    ##### GOOD SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$bmi ~ lab$good.sleep.quality)

#unequal variances t-test
t.test(lab$bmi ~ lab$good.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$bmi ~ lab$good.sleep.quality, var.equal=T)


###BMI BY SLEEP QUALITY    ##### OK SLEEP #######
###CONTINUOUS BY CATEGORICAL
#Levene's test for equality of variance
var.test(lab$bmi ~ lab$ok.sleep.quality)

#unequal variances t-test
t.test(lab$bmi ~ lab$ok.sleep.quality, var.equal=F)

#equal variances t-test
t.test(lab$bmi ~ lab$ok.sleep.quality, var.equal=T)


#####################  MLS REGRESSION ####################
#####################  MLS REGRESSION ####################
#####################  MLS REGRESSION ####################
#####################  MLS REGRESSION ####################


lab1=within(lab, {
  raceRL= relevel(race_ethnicity,ref="White, Not Hispanic")
  #badsleepRL=relevel(bad.sleep.quality,ref="Not Bad")
  #oksleepRL=relevel(ok.sleep.quality,ref="Not OK")
})

factor(lab1$raceRL)
factor(lab1$bad.sleep.quality)
factor(lab1$race_ethnicity)




mod1=ols(bmi~sleep_duration+age+raceRL+bad.sleep.quality+ok.sleep.quality,data=lab1)
mod=lm(bmi~sleep_duration+age+raceRL+bad.sleep.quality+ok.sleep.quality,data=lab1)

mod1

Anova.mod=anova(mod1)

Anova.mod

Corrected_Total=Anova.mod[6,"d.f."] + Anova.mod[7,"d.f."]

Corrected_Total


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
sum( rstudent(mod) <= (-2) | rstudent(mod) >= 2 )

ll=rstudent(mod) <= -2
RR=rstudent(mod) >= 2  
sum(ll | RR)
#How do you get PRESS
#install.packages("qpcR")
library(qpcR)

PRESS(mod)

plot(mod)
h=hatvalues(mod)
Leverage=h/(1-h)

sum(Leverage> 0.0092)


residuals(mod1)

######################################
# How do I get standardized betas's ##
######################################

#install.packages("lm.beta")
library(lm.beta)
lm.beta(mod)

