############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################
############################### STEP ONE: CONDUCT UNIVARIATE ANALYSES #####################################

library(summarytools)
library(ggplot2)
library(ggpubr)


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
qqnorm(lab$age)
qqline(lab$age)
age.shapiro
ggqqplot(lab$age, ylab = "Age")

ggplot(data = lab, mapping = aes(x = 'cut', y = lab$age)) +
  geom_boxplot() +
  geom_jitter( position=position_jitter(0.08))


#Is Sleep Duration normally distributed?
sleep_duration.shapiro <- shapiro.test(lab$sleep_duration)
qqnorm(lab$sleep_duration);qqline(lab$sleep_duration)
sleep_duration.shapiro
ggplot(data = lab,  mapping = aes(x = 'cut', y = lab$sleep_duration)) +
  geom_boxplot() + coord_flip() 
  #geom_jitter( position=position_jitter(0.3))

#Is BMI normally distributed?
bmi.shapiro <- shapiro.test(lab$bmi)
qqnorm(lab$bmi);qqline(lab$bmi)
bmi.shapiro
ggplot(data = lab, mapping = aes(x = 'cut', y = lab$bmi)) +
  geom_boxplot() +
  geom_jitter( position=position_jitter(0.08))

#https://stackoverflow.com/questions/27839432/how-to-generate-bin-frequency-table-in-r
br = seq(0,1,by=0.1)

ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(lab, breaks=br, include.lowest=TRUE, plot=FALSE)

data.frame(range = ranges, frequency = freq$counts)

######################################################################
# Categorical Variables will get counts from summary  
######################################################################

summary(lab)

#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************
#################################### END OF STEP ONE: UNIVARIATE ANALYSIS *********************************************



#################################### REMOVE ROWS WITH INVALID DATA ############################################
#################################### REMOVE ROWS WITH INVALID DATA ############################################
#################################### REMOVE ROWS WITH INVALID DATA ############################################
#################################### REMOVE ROWS WITH INVALID DATA ############################################
#################################### REMOVE ROWS WITH INVALID DATA ############################################


#lab <- lab[-which(is.na(lab$sleep_quality) | is.na(lab$sleep_duration) |  lab$sleep_duration >= 30 ), ]

#summary(lab)

lab$sleep_duration[lab$sleep_duration == 30] <- NA

sleep_duration.shapiro <- shapiro.test(lab$sleep_duration)
qqnorm(lab$sleep_duration);qqline(lab$sleep_duration)
sleep_duration.shapiro
ggplot(data = lab, mapping = aes(x = 'cut', y = lab$sleep_duration)) +
  geom_boxplot() +
  geom_jitter( position=position_jitter(0.08))


######################### SIDE EXPERIMENT WITH categorical and continuous ###############################
#Sources:  fastDummies: https://www.marsja.se/create-dummy-variables-in-r/
#categorial-continuous investigation/correlation: https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618

library(fastDummies)

labDummiedUp <- dummy_cols(lab, select_columns = c('race_ethnicity', 'sleep_quality'))

boxplot(bmi ~ race_ethnicity, data = labDummiedUp, ylab = "Race and BMI")

boxplot(age ~ race_ethnicity, data = labDummiedUp, ylab = "Age and Race")

boxplot(age ~ sleep_quality, data = labDummiedUp, ylab = "Age and Sleep Quality")

boxplot(bmi ~ sleep_quality, data = labDummiedUp, ylab = "Sleep Quality and BMI")

bmi.sleep.model = lm(formula = bmi ~ sleep_quality, data = labDummiedUp)
summary(bmi.sleep.model)

print(bmi.sleep.model$fitted)

bmi.race.model = lm(formula = bmi ~ race_ethnicity, data = lab)
summary(bmi.race.model)

bmi.age.model = lm(formula = bmi ~ age, data = lab)
summary(bmi.age.model)

bmi.sleep.duration.model = lm(formula = bmi ~ sleep_duration, data = lab)
summary(bmi.sleep.duration.model)

bmi.sleep.duration.model2 = lm(formula = sleep_duration ~ bmi , data = lab)
summary(bmi.sleep.duration.model2)

########################################################################################################


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

ggs = ggscatter(lab, x = "age", y = "sleep_duration", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Sleep Duration") 
ggs


###AGE BY BMI
###CONTINUOUS BY CONTINUOUS


scatterplot(age ~ bmi, data = lab)
plot(x=lab$bmi,y=lab$age)
Hmisc::rcorr(x=lab$bmi,y=lab$age, type=c("spearman"))
Hmisc::rcorr(x=lab$bmi,y=lab$age, type=c("pearson"))

cor(lab$age, lab$bmi, use = "complete.obs", method = "pearson")

cor.test(lab$age, lab$bmi)



###SLEEP DURATION BY BMI
###CONTINUOUS BY CONTINUOUS


scatterplot(sleep_duration ~ bmi, data = lab)
plot(x=lab$bmi,y=lab$sleep_duration)
Hmisc::rcorr(x=lab$bmi,y=lab$sleep_duration, type=c("spearman"))
Hmisc::rcorr(x=lab$bmi,y=lab$sleep_duration, type=c("pearson"))

#complete.obs means only complete rows, ignore NA values
cor(lab$bmi, lab$sleep_duration, use = "complete.obs", method = "pearson")

cor.test(lab$bmi, lab$sleep_duration)

ggs2 = ggscatter(lab, x = "bmi", y = "sleep_duration", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "BMI", ylab = "Sleep Duration") 
ggs2



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

#factor(lab1$raceRL)
#factor(lab1$bad.sleep.quality)
#factor(lab1$race_ethnicity)


##freq(lab1)

#mod1=ols(bmi~sleep_duration+age+raceRL+bad.sleep.quality+ok.sleep.quality,data=lab1)
mod=ols   (bmi~sleep_duration+age+raceRL+bad.sleep.quality+ok.sleep.quality,data=lab1)

mod.lms=lm(bmi~sleep_duration+age+raceRL+bad.sleep.quality+ok.sleep.quality,data=lab1)


mod
mod.lms
summary(mod.lms)

confint(mod)


Anova.mod <- anova(mod)

Anova.mod

Anova.modlms <- anova(mod.lms)

Anova.modlms

Corrected_Total=Anova.mod[6,"d.f."] + Anova.mod[7,"d.f."]

Corrected_Total

library(broom)
glance(mod) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

glance(mod) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)








summary(mod)
summary(mod.lms)

##https://web.stanford.edu/class/stats191/notebooks/Diagnostics_for_multiple_regression.html

#par(mfrow=c(2,2))
plot(mod, pch=23 ,bg='orange',cex=2)

plot(resid(mod), rstudent(mod), pch=23, bg='blue', cex=3)
plot(rstandard(mod), rstudent(mod), pch=23, bg='purple', cex=3)
qqnorm(rstandard(mod), pch=23, bg='red', cex=2)

plot(dffits(mod), pch=23, bg='orange', cex=2, ylab="DFFITS")

lab1[which(dffits(mod) > 1),]

plot(cooks.distance(mod), pch=23, bg='orange', cex=2, ylab="Cook's distance")
plot(hatvalues(mod), pch=23, bg='orange', cex=1, ylab='Hat values')
lab1[which(hatvalues(mod) > 0.3),]

plot(hatvalues(mod), rstandard(mod), pch=23, bg='red', cex=2)

plot(dfbetas(mod)[,'sleep_duration'], pch=23, bg='orange', cex=2, ylab="DFBETA (sleep_duration)")
dfbetas.sleep.duration <- lab1[which(abs(dfbetas(mod)[,'sleep_duration']) > 1),]
dfbetas.sleep.duration 

plot(dfbetas(mod)[,'age'], pch=23, bg='orange', cex=2, ylab="DFBETA (age)")
dfbetas.age <- lab1[which(abs(dfbetas(mod)[,'age']) > 1),]
dfbetas.age

#plot(trunc(lab1$bmi))
plot(mod)

mod.predicted <- predict(mod)   # Save the predicted values
mod.residuals <- residuals(mod) # Save the residual values



#mod1
mod

#Anova.mod=anova(mod1)

Anova.mod <- anova(mod)

Anova.mod

Anova.modlms <- anova(mod.lms)

Anova.modlms

Corrected_Total=Anova.mod[6,"d.f."] + Anova.mod[7,"d.f."]

Corrected_Total

library(broom)
glance(mod) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

glance(mod.lms) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)


######################################
# How do I get standardized betas's ##
######################################

#install.packages("lm.beta")
library(lm.beta)
lm.beta(mod)
lm.beta(mod.lms)

AIC(mod)
BIC(mod)
vif(mod)
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

#leverages(mod) # will not run
plot(resid(mod))
#mod1
confint(mod)
sum( rstudent(mod) <= (-2) | rstudent(mod) >= 2 )

ll=rstudent(mod) <= -2
RR=rstudent(mod) >= 2  
sum(ll | RR)
#How do you get PRESS
#install.packages("qpcR")
library(qpcR)

#res0 <- PRESS(mod)
res1 <- PRESS(mod)
barplot(res1$residuals)

plot(mod)
h=hatvalues(mod)
Leverage=h/(1-h)

sum(Leverage> 0.0092)


residuals(mod)






# Fit the model

lab3 <- lab[-which(is.na(lab$sleep_quality) | is.na(lab$sleep_duration) |  lab$sleep_duration >= 30 ), ]

#which(! complete.cases(x))


## https://www.r-bloggers.com/visualising-residuals/


fit <- lm(bmi~sleep_duration+age+race_ethnicity+bad.sleep.quality+ok.sleep.quality,data=lab3)

fit

# Obtain predicted and residual values
lab3$predicted = predict(fit)
lab3$residuals <- residuals(fit)

library(tidyr)

# Create plot
lab3 %>% 
  gather(key = "iv", value = "x", -bmi, -predicted, -residuals) %>%
  ggplot(aes(x = x, y = bmi)) +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +
  theme_bw()


ggqqplot(lab3$residuals, ylab = "Residuals")

