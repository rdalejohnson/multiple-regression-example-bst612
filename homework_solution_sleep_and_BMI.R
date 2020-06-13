library(summarytools)

lab=read.csv("BMI_Sleep.csv",na.strings=c("NA",""))
names(lab)
lab=lab[,c(2,3,4,5,6)]
names(lab)

colnames(lab)=c("age","race_ethnicity", "sleep_quality", "sleep_duration", "bmi")
names(lab)

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


summary(lab)



  

by(lab, lab$SLEEP.DURATION, summary)

by(lab, lab$AGE, summary)

zz <- plot(lab$SLEEP.DURATION, lab$AGE, main="Sleep Duration and Age",
     xlab="Sleep Duration ", ylab="Age")
zz

hist(lab$SLEEP.DURATION, lab$age)



library(ggplot2)
sp <- ggplot(lab, aes(x=SLEEP.DURATION  , y=AGE)) + geom_point(shape=1)
sp

sp + facet_grid(RACE ~ .)



hp <- ggplot(lab, aes(x=sleep_duration)) + geom_histogram(binwidth=1,colour="white")
hp

hp + facet_grid(RACE ~ SLEEP.QUALITY.RATING, scales="free_y")


