library(summarytools)


lab=read.csv("BMI_Sleep.csv",na.strings=c("NA",""))

summarytools::descr(lab)

summary(lab)


by(lab, lab$SLEEP.DURATION, summary)

by(lab, lab$AGE, summary)




hist(lab$SLEEP.DURATION, lab$age)



library(ggplot2)
sp <- ggplot(lab, aes(x=SLEEP.DURATION  , y=AGE)) + geom_point(shape=1)
sp

sp + facet_grid(RACE ~ .)



hp <- ggplot(lab, aes(x=SLEEP.DURATION)) + geom_histogram(binwidth=1,colour="white")
hp

hp + facet_grid(RACE ~ SLEEP.QUALITY.RATING, scales="free_y")


