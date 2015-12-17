setwd(dirname(parent.frame(2)$ofile))
library(survival)

#-------------------------------------

p <- list(name=c('beta','lambda'), value=c(2.5,50))
h <- list(name='h', time=seq(0, 30, by=0.1))
e <- list(name='e', time=0)
o <- list(h, e);
g <- list(size=100, level='longitudinal')
res1 <- simulx(model='event1_model.txt', parameter=p, output=o, group=g)

event=res1$e[res1$e[,2]>0,]
my.surv <- Surv(time=event$time, event=event$e)
my.fit <- survfit(formula = my.surv ~1)
plot(my.fit, main="Kaplan-Meier estimate with 95% confidence bounds",xlab="time", ylab="survival function")

