setwd(this.dir <- dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=c(2,14), amount=40)
p   <- list(name=c('V','Cl','k12','k21'), value=c(8,0.5,0.3,0.2))
t   <- seq(0, 30, by=0.1)

res   <- pkmodel(time=t,treatment=adm,parameter=p)

pl=ggplotmlx(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
  xlab("time (h)") + ylab("concentration (mg/L)")
print(pl)

