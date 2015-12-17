setwd(this.dir <- dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

adm <- list(time=seq(2, 100, by=24), amount=40, rate=5)
p <- list(name=c('V','Cl','k12','k21','ke0'), value=c(8,0.5,0.3,0.2,0.5))
t <- seq(0, 50, by=0.1)

res <- pkmodel(t,adm,p)

pl1=ggplotmlx(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
  xlab("time (h)") + ylab("concentration (mg/L)") + ggtitle("central compartment")
pl2=ggplotmlx(data=res, aes(x=time, y=ce)) + geom_line(size=1) +
  xlab("time (h)") + ylab("concentration (mg/L)") + ggtitle("effect compartment")
grid.arrange(pl1, pl2, ncol=2)
