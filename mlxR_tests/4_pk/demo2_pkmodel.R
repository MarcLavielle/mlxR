setwd(this.dir <- dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time = c(1,23,37,60), amount = c(1,0.5,2,0.3))

p <- list(name=c('Mtt', 'Ktr', 'ka', 'V', 'Vm', 'Km', 'p'), 
          value=c(5, 1, 0.5, 10, 1, 0.6, 0.5))
t <- seq(0, 80, by=0.1)

res <- pkmodel(t,adm,p)

pl=ggplotmlx(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
  xlab("time (h)") + ylab("concentration (mg/L)")
print(pl)
