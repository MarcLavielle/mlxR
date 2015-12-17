setwd(this.dir <- dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list( time = 2, amount = 40)
p <- list( name   = c('ka', 'V', 'Cl'),
           header = c('id', 'ka', 'V', 'Cl'),
           value  = matrix(ncol=4,nrow=3,byrow=TRUE,
                           data=c(  1, 0.5, 4,   1,
                                    2,   1, 6,   1,
                                    3, 1.5, 6, 1.5)))
t <- seq(0, 30, by=0.1)

res <- pkmodel(t,adm,p)

pl=ggplotmlx(data=res, aes(x=time, y=cc, colour=id)) + geom_line(size=1) +
  xlab("time (h)") + ylab("concentration (mg/L)")
print(pl)



