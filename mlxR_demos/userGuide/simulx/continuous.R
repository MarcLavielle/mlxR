setwd(dirname(parent.frame(2)$ofile))
library(mlxR)
# source("../../../initMlxR220.R")
library(reshape)
library(gridExtra)

#-------------------------------------


p <- c(ka=0.5, 
       V=10, 
       k=0.2, 
       a=0.3, 
       b=0.1)

f <- list(name=c('f1','f2'), time=seq(0, 30, by=0.1))

y1 <- list(name='y1', time=seq(0, 30, by=2))
y2 <- list(name='y2', time=seq(1, 30, by=3))

res <- simulx(model     = 'model/continuous.txt', 
              parameter = p, 
              output    = list(f, y1, y2))


plot1=ggplotmlx(data=res$f1, aes(x=time, y=f1)) + 
  geom_line(size=0.5) +
  geom_point(data=res$y1, aes(x=time, y=y1), colour="red") 

plot2=ggplotmlx(data=res$f2, aes(x=time, y=f2)) + 
  geom_line(size=0.5) +
  geom_point(data=res$y2, aes(x=time, y=y2), colour="red")

grid.arrange(plot1, plot2, ncol=2)
