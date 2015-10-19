setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)

#-------------------------------------

p <- c(a  = 0.08,  
       b  = 0.05, 
       tau1 = 5, 
       tau2 = 10) 

out <- list(name=c('f1', 'f2'), time=seq(-10,100,by=0.1))

res <- simulx(model     = 'model/dde.txt', 
              parameter = p, 
              output    = out)

plot1=ggplotmlx(data=res$f1, aes(x=time, y=f1)) + geom_line()  
plot2=ggplotmlx(data=res$f2, aes(x=time, y=f2)) + geom_line() 
grid.arrange(plot1, plot2, ncol=2)

