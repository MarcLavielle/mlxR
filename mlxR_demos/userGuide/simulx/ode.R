setwd(dirname(parent.frame(2)$ofile))
library(mlxR)
library(gridExtra)

#-------------------------------------

p <- c(a  = 0.07,  
       b  = 0.1,
       c  = 0.5) 

out <- list(name=c('f1', 'f2'), time=-5:100)

res <- simulx(model     = 'model/ode.txt', 
              parameter = p, 
              output    = out)

plot1=ggplotmlx(data=res$f1, aes(x=time, y=f1)) + geom_line(colour='blue',size=0.75)  
plot2=ggplotmlx(data=res$f2, aes(x=time, y=f2)) + geom_line(colour='red', size=0.75) 
grid.arrange(plot1, plot2, ncol=2)

