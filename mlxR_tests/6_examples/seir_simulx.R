setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

p <- list(name=c('A','lambda','gamma','epsilon','d','omega','tau'), 
          value=c(0.5, 0.4, 0.02, 0.1, 0.01, 0.3, 60))

out <- list(name=c('S', 'E', 'I', 'R'), time=-20:350)

res <- simulx(model='seir_model.txt', parameter=p, output=out)

plot1=ggplotmlx(data=res$S, aes(x=time, y=S)) + geom_line()  
plot2=ggplotmlx(data=res$E, aes(x=time, y=E)) + geom_line() 
plot3=ggplotmlx(data=res$I, aes(x=time, y=I)) + geom_line() 
plot4=ggplotmlx(data=res$R, aes(x=time, y=R)) + geom_line() 
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

