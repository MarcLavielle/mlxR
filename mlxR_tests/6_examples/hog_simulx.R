setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

ton <- list(amount=1, rate=1, time=c(35, 65, 95, 125, 155, 185, 215, 245, 280, 
                                   341, 371, 401, 449, 479, 533, 563, 649, 683))
toff <- list(amount=-1, rate=0.25, time=c(43, 73, 103, 133, 163, 193, 223, 253, 285, 
                                        349, 379, 409, 454, 486, 541, 570, 657, 691))
tr <- list(ton, toff)

out <- list(name=c('h','s','u','x6'), time=seq(0, 800, by=0.5))

p <- list(name=c('c1','c2','c3','c4','c5','c6','c7','c8','x30','tau'), 
          value=c(75,2500,0.00002,0.04,1.2,800,0.005,0.1,150,10))

res <- simulx(model='hog_model.txt', parameter=p, output=out, treatment=tr)


plot1=ggplotmlx() + geom_line(data=res$h, aes(x=time, y=h), color="red") + 
geom_line(data=res$s,  aes(x=time, y=s), color="green") + 
geom_line(data=res$u,  aes(x=time, y=u), color="blue") 
plot2=ggplotmlx(data=res$x6, aes(x=time, y=x6))+ geom_line()

grid.arrange(plot1, plot2, ncol=2)


