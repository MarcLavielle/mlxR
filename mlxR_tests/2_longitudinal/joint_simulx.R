setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

a <- list(time=seq(from=2,to=120,by=12),amount=10)
p <- list(name=c('ka','V','Cl','u','v','b'), value=c(0.5,8,1.5,0.002,2,0.05))
f <- list(name=c('Cc', 'h'), time=seq(from=0,to=100,by=1))
c <- list(name='Concentration', time=seq(from=5,to=100,by=10))
e <- list(name='Hemorrhaging', time=0)
out <- list(f, c, e)
g <- list(size=20, level='longitudinal')

res <- simulx(model='joint_model.txt',treatment=a,parameter=p,output=out,group=g)

plot1 = ggplotmlx() + geom_line(data=res$Cc, aes(x=time, y=Cc, group=id)) +
        geom_point(data=res$Concentration, aes(x=time, y=Concentration),colour="red") 
plot2 = ggplotmlx() + geom_line(data=res$h, aes(x=time, y=h, group=id)) +
        ylab("hazard")  + theme(legend.position="none")
h1=res$Hemorrhaging[res$Hemorrhaging[,3]>0 ,]
plot3 = ggplotmlx()+geom_point(data=h1, aes(x=time,y=id), size=3) + 
        xlab("event time") + ylab("id")  + theme(legend.position="none")
grid.arrange(plot1, plot2, plot3, ncol=3)
