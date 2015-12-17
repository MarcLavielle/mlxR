setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=seq(0,to=60,by=6), amount=50)
Cc <- list(name='Cc', time=seq(0, 100, by=1))
p <- list(name=c('V'), value=c(100))
p1 <- list(name=c('k'), value=c(0.1))
p2 <- list(name=c('k'), value=c(1))
#p1 <- list(name=c('V','k'), value=c(10,0.2))
#p2 <- list(name=c('V','k'), value=c(15,0.1))
g1 <- list(parameter=p1);
g2 <- list(parameter=p2);
g <- list(g1,g2)
#res <- simulx(model='group4_model.txt',treatment=adm,output=Cc,group=g)
res <- simulx(model='group4_model.txt',treatment=adm,output=Cc,group=g,parameter=p)

plot1 <- ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1)  
print(plot1)
