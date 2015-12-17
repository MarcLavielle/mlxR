setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=seq(0,to=60,by=6), amount=50)
Cc1 <- list(name='Cc', time=seq(0, 70, by=1))
Cc2 <- list(name='Cc', time=seq(30, 100, by=0.5))
p <- list(name=c('V','k'), value=c(10,0.2))
g1 <- list(output=Cc1);
g2 <- list(output=Cc2);
g <- list(g1,g2)

res <- simulx(model='group4_model.txt',treatment=adm,parameter=p,group=g)

plot1 <- ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1)  
print(plot1)
