setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm1 <- list(time=seq(0,to=60,by=6), amount=50)
adm2 <- list(time=seq(0,to=60,by=12), amount=100)
Cc <- list(name='Cc', time=seq(0, 100, by=1))
p <- list(name=c('V','k'), value=c(10,0.2))
g1 <- list(treatment=adm1);
g2 <- list(treatment=adm2);
g <- list(g1,g2)
res <- simulx(model='group4_model.txt',parameter=p,output=Cc,group=g)

plot1 <- ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1)  
print(plot1)
