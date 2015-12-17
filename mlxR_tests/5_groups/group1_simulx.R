setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=0, amount=100)
y <- list(name='y', time=seq(0, 10, by=1))
p <- list(name=c('V','k','a'), value=c(10,0.2,0.5))
g <- list(size=5);
res <- simulx(model='group1_model.txt', parameter=p, output=y, treatment=adm, group=g)

plot1 <- ggplotmlx(data=res$y, aes(x=time, y=y, colour=id)) + geom_line(size=1) + geom_point() 
print(plot1)
