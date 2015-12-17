setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------


p  <- list(name=c('a','b'), value=c(8,0.2))
pr <- list(name=c('p1','p2','p3'), time=seq(0, 100, by=1))
y  <- list(name='y', time=seq(0, 100, by=4))

out <- list(pr, y);

res <- simulx(model='categorical_model.txt', parameter=p, output=out)

plot1=ggplotmlx() + ylab("probabilities") +
  geom_line(data=res$p1, aes(x=time, y=p1, colour="green")) +
  geom_line(data=res$p2, aes(x=time, y=p2, colour="blue")) +
  geom_line(data=res$p3, aes(x=time, y=p3, colour="red")) +
  scale_colour_manual(name="", values=c('green'='green','blue'='blue','red'='red'), labels=c('p1','p2','p3')) +
  theme(legend.position=c(0.1,0.5)) 
plot2=ggplotmlx() + geom_point(aes(x=time, y=y), data=res$y) 

grid.arrange(plot1, plot2, ncol=2)
