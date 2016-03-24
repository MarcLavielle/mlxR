setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)
library(gridExtra)
library(reshape)

#-------------------------------------

seed <- 12345 
p    <- c(a=8,b=0.2)
pr   <- list(name=c('p0','p1','p2'), time=0:100)
y    <- list(name='y', time=seq(0, 100, by=2))
res  <- simulx(model     = 'model/categoricalA.txt', 
               parameter = p, 
               output    = list(pr, y),
               settings  = list(seed=seed))

plot1=ggplotmlx() + ylab("probabilities") +
  geom_line(data=res$p0, aes(x=time, y=p0, colour="p0"),size=1) +
  geom_line(data=res$p1, aes(x=time, y=p1, colour="p1"),size=1) +
  geom_line(data=res$p2, aes(x=time, y=p2, colour="p2"),size=1) +
  theme(legend.position=c(0.1,0.5),legend.title=element_blank()) 
plot2=ggplotmlx() + geom_point(aes(x=time, y=y), data=res$y) 
grid.arrange(plot1, plot2, ncol=2)

#------------------------------------------------------------
lpr  <- list(name=c('lp0','lp1'), time=0:100)
res  <- simulx(model     = 'model/categoricalB.txt', 
               parameter = p, 
               output    = list(y,lpr),
               settings  = list(seed=seed))
# derive the probability mass function from the cumulative logits
p0 <- 1/(1+exp(-res$lp0$lp0))
p1 <- 1/(1+exp(-res$lp1$lp1))-p0
p2 <- 1-p0-p1
# create a single data frame with the probabilities  
pr <- data.frame(time=0:100,p0,p1,p2)
pr <- melt(pr ,  id = 'time', variable_name = 'proba')
# pr is a data frame with columns "id", "proba" and "value"
plot1=ggplotmlx(pr, aes(time,value)) + geom_line(aes(colour = proba),size=1) +
  ylab('probabilities') + theme(legend.position=c(.1, .5))
plot2=ggplotmlx() + geom_point(aes(x=time, y=y), data=res$y) 
grid.arrange(plot1, plot2, ncol=2)

#------------------------------------------------------------
g <- list(size=1000)
res <- simulx(model='model/categoricalA.txt', parameter=p, output=y, group=g)
plot3 <- catplotmlx(res$y, breaks=25)
print(plot3)

