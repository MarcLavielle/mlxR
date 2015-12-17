setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)

p <- c(ka=0.5, V=8, Cl=1.5, u=0.003, v=3, a1=0.05)

a <- list(amount = 10, time = seq(2,120,by=12))

f <- list(name = c('C', 'h'),     time = seq(0,100,by=1))
c <- list(name = 'Concentration', time = seq(4,100,by=12))
e <- list(name = 'Hemorrhaging',  time = 0)

res1 <- simulx(model     = 'model/joint2.txt',
               treatment = a,
               parameter = p,
               output    = list(f, c, e),
               settings  = list(seed = 121212))

print(res1$Hemorrhaging)

plot1 = ggplotmlx() + geom_line(data=res1$C, aes(x=time, y=C)) +
        geom_point(data=res1$Concentration, aes(x=time, y=Concentration),colour="red") 
plot2 = ggplotmlx() + geom_line(data=res1$h, aes(x=time, y=h)) +
        ylab("hazard")  + theme(legend.position="none")
grid.arrange(plot1, plot2, ncol=2)


#-----------------------------------------------
N   <- 20
res2 <- simulx(model     = 'model/joint2.txt',
               treatment = a,
               parameter = p,
               output    = list(f, c, e),
               group     = list(size = N, level='longitudinal'),
               settings  = list(seed = 121212))

plot3 = ggplotmlx() + geom_line(data=res2$C, aes(x=time, y=C, group=id)) +
  geom_point(data=res2$Concentration, aes(x=time, y=Concentration),colour="red") 
plot(plot3)

h1    = res2$Hemorrhaging[res2$Hemorrhaging[,3]==1 ,]
plot4 = ggplotmlx()+geom_point(data=h1, aes(x=time,y=id), size=3) + 
        xlab("event time") + ylab("replicate")  + theme(legend.position="none")
plot(plot4)