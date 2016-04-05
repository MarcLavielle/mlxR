setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)
library(gridExtra)

#----------------------------------------
t   <- (0:50)
reg <- list(name='C',
            time=t,
            value=exp(-0.1*t))

out <- list(name='E',
            time=t)

res <- simulx( model     = "model/regression1a.txt",
               parameter = c(Emax=100, EC50=0.3),
               regressor = reg,
               output    = out)

plot(ggplotmlx(data=res$E) + geom_line(aes(x=time, y=E)))

#----------------------------------------
out <- list(name=c('E','Cout'),
            time=t)

res <- simulx( model     = "model/regression1b.txt",
               parameter = c(Emax=100, EC50=0.3),
               regressor = reg,
               output    = out)

names(res[2]) <- "C"
names(res$C)  <- c("time","C")
plot1 <- ggplotmlx(data=res$C) + geom_line(aes(x=time, y=C))
plot2 <- ggplotmlx(data=res$E) + geom_line(aes(x=time, y=E))
grid.arrange(plot1, plot2, ncol=2)

#----------------------------------------
x <- list(name='x',
          time=c(0,5,10,20,25,30,40),
          value=c(1,-1,1,-1,1,-1,1))

f <- list(name='f',
          time=seq(0, 50, by=1))

res <- simulx( model     = "model/regression2.txt",
               parameter = c(a=1, b=-0.5),
               regressor = x,
               output    = f)

print(ggplotmlx(data=res$f) + geom_line(aes(x=time, y=f)))

#----------------------------------------
x <- list(name='x',
          time=c(0,5,10,20,25,30,40),
          value=c(1,-1,1,-1,1,-1,1))

f <- list(name='f',
          time=seq(-5, 50, by=1))

res <- simulx( model     = "model/regression3a.txt",
               parameter = c(k=0.2, f0=0),
               regressor = x,
               output    = f)

print(ggplotmlx(data=res$f) + geom_line(aes(x=time, y=f)))


#----------------------------------------
x1 <- list(name='x',
           time=c(0,5,10,20,25,30,40),
           value=c(1,-1,1,-1,1,-1,1))
x2 <- list(name='x',
           time=c(0,4,14,24,34),
           value=c(1,-0.5,1.5,-1,0.2))
g1  <- list(regressor = x1) #, size=3)
g2  <- list(regressor = x2) #, size=2)

f <- list(name='f',
          time=seq(-5, 50, by=1))

res <- simulx( model     = "model/regression3a.txt",
               parameter = c(k=0.2, f0=0),
               group     = list(g1,g2),
               output    = f)

print(ggplotmlx(data=res$f) + geom_line(aes(x=time, y=f, colour=id)) + theme(legend.position=c(0.9, 0.85)))

#----------------------------------------
x  <- inlineDataFrame("
id   time  x
1    0     1
1    5    -1
1   10     1
1   20    -1
1   25     1
1   30    -1
1   40     1    
2    0   1.0
2    4  -0.5
2   14   1.5
2   24  -1.0
2   34   0.2
")

f <- list(name='f',
          time=seq(-5, 50, by=1))

res <- simulx( model     = "model/regression3a.txt",
               parameter = c(k=0.2, f0=0),
               regressor = x,
               output    = f)

print(ggplotmlx(data=res$f) + geom_line(aes(x=time, y=f, colour=id)) + theme(legend.position=c(0.9, 0.85)))

#----------------------------------------
x <- list(name='x',
          time=c(0,5,10,20,25,30,40),
          value=c(1,-1,1,-1,1,-1,1))
y <- list(name='y', time=seq(4, 48, by=1))

res <- simulx( model     = "model/regression3b.txt",
               parameter = c(k=0.2, f0=0, a=0.5),
               regressor = x,
               output    = list(f,y))

print(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f), colour="black") + 
        geom_point(data=res$y, aes(x=time, y=y), colour="red"))

#----------------------------------------
x1 <- list(name='x1',
           time=c(0,10,20,30,40),
           value=c(1,-1,1,-1,1)*0.5)
x2 <- list(name='x2',
           time=c(5,15,25,35),
           value=c(1,-1,1,-1)*0.3)

fg <- list(name=c('f','g'),
           time=seq(-5, 50, by=1))

res <- simulx( model     = "model/regression4.txt",
               parameter = c(k1=0.2, k2=0.1, f0=0, g0=0),
               regressor = list(x1, x2),
               output    = fg)

print(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour="blue")) + 
        geom_line(data=res$g, aes(x=time, y=g, colour="red")) +
        scale_colour_manual(name="",values=c('blue'='blue','red'='red'),labels=c('f','g')) + 
        theme(legend.position=c(0.9, 0.85)))

