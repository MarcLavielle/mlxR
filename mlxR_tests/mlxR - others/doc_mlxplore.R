setwd(dirname(parent.frame(2)$ofile))
library("gridExtra")

#-----------------------------------------
p <- c( ka=0.5, V=10, k=0.2)
f <- list(name = c('f1','f2'), time = seq(0, 25, by=0.1))

mlxplore(model     = 'model/analytical.txt', 
         parameter = p, 
         output    = f)

#-----------------------------------------
res <- simulx(model     = 'model/analytical.txt', 
              parameter = p, 
              output    = f)
plot1 <- ggplotmlx(data=res$f1) + geom_line(aes(x=time, y=f1))
plot2 <- ggplotmlx(data=res$f2) + geom_line(aes(x=time, y=f2)) 
grid.arrange(plot1, plot2, ncol=2)

#---------------------------------------
adm1 <- list(type=1, time=seq(6, 66, by=8),  amount=2)
adm2 <- list(type=2, time=seq(9, 57, by=12), amount=1)
adm3 <- list(type=3, time=seq(12,60, by=12), amount=1,rate=0.2)

p <- c(F1=0.5, F2=0.8, ka=0.5, Tk0=4, kl=0.5, k23=0.3, k32=0.5, V=10, k=0.2, Vm=0.5, Km=1)

Cc <- list(name = "Cc", time = seq(0,to=80,by=0.1))

mlxplore( model     = "model/pk3.txt", 
          parameter = p, 
          output    = Cc, 
          treatment = list(adm1, adm2, adm3))

#-------------------------------------
g1  <- list(treatment=list(adm1, adm3))
g2  <- list(treatment=list(adm2, adm3))

mlxplore( model     = "model/pk3.txt", 
          parameter = p, 
          output    = Cc, 
          group     = list(g1,g2))

#-------------------------------------
p <- c(V_pop=10, omega_V=0.3, w=75, w_pop=70, k=0.1, b=0.1)
f <- list(name='f', time=seq(0, 30, by=0.1))

mlxplore(model     = 'model/hierarchical1.txt', 
         parameter = p, 
         output    = f)

#-------------------------------------
p <- c(V_pop=10, omega_V=0.3, w_pop=70, omega_w=12, k=0.15, b=0.1)
f <- list(name='f', time=seq(0, 30, by=0.1))

mlxplore(model     = 'model/hierarchical2.txt', 
         parameter = p, 
         output    = f)

