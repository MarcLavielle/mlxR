setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)

#-------------------------------------
adm1 <- list(type   = 1, 
             time   = c(6, 30, 36), 
             amount = 40)

adm2 <- list(type   = 2, 
             time   = c(12,42), 
             amount = 20, 
             rate   = c(5, 10))

p    <- c(F=0.7, ka=1, V=10, k=0.1)

Cc   <- list(name="Cc", time=seq(0, 60, by=0.1))

res  <- simulx(model     = "model/pk2a.txt",
               parameter = p,
               output    = Cc,  
               group = list(size=2),
               treatment = list(adm1, adm2))

print(ggplotmlx(data=res$Cc) + geom_line(aes(x=time, y=Cc)))

#-------------------------------------
adm1 <- list(target = "Ad", 
             time   = c(6, 30, 36), 
             amount = 40*p[["F"]])

adm2 <- list(target = "Ac", 
             time   = c(12,42), 
             amount = 20, 
             rate   = c(5, 10))

res  <- simulx(model     = "model/pk2b.txt",
               parameter = p,
               output    = Cc,
               treatment = list(adm1, adm2))

print(ggplotmlx(data=res$Cc) + geom_line(aes(x=time, y=Cc)))

#-------------------------------------
adm1 <- list(type=1, time=seq(6, 66, by=8),  amount=2)
adm2 <- list(type=2, time=seq(9, 57, by=12), amount=1)
adm3 <- list(type=3, time=seq(12,60, by=12), amount=1,rate=0.2)

p <- c(F1=0.5, F2=0.8, ka=0.5, Tk0=4, kl=0.5, k23=0.3, 
       k32=0.5, V=10, k=0.2, Vm=0.5, Km=1)

Cc <- list(name = "Cc", time = seq(0,to=80,by=0.1))

res <- simulx( model     = "model/pk3.txt", 
               parameter = p, 
               output    = Cc, 
               treatment = list(adm1, adm2, adm3))

print(ggplotmlx(data=res$Cc) + geom_line(aes(x=time, y=Cc)))

#-------------------------------------
g1  <- list(treatment=list(adm1, adm3))
g2  <- list(treatment=list(adm2, adm3))

res <- simulx( model     = "model/pk3.txt", 
               parameter = p, 
               output    = Cc, 
               group     = list(g1, g2))
print(ggplotmlx(data=res$Cc) + geom_line(aes(x=time, y=Cc, colour=id)) +
  theme(legend.position=c(.95, .85)))
