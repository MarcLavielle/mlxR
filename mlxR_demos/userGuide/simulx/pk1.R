
#----------------------------------
adm <- list(time=c(3,10), amount=c(40,60), tinf=c(2,3))

Cc  <- list(name='Cc',time=seq(from=0, to=20, by=0.1))
p   <- c(V=10, Cl=4)

res <- simulx(model='model/pk1a.txt', 
              parameter=p, 
              output=Cc, 
              treatment=adm)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))

setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)

#----------------------------------
adm <- list(time=c(1, 7, 13), amount=40)
res <- simulx(model='model/pk1a.txt', parameter=p, output=Cc, treatment=adm)
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))

#----------------------------------
adm <- list(time=c(1, 7, 13), amount=c(40, 20, 10))
res <- simulx(model='model/pk1a.txt', parameter=p, output=Cc, treatment=adm)
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))

#----------------------------------
adm <- list(time=c(1, 7, 13), amount=c(40, 20, 10), rate=8)
res <- simulx(model='model/pk1a.txt', parameter=p, output=Cc, treatment=adm)
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))

#----------------------------------
adm <- list(time=c(1, 7, 13), amount=c(40, 20, 10), rate=c(8, 5, 4))
res <- simulx(model='model/pk1a.txt', parameter=p, output=Cc, treatment=adm)
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))

#----------------------------------
adm <- list(time=c(1, 7, 13), amount=c(40, 20, 10), tinf=c(5, 4, 2.5))
res <- simulx(model='model/pk1a.txt', parameter=p, output=Cc, treatment=adm)
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))

#----------------------------------
adm1 <- list(time=seq(0,20, by=6), amount=40, tinf=2)
adm2 <- list(time=seq(0,20, by=3), amount=20, tinf=1)
g1   <- list(treatment=adm1)
g2   <- list(treatment=adm2)
res <- simulx(model='model/pk1a.txt', parameter=p, output=Cc, group=list(g1, g2))
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1))

#----------------------------------
adm <- list(target="Ac", time=3, amount=40)
res <- simulx(model='model/pk1b.txt', parameter=p, output=Cc, treatment=adm)
print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1))
