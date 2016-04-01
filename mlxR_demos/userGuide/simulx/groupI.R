#library(mlxR)

pk.model <- inlineModel("
[LONGITUDINAL]
input = {V, k}
EQUATION:
C = pkmodel(V,k)
")

#-------------------------------------
adm1 <- list(time=seq(0,to=66,by=6), amount=50)
adm2 <- list(time=seq(0,to=66,by=12), amount=100)
adm3 <- list(time=seq(0,to=66,by=18), amount=150)
g1 <- list(treatment=adm1);
g2 <- list(treatment=adm2);
g3 <- list(treatment=adm3);

C <- list(name='C', time=seq(0, 100, by=1))
p <- c(V=10, k=0.2)


res1 <- simulx(model     = pk.model, 
               parameter = p, 
               output    = C, 
               group     = list(g1,g2,g3))

print(ggplotmlx(data=res1$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

#-------------------------------------------------------------
adm <- list(time=seq(0,to=66,by=12), amount=100)

g1 <- list(parameter=c(V=10, k=0.2))
g2 <- list(parameter=c(V=15, k=0.1))
g3 <- list(parameter=c(V=20, k=0.05))

res2 <- simulx(model     = pk.model, 
               treatment = adm, 
               output    = C, 
               group     = list(g1,g2,g3))

print(ggplotmlx(data=res2$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

#-------------------------------------
adm <- list(time=seq(0,to=66,by=12), amount=100)
p <- c(V=10, k=0.2)
C1 <- list(name='C', time=seq(0,  25, by=1))
C2 <- list(name='C', time=seq(30, 55, by=0.5))
C3 <- list(name='C', time=seq(60,100, by=0.25))
g1 <- list(output=C1)
g2 <- list(output=C2)
g3 <- list(output=C3)

res3 <- simulx(model     = pk.model, 
               treatment = adm, 
               parameter = p, 
               group     = list(g1,g2,g3))

print(ggplotmlx(data=res3$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

#-------------------------------------
adm1 <- list(time=seq(0,to=66,by=6), amount=50)
adm2 <- list(time=seq(0,to=66,by=12), amount=100)
adm3 <- list(time=seq(0,to=66,by=18), amount=150)
p1 <- c(V=10, k=0.2)
p2 <- c(V=15, k=0.1)
p3 <- c(V=20, k=0.05)
C1 <- list(name='C', time=seq(0,  75, by=1))
C2 <- list(name='C', time=seq(20, 90, by=0.5))
C3 <- list(name='C', time=seq(25,100, by=0.25))
g1 <- list(output=C1, treatment=adm1, parameter=p1)
g2 <- list(output=C2, treatment=adm2, parameter=p2)
g3 <- list(output=C3, treatment=adm3, parameter=p3)

res4 <- simulx(model = pk.model, 
               group = list(g1,g2,g3))

print(ggplotmlx(data=res4$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

