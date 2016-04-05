#library(mlxR)
library(gridExtra)

joint.model1 <- inlineModel("
[LONGITUDINAL]
input = {ka, V, Cl, IC50, kin, kout, a1, a2}  

EQUATION:
C     = pkmodel(ka, V, Cl)
t0    = 0
E_0   = kin/kout
ddt_E = kin*(1 - C/(IC50+C)) - kout*E

DEFINITION:
Concentration = {distribution = lognormal, 
                 prediction   = C, 
                 sd           = a1}

Effect = {distribution = normal, 
          prediction   = E, 
          sd           = a2}
")

p <- c(ka=0.5, V=8, Cl=1.5, IC50=0.5, kin=10, kout=0.1, a1=0.1, a2=5)

a <- list(amount = 10, time = seq(2,120,by=12))

f <- list(name = c('C', 'E'),     time = seq(0,100,by=1))
c <- list(name = 'Concentration', time = seq(4,100,by=12))
e <- list(name = 'Effect',        time = seq(5,100,by=10))

res <- simulx(model     = joint.model1,
              treatment = a,
              parameter = p,
              output    = list(f, c, e),
              settings  = list(seed = 1234))

plot1 = ggplotmlx() + geom_line(data=res$C, aes(x=time, y=C)) +
        geom_point(data=res$Concentration, aes(x=time, y=Concentration),colour="red") 
plot2 = ggplotmlx() + geom_line(data=res$E, aes(x=time, y=E)) +
        geom_point(data=res$Effect, aes(x=time, y=Effect),colour="red") 
grid.arrange(plot1, plot2, ncol=2)

