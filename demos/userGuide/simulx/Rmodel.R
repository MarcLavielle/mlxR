setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)
library(gridExtra)
library(deSolve)

#------------------------------------------------
pk.model <- "Rmodel/modelR_pk1.R"
# pk.model <- "Rmodel/modelMlxt_pk1.txt"

adm1 <- list(time=seq(0,66,by=12), amount=100)
adm2 <- list(time=seq(12,78,by=24), amount=200)
y <- list(name="y", time=seq(18, 80, by=6))
C <- list(name="C", time=seq(0,100, by=0.5))
ind <- list(name=c("V","k"))
p <- c(V_pop=10, omega_V=0.3, w=50, k_pop=0.1, omega_k=0.2, a=2)
g1 <- list(treatment=adm1, size=6)
g2 <- list(treatment=adm2, size=3)

res1 <- simulx(model     = pk.model, 
               output    = list(C,y,ind),
               parameter = p,
               group     = list(g1,g2))

print(res1$parameter)

print(ggplotmlx()  +
        geom_line( data=res1$C, aes(x=time, y=C), colour="black", size=0.5) +
        geom_point(data=res1$y, aes(x=time, y=y), colour="red", size=1)+ 
        facet_wrap( ~ id))

#------------------------------------------------

pk.model <- "Rmodel/modelR_pk2.R"
# pk.model <- "Rmodel/modelMlxt_pk2.txt"

adm1 <- list(time=seq(1,144,by=24), amount=10, target="Ac")
adm2 <- list(time=seq(7,144,by=12), amount=4,  target="Ad")
adm <- list(adm1, adm2)
C <- list(name="C", time=seq(0,200, by=0.2))

p = c(ka=1, V=0.1, Cl=0.01, k12=0.2, k21=0.2, k13=0.3, k31=0.3)

res2 <- simulx(model     = pk.model, 
               output    = C,
               treatment = adm,
               parameter = p)

print(ggplotmlx(data=res2$C)  + geom_line(aes(x=time, y=C), size=0.5))

# ---------------------------------------------------------

adm <- list(time=seq(0,66,by=12), amount=100)
C <- list(name="C", time=seq(0,100, by=0.5))
ind <- list(name=c("w","V","k"))
p <- c(w_pop=70, omega_w=10, V_pop=10, omega_V=0.3, k_pop=0.1, omega_k=0.2)
g <- list(size=1000)

ptm <- proc.time()
res3a <- simulx(model     = "Rmodel/modelR_pk3.R", 
                output    = list(C,ind),
                parameter = p,
                treatment = adm,
                group     = g)
print(proc.time() - ptm)
print(prctilemlx(res3a$C)+ylim(c(0,25)))

ptm <- proc.time()
res3b <- simulx(model     = "Rmodel/modelMlxt_pk3.txt", 
                output    = list(C,ind),
                parameter = p,
                treatment = adm,
                group     = g)
print(proc.time() - ptm)
print(prctilemlx(res3b$C)+ylim(c(0,25)))

# -------------------------------------------------
modelR_reg1 <- function(parameter,regressor,time)
{  
  with(as.list(parameter),{
    t <- time[[1]]
    regt <- regressor[[1]]$time
    regC <- regressor[[1]]$value
    C <- approx(regt,regC,t)$y
    E = Emax*C/(C+EC50)
    r <- list(C=data.frame(time=t, C=C),
              E=data.frame(time=t, E=E))
    return(r)
  })
}

reg <- data.frame(time=seq(0,50,by=5),  Cin=exp(-0.1*seq(0,50,by=5)))
out <- list(name=c('C','E'), time=seq(0,50, by=0.2))

res4 <- simulx( model     = "modelR_reg1",
                parameter = c(Emax=100, EC50=0.3),
                regressor = reg,
                output    = out)


plot1 <- ggplotmlx(data=res4$C) + geom_line(aes(x=time, y=C))
plot2 <- ggplotmlx(data=res4$E) + geom_line(aes(x=time, y=E))
grid.arrange(plot1, plot2, ncol=2)

