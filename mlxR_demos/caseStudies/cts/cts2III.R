# library(mlxR)
library(gridExtra)
library(reshape2)


ctsModel3 <- inlineModel("
[LONGITUDINAL] 
input={Tk0,V,Cl,Imax,E0,IC50,kout,alpha,beta,b}

EQUATION:
C = pkmodel(Tk0, V, Cl)

E_0 = E0 
kin = E0*kout
ddt_E = kin*(1-Imax*C/(C+IC50)) - kout*E  

h = (alpha/1000)*exp(beta*E)
g = b*C

DEFINITION:
y = {distribution=normal, prediction=C, sd=g}
e = {type=event, maxEventNumber=10, rightCensoringTime=200, hazard=h}

;-----------------------------------------
[INDIVIDUAL]
input={Tk0_pop,V_pop,Cl_pop,Imax_pop,E0_pop,IC50_pop,kout_pop,alpha_pop,beta_pop,
  omega_Tk0,omega_V,omega_Cl,omega_Imax,omega_E0,omega_IC50,omega_kout,omega_alpha,omega_beta}

DEFINITION:
Tk0   = {distribution=lognormal,   prediction=Tk0_pop,  sd=omega_Tk0}
V     = {distribution=lognormal,   prediction=V_pop,    sd=omega_V}
Cl    = {distribution=lognormal,   prediction=Cl_pop,   sd=omega_Cl}
E0    = {distribution=lognormal,   prediction=E0_pop,   sd=omega_E0}
IC50  = {distribution=lognormal,   prediction=IC50_pop, sd=omega_IC50}
kout  = {distribution=lognormal,   prediction=kout_pop, sd=omega_kout}
Imax  = {distribution=logitnormal, prediction=Imax_pop, sd=omega_Imax}
alpha = {distribution=normal,      prediction=alpha_pop,sd=omega_alpha}
beta  = {distribution=normal,      prediction=beta_pop, sd=omega_beta}
")

#-------------------------------------
M <- 100
vN <- c(25, 50, 100)

adm.amount <- c(0, 25, 50, 100)
adm.time   <- seq(0,200,by=12)

pop.param   <- c(
  Tk0_pop   = 3,    omega_Tk0   = 0.2,
  V_pop     = 10,   omega_V     = 0.2,
  Cl_pop    = 1,    omega_Cl    = 0.2,
  Imax_pop  = 0.8,  omega_Imax  = 0.5,
  E0_pop    = 100,  omega_E0    = 0.1,
  IC50_pop  = 4,    omega_IC50  = 0.1,
  kout_pop  = 0.1,  omega_kout  = 0.1,
  alpha_pop = 0.5,  omega_alpha = 0.1,
  beta_pop  = 0.02, omega_beta  = 0,
  b         = 0.1
)

e <- list(name='e', time=0, surv.t=c(100,200))

#---------------------------------------------------
labels.arm <- c("placebo", "25 mg", "50 mg", "100mg")
labels.N   <- c("N = 25", "N = 50", "N = 100")

R <- NULL
ptm <- proc.time()
for (k in seq(1,length(adm.amount))){
  adm <- list(time=adm.time, amount=adm.amount[k])
  for (l in seq(1,length(vN))){
    g <- list(size=vN[l], level='individual'); 
    cat(paste0("\nN = ",vN[l],"  ;  amount = ",adm.amount[k], "\n")) 
    s <- 1000*l+100*k
    res <- simulx(model     = ctsModel3,
                  parameter = pop.param,
                  treatment = adm,
                  output    = e, 
                  group     = g,
                  nrep      = M,
                  settings  = list(seed = s, out.trt = F))
    res$e$nbEv.mean <- NULL
    R <- rbind(R, cbind(res$e, N=labels.N[l], arm=labels.arm[k]))
  }
} 
print(proc.time() - ptm)

#----------------------------------------------

rm <- melt(R,  id = c('N','arm','rep'), value.name="Survival", variable.name="time")
rm$time <- factor(rm$time, labels = c("T = 100", "T = 200"))

print(ggplotmlx(rm, aes(arm,Survival)) + geom_boxplot(aes(fill = arm)) + 
        facet_grid(time ~N) + theme(legend.position="none"))

#----------------------------------------------
S <- 0.1

R.p <- R[R$arm=="placebo","S200.mean"]
R.t <- R[R$arm!="placebo",]
R.t$dS <- (R.t$S200.mean-rep(R.p,3) > S)
R.t$S200.mean <- NULL
print(acast(R.t,N~arm,mean, value.var="dS"))


