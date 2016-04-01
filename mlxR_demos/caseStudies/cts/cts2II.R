library(gridExtra)

ctsModel2 <- inlineModel("
[LONGITUDINAL] 
input={Tk0,V,Cl,Imax,E0,IC50,kout,alpha,beta}

EQUATION:
C = pkmodel(Tk0, V, Cl)

E_0 = E0 
kin = E0*kout
ddt_E = kin*(1-Imax*C/(C+IC50)) - kout*E  

h = (alpha/1000)*exp(beta*E)
H_0 = 0
ddt_H = h
S = exp(-H)

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
alpha = {distribution=lognormal,   prediction=alpha_pop,sd=omega_alpha}
beta  = {distribution=lognormal,   prediction=beta_pop, sd=omega_beta}
")

#-----------------------------------------------
N=500
adm.time <- seq(0,200,by=12)

g1 <- list(size=N, level='individual', treatment=list(time=adm.time, amount=  0))
g2 <- list(size=N, level='individual', treatment=list(time=adm.time, amount= 25))
g3 <- list(size=N, level='individual', treatment=list(time=adm.time, amount= 50))
g4 <- list(size=N, level='individual', treatment=list(time=adm.time, amount=100))

pop.param   <- c(
  Tk0_pop   = 3,    omega_Tk0   = 0.2,
  V_pop     = 10,   omega_V     = 0.2,
  Cl_pop    = 1,    omega_Cl    = 0.2,
  Imax_pop  = 0.8,  omega_Imax  = 0.5,
  E0_pop    = 100,  omega_E0    = 0.1,
  IC50_pop  = 4,    omega_IC50  = 0.1,
  kout_pop  = 0.1,  omega_kout  = 0.1,
  alpha_pop = 0.5,  omega_alpha = 0.2,
  beta_pop  = 0.02, omega_beta  = 0
)

f <- list(name=c('C','E','S'), 
          time=seq(0,to=200,by=2))
  
res <- simulx(model     = ctsModel2,
              parameter = pop.param,
              group     = list(g1,g2,g3,g4),
              output    = f)

#we will display 8 predictions intervals defined by percentiles 10%, 20%, ... 90%
band <- list(number=8,level=80)
nf <- length(f$name)
adm.amount <- c(0, 25, 50, 100)
ng <- length(adm.amount)
for (j in seq(1,nf)){
  resj <- res[[j]]
  fj   <- f$name[j]
  
  #compute first the range of the percentiles in the 4 groups and define 
  #the y-limits as the wider range
  rj <- NULL
  for (k in seq(1,ng)){
    resjk <- resj[resj$group==k,]
    pj   <- prctilemlx(resjk, band=band, plot=FALSE)
    rj <- c(rj,range(pj$y[,-1]))
  }
  rj <- range(rj)
  
  #we can now compute and display the percentiles in each group using the 
  #same y-limits
  for (k in seq(1,ng)){
    resjk <- resj[resj$group==k,]
    qj <- prctilemlx(resjk, band=band)+ylim(rj)
    qj <- qj + ggtitle(paste0("amount = ",adm.amount[k],"mg")) +
          xlab("time (h)") + ylab(f$name[j]) + theme(legend.position="none")
    eval(parse(text=paste0("pl",k,"=qj")))
  }
  cat(fj,"\n")
  grid.arrange(pl1, pl2, pl3, pl4, ncol=2)
}
