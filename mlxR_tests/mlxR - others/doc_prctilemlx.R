# # library("mlxR")

myModel <- inlineModel("
[LONGITUDINAL]
input = {ka, V, Cl}
EQUATION:
C = pkmodel(ka,V,Cl)

[INDIVIDUAL]
input = {ka_pop, V_pop, Cl_pop, omega_ka, omega_V, omega_Cl}
DEFINITION:
ka = {distribution=lognormal, reference=ka_pop, sd=omega_ka}
V  = {distribution=lognormal, reference=V_pop,  sd=omega_V }
Cl = {distribution=lognormal, reference=Cl_pop, sd=omega_Cl}
")

N=500

pop.param   <- c(
  ka_pop  = 1,    omega_ka  = 0.5,
  V_pop   = 10,   omega_V   = 0.4,
  Cl_pop  = 1,    omega_Cl  = 0.3)
  
res <- simulx(model     = myModel,
              parameter = pop.param,
              treatment = list(time=0, amount=100),
              group     = list(size=N, level='individual'),
              output    = list(name='C', time=seq(0,24,by=0.5)))

p1   <- prctilemlx(res$C)
print(p1)

p2 <- prctilemlx(res$C, band=list(number=2, level=50))
print(p2)

p3 <- prctilemlx(res$C, band=list(number=1, level=90))
print(p3)

p4 <- prctilemlx(res$C, band=list(number=75, level=90))
print(p4)

p5 <- prctilemlx(res$C, band=list(number=4, level=80), plot=FALSE)
print(names(p5))
print(p5$proba)
print(p5$color)
print(p5$y[1:5,])

