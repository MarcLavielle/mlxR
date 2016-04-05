# library(mlxR)


modelPK <- inlineModel("
[LONGITUDINAL] 
input={V,Cl,alpha, beta,b}

EQUATION:
C = pkmodel(V, Cl)
h = alpha*exp(beta*C)
g = b*C

DEFINITION:
y = {distribution=normal, prediction=C, sd=g}
e = {type=event, maxEventNumber=1, rightCensoringTime=30, hazard=h}

;-----------------------------------------
[INDIVIDUAL]
input={V_pop,Cl_pop,omega_V,omega_Cl}

DEFINITION:
V     = {distribution=lognormal,   prediction=V_pop,    sd=omega_V}
Cl    = {distribution=lognormal,   prediction=Cl_pop,   sd=omega_Cl}
")

#------------------------------------------
adm  <- list(amount=100, time=0)
p <- c(V_pop=10, Cl_pop=1, omega_V=0.2, omega_Cl=0.2, alpha=0.02, beta=0.1, b=0.1)
g <- list(size=5, level='individual')

out.y <- list(name='y', time=seq(0,to=25,by=5))
out.e <- list(name='e', time=0)
out.p <- c("V", "Cl")
out   <- list(out.p, out.y, out.e)

res1 <- simulx(model=modelPK, treatment=adm, parameter=p, output=out, group=g, nrep=3)
print(head(res1$parameter))
print(summary(res1$parameter))

#------------------------------------------
out.p2 <- list(name=c("V", "Cl"), FUN="quantile", probs=c(0.05, 0.5, 0.95)) 
out2   <- list(out.p, out.y, out.e)
res2 <- simulx(model=modelPK, treatment=adm, parameter=p, output=out2, group=g, nrep=3)
print(res2$parameter)

#------------------------------------------
g1 <- list(size=3, level="individual", treatment=list(amount=100, time=0))
g2 <- list(size=3, level="individual", treatment=list(amount=50,  time=0))
g  <- list(g1, g2)
res3 <- simulx(model=modelPK, parameter=p, output=out, group=g, nrep=2)
print(res3$parameter, digits=3)
print(head(res3$e))


