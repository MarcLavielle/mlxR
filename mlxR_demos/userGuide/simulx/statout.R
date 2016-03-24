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

adm  <- list(amount=100, time=0)
p <- c(V_pop=10, Cl_pop=1, omega_V=0.2, omega_Cl=0.2, alpha=0.02, beta=0.1, b=0.1)
g <- list(size=100, level='individual')
s <- list(seed=123456)

out.y <- list(name='y', time=seq(0,to=25,by=5))
out.e <- list(name='e', time=0)

out.p <- list(name=c("V", "Cl"), FUN= c("mean", "sd"))
out   <- list(out.p, out.y, out.e)
res1a <- simulx(model=modelPK, treatment=adm, parameter=p, output=out, group=g, settings=s)
head(res1a$parameter)

out.p <- list(name=c("V", "Cl"), FUN="quantile", probs=c(0.05, 0.5, 0.95)) 
out   <- list(out.p, out.y, out.e)
res1b <- simulx(model=modelPK, treatment=adm, parameter=p, output=out, group=g, settings=s)
head(res1b$parameter)

out.p <- list(name=c("V", "Cl"),  FUN=c("mean","quantile"), probs=c(0.05,0.95)) 
out   <- list(out.p, out.y, out.e)
res1c <- simulx(model=modelPK, treatment=adm, parameter=p, output=out, group=g, settings=s)
head(res1c$parameter)

out.p <- c("V", "Cl")
out   <- list(out.p, out.y, out.e)
res1d <- simulx(model=modelPK, treatment=adm, parameter=p, output=out, group=g, settings=s)
statmlx(res1d$parameter, FUN=c("mean","quantile"), probs=c(0.05,0.95))

#----------------------

out.p <- list(name=c("V", "Cl"),  FUN=c("mean","quantile"), probs=c(0.05,0.95)) 
out.y <- list(name='y', time=seq(0,to=25,by=5), FUN = c("mean", "sd", "quantile"), probs = c(0.05, 0.95))
out.e <- list(name='e', time=0, type="event", surv.time=c(10,20))
out   <- list(out.p, out.y, out.e)
res2 <- simulx(model=modelPK, treatment=adm, parameter=p, output=out, group=g)
head(res2$y)

#----------------------

g1 <- list(size=100, level="individual", treatment=list(amount=100, time=0))
g2 <- list(size=100, level="individual", treatment=list(amount=50,  time=0))
g  <- list(g1, g2)
res3a <- simulx(model=modelPK, parameter=p, output=out, group=g, nrep=3)
head(res3a$parameter)
head(res3a$e)

res3a <- simulx(model=modelPK, parameter=p, output=out, group=g, nrep=3, result.file = "res3a.csv")
head(read.table("res3a.csv", header=T, sep=","))

#----------------------

