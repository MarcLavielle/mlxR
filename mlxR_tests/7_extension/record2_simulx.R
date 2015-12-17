setwd(dirname(parent.frame(2)$ofile))

myModel = inlineModel("
[LONGITUDINAL]
input = {V, k, a}
EQUATION:
  Cc = pkmodel(V,k)
DEFINITION:
  y = {distribution=normal, prediction=Cc, sd=a}

[INDIVIDUAL]
input = {V_pop, omega_V, w}
EQUATION:
  Vpred=V_pop*(w/70)
DEFINITION:
  V = {distribution=lognormal, prediction=Vpred, sd=omega_V}
")

adm1 <- list(time=seq(0,to=60,by= 6), amount= 50, rate=25)
adm2 <- list(time=seq(0,to=60,by=12), amount=100, rate=50)
y <- list(name='y', time=seq(0, 72, by=6))
p <- list(name=c('V_pop','omega_V','w','k','a'), value=c(10,0.3,75,0.2,0.5))
g1 <- list(treatment=adm1,size=c(2,3),level=c('individual','longitudinal'));
g2 <- list(treatment=adm2,size=4,level='individual');
g <- list(g1,g2)
s <- list(record.file="simulx2_data.txt")

res <- simulx(model=myModel, parameter=p, output=y, group=g, settings=s)

