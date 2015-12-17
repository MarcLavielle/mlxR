setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

myModel = inlineModel("
  [LONGITUDINAL]
  input = {V, k, b}
  EQUATION:
    D=100
  f = D/V*exp(-k*t)
  DEFINITION:
    y = {distribution=normal, prediction=f, sd=b*f}
  
  [INDIVIDUAL]
  input = {V_pop, omega_V, w, w_pop}
  EQUATION:
    V_pred = V_pop*(w/w_pop)
  DEFINITION:
    V = {distribution=logNormal, prediction=V_pred, sd=omega_V}
  
  [COVARIATE]
  input = {w_pop, omega_w}
  DEFINITION:
    w = {distribution=normal, mean=w_pop, sd=omega_w}
  
  [POPULATION]
  input = {ws, gw, Vs, gV}
  DEFINITION:
    w_pop = {distribution=normal, mean=ws, sd=gw}
  V_pop = {distribution=logNormal, mean=log(Vs), sd=gV}
")

p <- list(name=c('ws','gw','Vs','gV','omega_w','omega_V','k','b'), 
          value=c(70,15,10,0.2,2,0.05,0.2,0.1))
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
ind <- list(name=c('w','V'))
pop <- list(name=c('w_pop','V_pop'))
out <- list(pop, ind, f, y)
g = list(size=c(2,3),level=c('population','covariate'));
s <- list(seed=444)
res <- simulx(model=myModel, parameter=p, output=out,
              group=g, settings=s)

print(res$parameter)

plot1 <- ggplotmlx() + 
  geom_line(data=res$f, aes(x=time, y=f, colour=id), size=1) + 
  geom_point(data=res$y, aes(x=time, y=y, colour=id)) 
print(plot1)

