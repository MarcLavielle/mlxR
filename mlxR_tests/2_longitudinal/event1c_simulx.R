tteModel1 <- inlineModel("
[LONGITUDINAL]
input = {beta,lambda}  
EQUATION:
h=(beta/lambda)*(t/lambda)^(beta-1)
DEFINITION:
e = {type=event, maxEventNumber=1, rightCensoringTime=70, hazard=h}
")

p1 <- list(name=c('beta','lambda'), value=c(2.5,50))
p2 <- list(name=c('beta','lambda'), value=c(2,45))
e <- list(name='e', time=0)
g1 <- list(size=50, parameter=p1)
g2 <- list(size=100, parameter=p2)

res1 <- simulx(model=tteModel1, output=e, group=list(g1,g2))

pl<-kmplotmlx(res1$e, level=0.9)
print(pl)
