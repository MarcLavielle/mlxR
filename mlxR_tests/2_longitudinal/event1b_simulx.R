
tteModel1 <- inlineModel("
[LONGITUDINAL]
input = {beta,lambda}  
EQUATION:
h=(beta/lambda)*(t/lambda)^(beta-1)
DEFINITION:
e = {type=event, maxEventNumber=1, rightCensoringTime=70, hazard=h}
")

p1   <- c(beta=2.5,lambda=50)
e    <- list(name='e', time=0)
res1 <- simulx(model=tteModel1, parameter=p1, output=e, group=list(size=100))
pl1  <- kmplotmlx(res1$e)
print(pl1)

p2   <- c(beta=2,lambda=45)
g1   <- list(size=50, parameter=p1)
g2   <- list(size=100, parameter=p2)
res2 <- simulx(model=tteModel1, output=e, group=list(g1,g2))
pl2  <- kmplotmlx(res2$e,level=0.95)
print(pl2)

#-------------------------------------

tteModel2 <- inlineModel("
[LONGITUDINAL]
input = {beta,lambda}  

EQUATION:
  h = (beta/lambda)*(t/lambda)^(beta-1)

DEFINITION:
  e = {type=event, eventType=intervalCensored, 
       intervalLength=10, rightCensoringTime=100,  
       hazard=h}
")

p <- list(name=c('beta','lambda'), value=c(1.5,50))
h <- list(name='h', time=seq(0, 50, by=0.2))
e <- list(name='e', time=0)
o <- list(h, e);
g <- list(size=100, level='longitudinal')
res2 <- simulx(model     = tteModel2, 
               parameter = p, 
               output    = o,
               settings  = list(seed=12345),
               group     = g)

pl<-kmplotmlx(res2$e,index=3)
print(pl)
