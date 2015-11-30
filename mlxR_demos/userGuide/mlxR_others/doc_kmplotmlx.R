
tteModelA <- inlineModel("
[LONGITUDINAL]
input = {beta,lambda}  
EQUATION:
h=(beta/lambda)*(t/lambda)^(beta-1)
DEFINITION:
e = {type=event, maxEventNumber=1, rightCensoringTime=70, hazard=h}
")

p1   <- c(beta=2.5,lambda=50)
e    <- list(name='e', time=0)
res1 <- simulx(model     = tteModelA, 
               parameter = p1, 
               output    = e, 
               group     = list(size=100))

pl1a  <- kmplotmlx(res1$e)
print(pl1a)

pl1b  <- kmplotmlx(res1$e, level=0.90)
print(pl1b)

p2   <- c(beta=3,lambda=40)
g1   <- list(size=50,  parameter=p1)
g2   <- list(size=100, parameter=p2)
res2 <- simulx(model    = tteModelA, 
               output   = e, 
               group    = list(g1,g2),
               settings = list(seed=1234))

pl2  <- kmplotmlx(res2$e)
print(pl2)

#-------------------------------------

tteModelB <- inlineModel("
[LONGITUDINAL]
input = {beta,lambda}  

EQUATION:
  h = (beta/lambda)*(t/lambda)^(beta-1)

DEFINITION:
  e = {type=event, eventType=intervalCensored, 
       intervalLength=10, rightCensoringTime=100,  
       hazard=h}
")

p <- c(beta=1.5,lambda=50)
e <- list(name='e', time=0)
g <- list(size=100)
res2 <- simulx(model     = tteModelB, 
               parameter = p, 
               output    = e,
               group     = g)

pl2a<-kmplotmlx(res2$e,level=0.90)
print(pl2a)

pl2b<-kmplotmlx(res2$e,index=3,level=0.90)
print(pl2b)
