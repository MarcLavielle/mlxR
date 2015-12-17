setwd(dirname(parent.frame(2)$ofile))

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







