# library(mlxR)
library(plyr)
library(reshape2)

#---- POISSON  model ----------------

poissonModela <- inlineModel("
[LONGITUDINAL]
input =  {a,b}

EQUATION:
lambda = a +b*t

DEFINITION:
y = {distribution=poisson, lambda=lambda}
")

p <- c(a=10, b=0.5)
l <- list(name='lambda', time=seq(0, 100, by=1))
y <- list(name='y', time=seq(0, 100, by=4))

res1a <- simulx(model=poissonModela, 
               parameter=p, 
               output=list(l, y))

print(ggplotmlx(aes(x=time, y=lambda), data=res1a$lambda) + geom_line(size=1) +
        geom_point(aes(x=time, y=y), data=res1a$y, color="red") + ylab("") )


poissonModelb <- inlineModel("
[LONGITUDINAL]
input =  {a,b}

EQUATION:
lambda=a +b*t

DEFINITION:
y = {type=count, P(y=k)=exp(-lambda)*(lambda^k)/factorial(k)}
")

res1b <- simulx(model=poissonModelb, 
               parameter=p, 
               output=list(l, y))

print(ggplotmlx(aes(x=time, y=lambda), data=res1b$lambda) + geom_line(size=1) +
        geom_point(aes(x=time, y=y), data=res1b$y, color="red") + ylab("") )


#------- NEGATIVE BINOMIAL DISTRIBUTION -------------

negbinModela <- inlineModel("
[LONGITUDINAL]
input =  {n,p}
DEFINITION:
y = {distribution=negativeBinomial, size=n, prob=p}
")

param <- c(n=10,p=0.4)
N <- 10000
y <- list(name='y', time=seq(1, N, by=1))

res2a <- simulx(model=negbinModela, 
                parameter=param, 
                output=y)

negbinModelb <- inlineModel("
[LONGITUDINAL]
input =  {n,p}
DEFINITION:
y = {type   = count, 
     P(y=k) = factorial(k+n-1)/factorial(k)/factorial(n-1)*((1-p)^k)*(p^n)
}
")

res2b <- simulx(model=negbinModelb, 
                parameter=param, 
                output=y)

ca=count(res2a$y$y)
cb=count(res2b$y$y)
names(ca) <- c('k','obs.a')
names(cb) <- c('k','obs.b')
c <- merge(ca, cb)
c$theo <- dnbinom(c$k,size=param[1],p=param[2])*N
r <- melt(c,  id = 'k', variable.name = 'freq')
print(ggplotmlx(r, aes(k,value, colour=freq)) + geom_point())
