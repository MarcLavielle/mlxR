library(mlxR)
library(plyr)
library(reshape2)

#---- POISSON  model ----------------

countModel1 <- inlineModel("
[LONGITUDINAL]
input =  {a,b}

EQUATION:
  lambda=a +b*t

DEFINITION:
  y = {type=count, P(y=k)=exp(-lambda)*(lambda^k)/factorial(k)}
")

p <- c(a=10, b=0.5)
l <- list(name='lambda', time=seq(0, 100, by=1))
y <- list(name='y', time=seq(0, 100, by=4))

res1 <- simulx(model=countModel1, 
              parameter=p, 
              output=list(l, y))

print(ggplotmlx(aes(x=time, y=lambda), data=res1$lambda) + geom_line(size=1) +
  geom_point(aes(x=time, y=y), data=res1$y, color="red") + ylab("") )


#------- NEGATIVE BINOMIAL DISTRIBUTION -------------

countModel2 <- inlineModel("
[LONGITUDINAL]
input =  {n,p}


DEFINITION:
y = {type   = count, 
     P(y=k) = factorial(k+n-1)/factorial(k)/factorial(n-1)*((1-p)^k)*(p^n)
}
")

param <- c(n=10,p=0.4)
N <- 10000
y <- list(name='y', time=seq(1, N, by=1))

res2 <- simulx(model=countModel2, 
              parameter=param, 
              output=y)

c=count(res2$y$y)
names(c) <- c('k','obs')
c$theo <- dnbinom(c$k,size=param[1],p=param[2])*N
r <- melt(c,  id = 'k', variable_name = 'freq')
print(ggplotmlx(r, aes(k,value, colour=freq)) + geom_point())
