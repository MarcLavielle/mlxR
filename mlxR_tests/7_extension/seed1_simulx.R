library(ggplot2)
#-------------------------------------

myModel = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}

EQUATION:
f = a + b*t
e ~ normal(0,s)
y = f*exp(e)
")

y <- list(name='y',time=c(50, 100))
p <- list(name=c('a','b','s'), value=c(10, 0.5, 0.1))
s <- list(seed=12345)

res <- simulx(model=myModel, parameter=p, output=y, settings=s)
print(res$y)

res <- simulx(model=myModel, parameter=p, output=y, settings=s)
print(res$y)

s$seed = 654321
res <- simulx(model=myModel, parameter=p, output=y, settings=s)
print(res$y)