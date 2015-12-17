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

f <- list(name='f',time=seq(0, 100, by=1))
y <- list(name='y',time=seq(5, 100, by=10))
p <- list(name=c('a','b','s'), value=c(10, 0.5, 0.1))
res <- simulx(model=myModel,parameter=p,output=list(f, y))

print(ggplotmlx(aes(x=time, y=f), data=res$f) + geom_line(size=1) +
      geom_point(aes(x=time, y=y), data=res$y, color="red", size=3))
