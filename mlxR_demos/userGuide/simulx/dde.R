#library(mlxR)
library(gridExtra)

dde.model <- inlineModel("
[LONGITUDINAL]
input = {a, b, tau1, tau2}

EQUATION:
  t0 = 0
f1_0 = 10+t
f2_0 = 3
ddt_f1 = a*delay(f2,tau2) - b*delay(f1,tau1)
ddt_f2 = b*delay(f1,tau1) - a*delay(f2,tau2)
")

p <- c(a = 0.08, b  = 0.05, tau1 = 5, tau2 = 10) 

out <- list(name=c('f1', 'f2'), time=seq(-10,100,by=0.1))

res <- simulx(model     = dde.model, 
              parameter = p, 
              output    = out)

plot1=ggplotmlx(data=res$f1, aes(x=time, y=f1)) + geom_line()  
plot2=ggplotmlx(data=res$f2, aes(x=time, y=f2)) + geom_line() 
grid.arrange(plot1, plot2, ncol=2)

