# library(mlxR)
library(gridExtra)

ode.model <- inlineModel("
[LONGITUDINAL]
input = {a, b, c}

EQUATION:
t0     = 0
f1_0   = 10
f2_0   = 0
ddt_f1 = a*f2 - b*f1/(1+c*f1)
ddt_f2 = b*f1/(1+c*f1) - a*f2
")

p <- c(a = 0.07, b = 0.1, c = 0.5) 

out <- list(name=c('f1', 'f2'), time=-5:100)

res <- simulx(model     = ode.model, 
              parameter = p, 
              output    = out)

plot1=ggplotmlx(data=res$f1, aes(x=time, y=f1)) + geom_line(colour='blue',size=0.75)  
plot2=ggplotmlx(data=res$f2, aes(x=time, y=f2)) + geom_line(colour='red', size=0.75) 
grid.arrange(plot1, plot2, ncol=2)

