# # # library("mlxR")
library(ggplot2)
#-------------------------------------

myModel = inlineModel("
  [LONGITUDINAL]
  input = {u, v}
  EQUATION:
  t0=0.2
  f_0=10
  ddt_f = -u*f/(v+f) 
")

f <- list(name='f',
          time=seq(0, 2, by=0.01))

p <- list(name=c('u','v'), 
          value=c(40,10))

res <- simulx( model     = myModel,
               parameter = p,
               output    = f)

print(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f)))
