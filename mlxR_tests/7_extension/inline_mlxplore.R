
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

mlxplore( model     = myModel,
          parameter = p,
          output    = f)

