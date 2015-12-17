setwd(dirname(parent.frame(2)$ofile))


f <- list( name = 'f',
           time = seq(0, 1, by=0.1))

p <- list( name  = c('u','v'),
           value = c(50,10))

mlxplore( model     = 'intro1_model.txt',
          parameter = p,
          output    = f)