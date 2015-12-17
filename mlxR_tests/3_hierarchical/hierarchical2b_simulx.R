setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

p <- list( name = c('V_pop','omega_V','w_pop','omega_w','k','b'), 
           value = c(10,0.2,70,12,0.2,0.2))

f <- list( name = 'f', 
           time = seq(0, 30, by=0.1))

y <- list( name = 'y', 
           time = seq(1, 30, by=3))

ind <- list( name = c('w','V'))

o <- list(ind, f, y)

g <- list( size  = 5,
           level = 'covariate')

res <- simulx( model     = 'hierarchical2_model.txt', 
               parameter = p, 
               output    = o, 
               group     = g)


print(res$parameter)

plot1 <- ggplotmlx(data=res$f, aes(x=time, y=f, colour=id)) + 
  geom_line(size=1) + 
  geom_point(data=res$y, aes(x=time, y=y, colour=id)) 
print(plot1)


