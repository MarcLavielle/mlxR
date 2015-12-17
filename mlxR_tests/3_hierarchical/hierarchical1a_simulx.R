setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

p <- list(name=c('V_pop','omega_V','w','w_pop','k','b'), value=c(10,0.3,75,70,0.1,0.1))
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
V <- list(name='V')
o <- list(V, f, y)
res <- simulx(model='hierarchical1_model.txt', parameter=p, output=o)

print(ggplotmlx(data=res$f, aes(x=time, y=f)) + geom_line(size=1) + 
      geom_point(data=res$y, aes(x=time, y=y), colour='red'))

print(res$V)
