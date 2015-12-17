setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------


p <- list(name=c('V_pop','omega_V','w_pop','omega_w','k','b'), value=c(10,0.2,70,12,0.2,0.2))
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
ind <- list(name=c('w','V'))
out <- list(ind, f, y)
res <- simulx(model='hierarchical2_model.txt', parameter=p, output=out)

plot1 <- ggplotmlx() + 
  geom_line(data=res$f, aes(x=time, y=f), colour="black") + 
  geom_point(data=res$y, aes(x=time, y=y), colour="red") 
print(plot1)

print(res$parameter)
