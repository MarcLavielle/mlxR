setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------


p <- list(name=c('ws','gw','Vs','gV','omega_w','omega_V','k','b'), 
          value=c(70,10,10,0.1,12,0.15,0.2,0.2))
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
ind <- list(name=c('w','V'))
pop <- list(name=c('w_pop','V_pop'))
out <- list(pop, ind, f, y)
res <- simulx(model='hierarchical3_model.txt', parameter=p, output=out)


plot1 <- ggplotmlx(data=res$f, aes(x=time, y=f)) + 
  geom_line(size=1) + 
  geom_point(data=res$y, aes(x=time, y=y), colour="red") 
print(plot1)

print(res$parameter)
