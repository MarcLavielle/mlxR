setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm1 <- list(time=0, amount=100)
adm2 <- list(time=2, amount=100)
p <- list(name=c('V_pop','omega_V','w','k','a'), value=c(10,0.3,75,0.2,0.5))
y <- list(name='y', time=seq(0, 10, by=1))
op<- list(name="V")
out=list(y,op)
g <- list(list(treatment=adm1) , list(treatment=adm2))
res<-simulx(model='group2_model.txt', parameter=p, output=out, group=g)

print(res$parameter)
plot1 <- ggplotmlx(data=res$y, aes(x=time, y=y, colour=id)) + 
  geom_line(size=1) + geom_point() 
print(plot1)
