setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=c(6, 15), amount=100)
Cc  <- list(name='Cc',time=seq(from=0, to=30, by=0.1))
p   <- list(name=c('F0','Tk0','ka','V','Cl'), value=c(0.3,3,0.6,10,0.5))
res <- simulx(model='pk5_model.txt', parameter=p, output=Cc, treatment=adm)

plot1=ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1) 
print(plot1)
