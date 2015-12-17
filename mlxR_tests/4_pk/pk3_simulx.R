setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------


adm <- list(time=c(6, 18, 36), amount=100)
Cc  <- list(name='Cc',time=seq(from=0, to=60, by=1))
p   <- list(name=c('F','Tlag','Tk0','V','Cl'), value=c(0.7,1,3,10,1))
res <- simulx(model='pk3_model.txt', parameter=p, output=Cc, treatment=adm)

plot1=ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1) 
print(plot1)
