setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------


adm <- list(time=c(3,10),rate=1, amount=40)
Cc  <- list(name='Cc',time=seq(from=0, to=20, by=0.1))
p   <- list(name=c('V','k'), value=c(10,0.4))
res <- simulx(model='pk1_model.txt', parameter=p, output=Cc, treatment=adm)

plot1=ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line(size=1) 
print(plot1)
