setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------


p <- list(name=c('ka','V','k'), value=c(0.5,10,0.2))
f <- list(name=c('f1','f2'), time=seq(0, 30, by=0.1))
res <- simulx(model='prediction_model.txt', parameter=p, output=f)

plot1=ggplotmlx(aes(x=time, y=f1), data=res$f1) + geom_line() +
      geom_line(aes(x=time, y=f2), data=res$f2, color="red") 
print(plot1)