setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

adm <- list(time=c(6, 18, 36), amount=100)
out <- list(name=c('Cc','Ce'),time=seq(from=0, to=60, by=1))
p   <- list(name=c('k12','k21','V','Vm','Km','ke0'), value=c(0.3,0.2,10,10,1,0.1))
res <- simulx(model='pk4_model.txt', parameter=p, output=out, treatment=adm)

plot1<-qplot(x=time, y=Cc, data=res$Cc, geom=c("line"))+ggtitle("Central compartment")
plot2<-qplot(x=time, y=Ce, data=res$Ce, geom=c("line"))+ggtitle("Effect compartment")
grid.arrange(plot1, plot2, ncol=2)
