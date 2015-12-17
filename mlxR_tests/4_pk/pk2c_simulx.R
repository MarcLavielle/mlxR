setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm1 <- list(time=c(6, 36), amount=40, type=1)
adm2 <- list(time=c(12,42), amount=c(20,30), type=2)

g1   <- list(treatment=adm1)
g2   <- list(treatment=adm2)
g    <- list(g1, g2)
p    <- list(name=c("F","ka","V","k"), value=c(0.7,1,10,0.1))
Cc   <- list(name="Cc",time=seq(0, 60, by=0.1))
res  <- simulx(model="pk2a_model.txt",parameter=p,output=Cc,group=g)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1))
