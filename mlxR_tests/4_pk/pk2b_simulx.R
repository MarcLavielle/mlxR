setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm1 <- list(time=c(6, 30), amount=40, target="Ad")
adm2 <- list(time=c(12,42), amount=20, rate=c(5, 10), target="Ac")
trt  <- list(adm1, adm2)
p    <- list(name=c("ka","V","k"), value=c(1,10,0.1))
Cc   <- list(name="Cc",time=seq(0, 60, by=0.1))
res  <- simulx(model="pk2b_model.txt",parameter=p,output=Cc,treatment=trt)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line())
