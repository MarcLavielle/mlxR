setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm1 <- list(time=seq(6,to=66,by=12), amount=2, type=1)
adm2 <- list(time=seq(9,to=57,by=24), amount=1, type=2)
adm3 <- list(time=seq(12,to=60,by=24), amount = 1,rate=0.2, type=3)
trt  <- list(adm1, adm2, adm3)
p <- list(name  = c('F1','F2','ka','Tk0','kl','k23','k32','V','k','Vm','Km'),
          value = c(0.5, 0.8, 0.5, 4, 0.5, 0.3, 0.5, 10, 0.2, 0.5, 1))
Cc <- list(name = "Cc", time = seq(0,to=80,by=0.1))

res <- simulx( model="pk6_model.txt", parameter=p, output=Cc, treatment=trt)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line())
