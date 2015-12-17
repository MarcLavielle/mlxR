setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

adm1 <- list(time=seq(0,300,by=24),amount=10000,'rate'=2000,'target'='AGH')
adm2 <- list(time=seq(6,300,by=12),amount=40000,'target'='Gs2')
adm3 <- list(time=seq(12,300,by=12),amount=40000,'rate'=10000,'target'='Gs3')

g1  <- list(treatment=list(adm1, adm2))
g2  <- list(treatment=list(adm1, adm3))
g   <- list(g1, g2)

p <- list(name  = c('ta','tge2','tge3','GPV0','IPV0','VGPI'),
          value = c( 10,  70   , 150  , 100 ,  10  ,  60))
out <- list(name = c('lGH', 'lIH'), time = seq(-50, 400, by=0.2))

res <- simulx( model="agi_model.txt", parameter=p, output=out, group=g)

plot1=ggplotmlx(data=res$lGH, aes(x=time, y=lGH, colour=id)) +  
  geom_line(size=1) +  theme(legend.position="none") 
plot2=ggplotmlx(data=res$lIH,  aes(x=time, y=lIH, colour=id)) + geom_line(size=1)  
grid.arrange(plot1, plot2, ncol=2)
