setwd(dirname(parent.frame(2)$ofile))

library(ggplot2)
# # library("mlxR")
#-------------------------------------

adm <- list(time=seq(0, 30, by= 6), amount=1, target='C')

p <- c(K=100,KDE_pop=0.3,omega_KDE=0.2,KPQ_pop=0.025,omega_KPQ=0.2,
       KQPP_pop=0.004,omega_KQPP=0.2,LAMBDAP_pop=0.12,omega_LAMBDAP=0.2,
       GAMMA_pop=1,omega_GAMMA=0.2,DELTAQP_pop=0.01,omega_DELTAQP=0.2,a=3)

f <- list(name='PSTAR', time=seq(-50,to=100,by=0.5))
y <- list(name='y', time=seq(-50,to=100,by=10))
q <- list(name=c('KDE','KPQ','KQPP','LAMBDAP','GAMMA','DELTAQP'))
out <- list(f,y,q)

g <- list(size=6, level='individual')

############################################
res <- simulx(model='tgi2_model.txt',
              parameter=p,
              output=out,
              treatment=adm,
              group=g)
############################################

plot1=ggplotmlx() + geom_line(data=res$PSTAR, aes(x=time, y=PSTAR)) +
      geom_point(data=res$y, aes(x=time, y=y), colour="red") +
      facet_wrap( ~ id) + xlab("time (month)") + ylab("tumor size")

print(plot1)

print(res$parameter)
