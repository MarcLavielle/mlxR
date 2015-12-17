setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=seq(0, 45, by= 9), amount=1, target='C')

p <- list( name=c('K','KDE_pop','omega_KDE','KPQ_pop','omega_KPQ','KQPP_pop',
                  'omega_KQPP','LAMBDAP_pop','omega_LAMBDAP','GAMMA_pop',
                  'omega_GAMMA','DELTAQP_pop','omega_DELTAQP','a'),
           value=c(100,0.3,0.2,0.025,0.2,0.004,0.2,0.12,0.2,1,0.2,0.01,0.2,3))

f <- list(name='PSTAR', time=seq(-50,to=100,by=0.5))

mlxplore(model='tgi2_model.txt', parameter=p, output=f, treatment=adm)
