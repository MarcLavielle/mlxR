setwd(dirname(parent.frame(2)$ofile))

adm1 <- list( time=seq(0,7.5,by=1.5), amount=1, target='C' )
adm2 <- list( time=seq(0, 45, by= 9), amount=1, target='C' )

f <- list( name=c('PT','Q','QP','PSTAR'), time=seq(-50,100,by=0.5) )

psi <- list( name  = c('K','KDE','KPQ','KQPP','LAMBDAP','GAMMA','DELTAQP'),
             value = c(100, 0.3, 0.025, 0.004, 0.12, 1, 0.01) )

g1 <- list( treatment = adm1)
g2 <- list( treatment = adm2)

mlxplore( model     = 'tgi1_model.txt',
          parameter = psi,
          output    = f,
          group     = list(g1, g2) );

