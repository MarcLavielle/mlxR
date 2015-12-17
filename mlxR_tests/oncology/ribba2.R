library(gridExtra)
# # library("mlxR")

model.pharmml <- "pharmML/Ribba_2012_TGI.xml"
model.mlxtran <- 'Mlxtran/ribba2.txt'

adm <- list(time=seq(0, 36,by=12),amount=1)

f <- list(name='PSTAR', time = seq(-50,to=100,by=0.5))
y <- list(name='PSTAR_obs', time = seq(-50,to=100,by=10))
q <- list(name=c('KDE','KPQ','KQPP','LAMBDAP','GAMMA','DELTAQP'))

p <- c(K=100, pop_KDE=0.3, omega_KDE=0.2, pop_KPQ=0.025, omega_KPQ=0.2, pop_KQPP=0.004, 
       omega_KQPP=0.2, pop_LAMBDAP=0.12, omega_LAMBDAP=0.2, pop_GAMMA=1, omega_GAMMA=0.2, 
       pop_DELTAQP=0.01, omega_DELTAQP=0.2, a=3 )

g <- list(size=6, level='individual')

res <- simulx(model     = model.pharmml,
              parameter = p,
              output    = list(f,y,q),
              treatment = adm,
              group     = g,
              settings  = list(seed=12345))

print(res$parameter)

print(ggplotmlx() + geom_line(data=res$PSTAR, aes(x=time, y=PSTAR)) +
  geom_point(data=res$PSTAR_obs , aes(x=time, y=PSTAR_obs ),colour="red") +
  facet_wrap( ~ id) + xlab("time (month)") + ylab("tumor size") )
