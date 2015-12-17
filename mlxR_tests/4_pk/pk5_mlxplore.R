setwd(dirname(parent.frame(2)$ofile))

adm <- list(time=c(6, 15), amount=100)
Cc  <- list(name='Cc',time=seq(from=0, to=30, by=0.1))
p   <- list(name=c('F0','Tk0','ka','V','Cl'), value=c(0.3,3,0.6,10,0.5))
mlxplore(model='pk5_model.txt', parameter=p, output=Cc, treatment=adm)
