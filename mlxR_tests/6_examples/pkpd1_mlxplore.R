setwd(dirname(parent.frame(2)$ofile))

adm1 <- list(time=seq(24,143,by=24),amount=100)
adm2 <- list(time=seq(24,143,by=12),amount= 50)
g1 <- list(treatment=adm1);
g2 <- list(treatment=adm2);

p <- list(name=c('Tk0','V','k','ke0','Imax','S0','IC50','kout'), 
          value=c(3, 10, 0.2, 0.05, 0.5, 100, 1, 0.1))

f <- list(name=c('Cc','PCA1','PCA2','PCA3'), time=seq(0,to=200,by=1))

mlxplore(model='pkpd1_model.txt',parameter=p,output=f,group=list(g1,g2));

