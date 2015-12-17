# setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------
adm1 <- list(time=seq(24,to=143,by=24),amount=100)
adm2 <- list(time=seq(24,to=143,by=12),amount= 50)

p <- list(name=c('w_pop','omega_w','Tk0_pop','omega_Tk0','V_pop','omega_V',
                 'beta_V','k_pop','omega_k','beta_k','ke0_pop','omega_ke0',
                 'Imax_pop','omega_Imax','S0_pop','omega_S0','IC50_pop',
                 'omega_IC50','kout_pop','omega_kout','a1','a2','a3','a4'), 
          value=c(70, 10, 4, 0.2, 10, 0.3, 1, 0.2, 0.2, -0.25, 0.05, 0.1, 0.5, 
                  1, 100, 0.1, 1,0.2, 0.1, 0.1, 0.1, 2, 2, 2))

ind <- list(name=c('w','Tk0','V','k','ke0','Imax','S0','IC50','kout'))
f <- list(name=c('Cc','PCA1','PCA2','PCA3'), time=seq(0,to=200,by=1))
y <- list(name=c('y1','y2','y3','y4'), time=seq(1,to=200,by=5))
out <- list(ind, f, y)

g1 <- list(size=30, level='covariate', treatment=adm1);
g2 <- list(size=20, level='covariate', treatment=adm2);
g <- list(g1,g2)

res <- simulx(model='pkpd2_model.txt',parameter=p,output=out,group=g);


#--------------------------------------------------------
plot1=ggplotmlx(data=res$Cc, aes(x=time, y=Cc, group=id, colour=group)) +  geom_line(size=0.5) +
  xlab("time (month)") + ylab("Cc") + theme(legend.position="none")

plot2=ggplotmlx(data=res$PCA1, aes(x=time, y=PCA1, group=id, colour=group)) +  geom_line(size=0.5) +
  xlab("time (month)") + ylab("PCA1") 

plot3=ggplotmlx(data=res$PCA2, aes(x=time, y=PCA2, group=id, colour=group)) + geom_line(size=0.5) +
  xlab("time (month)") + ylab("PCA2") + theme(legend.position="none")

plot4=ggplotmlx(data=res$PCA3, aes(x=time, y=PCA3, group=id, colour=group)) + geom_line(size=0.5) +
  xlab("time (month)") + ylab("PCA3") 

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

print(res$parameter)
