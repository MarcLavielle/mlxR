setwd(dirname(parent.frame(2)$ofile))
library(XML)

#-------------------------------------

adm <- list( amount = 100, time = 0);

p1 <- list( name  = c('V_pop','omega_V', 'beta_V_lw70'),
            value = c(10, 0.1, 0))

N=10
p2 <- list( name   = 'wt',
            colNames = c('id', 'wt'),
            value  = cbind(c(1:N), rnorm(N)*10+70));

out1 <- list( name = 'conc', time = c(0,2.5,5,10,15,seq(20,200,by=10)))
out2 <- list( name = 'pca',  time = c(0,5,10,15,seq(20,200,by=20)))
out3 <- list( name=c('wt','V','IC50'))

res <- simulx( project   = "projects/warfarinPKPD_project.mlxtran", 
               output    = list(out1,out2,out3), 
               parameter = list(p1,p2),
               treatment = adm)

#------------------------------------------
print(res$parameter)

plot1 <- ggplotmlx() + 
  geom_point(data=res$conc, aes(x=time, y=conc, colour=id)) +
  geom_line( data=res$conc, aes(x=time, y=conc, colour=id)) +
  theme(legend.position="none") + ylab("concentration(mg/l)")
plot2 <- ggplotmlx() + 
  geom_point(data=res$pca, aes(x=time, y=pca, colour=id)) +
  geom_line( data=res$pca, aes(x=time, y=pca, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)

