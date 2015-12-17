setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)

#-------------------------------------

out1 <- list( name = 'conc', time = seq(0, 100, by=5))
out2 <- list( name = 'pca',  time = c(0,5,10,15,seq(20, 100, by=20)))


res <- simulx( project = "projects/warfarinPKPD_project.mlxtran", 
               group   = list(size=10),
               output  = list(out1,out2))


#------------------------------------------
plot1 <- ggplotmlx() + 
  geom_point(data=res$conc, aes(x=time, y=conc, colour=id)) +
  geom_line( data=res$conc, aes(x=time, y=conc, colour=id)) +
  theme(legend.position="none") + ylab("concentration(mg/l)")
plot2 <- ggplotmlx() + 
  geom_point(data=res$pca, aes(x=time, y=pca, colour=id)) +
  geom_line( data=res$pca, aes(x=time, y=pca, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)
