setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)

#-------------------------------------

out2 <- list( name = c('Tlag','ka','V','Cl','IC50'))
              
res <- simulx( project   = "projects/warfarinPKPD_project.mlxtran", 
               output    = out2, 
               parameter = 'mean')

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
