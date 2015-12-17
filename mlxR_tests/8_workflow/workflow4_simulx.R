setwd(dirname(parent.frame(2)$ofile))
library(XML)
library(ggplot2)
library(gridExtra)

#-------------------------------------

adm <- list( amount = 50, time = seq(-240, 96, by=24 ));
out <- list( name = c('Cc','E'), time = seq(0, 200, by=0.5));
res <- simulx( project   = "projects/warfarinPKPD_project.mlxtran", 
               output    = out, 
               treatment = adm)

#------------------------------------------
plot1 <- ggplotmlx() + 
  geom_point(data=res$conc, aes(x=time, y=conc, colour=id)) +
  geom_line( data=res$Cc, aes(x=time, y=Cc, colour=id)) +
  theme(legend.position="none") + ylab("concentration(mg/l)")
plot2 <- ggplotmlx() + 
  geom_point(data=res$pca, aes(x=time, y=pca, colour=id)) +
  geom_line( data=res$E, aes(x=time, y=E, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)

