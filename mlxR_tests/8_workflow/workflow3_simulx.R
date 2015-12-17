setwd(dirname(parent.frame(2)$ofile))
library(XML)
library(ggplot2)
library(gridExtra)

#-------------------------------------

out1 <- list( name = c("wt"))
out2 <- list( name = c('V','IC50'))
out3 <- list( name = c('Cc','E'), time = seq(0, 200, by=0.5))

res <- simulx( project = "projects/warfarinPKPD_project.mlxtran", 
               output  = list(out1, out2, out3))
              
print(res$parameter)

plot1 <- ggplotmlx() + 
  geom_point(data=res$conc, aes(x=time, y=conc, colour=id)) +
  geom_line( data=res$Cc, aes(x=time, y=Cc, colour=id)) +
  theme(legend.position="none") + ylab("concentration(mg/l)")
plot2 <- ggplotmlx() + 
  geom_point(data=res$pca, aes(x=time, y=pca, colour=id)) +
  geom_line( data=res$E, aes(x=time, y=E, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)
