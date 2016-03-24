setwd(dirname(parent.frame(2)$ofile))
#-------  Get required libraries
library(gridExtra)

project.file <- 'monolixRuns/warfarin_project.mlxtran'

#-------------------------------------
res <- simulx(project=project.file)

plot1 <- ggplotmlx() + geom_point(data=res$y1, aes(x=time, y=y1, colour=id)) +
  geom_line( data=res$y1, aes(x=time, y=y1, colour=id)) +
  theme(legend.position="none") + ylab("concentration (mg/l)")
plot2 <- ggplotmlx() + geom_point(data=res$y2, aes(x=time, y=y2, colour=id)) +
  geom_line( data=res$y2, aes(x=time, y=y2, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)


#-------------------------------------
out2 <- list( name = c('Tlag','ka','V','Cl','IC50'))
res <- simulx( project   = project.file, 
               output    = out2, 
               parameter = 'mode')

print(res$parameter)

plot1 <- ggplotmlx() + 
  geom_point(data=res$y1, aes(x=time, y=y1, colour=id)) +
  geom_line( data=res$y1, aes(x=time, y=y1, colour=id)) +
  theme(legend.position="none") + ylab("concentration(mg/l)")
plot2 <- ggplotmlx() + 
  geom_point(data=res$y2, aes(x=time, y=y2, colour=id)) +
  geom_line( data=res$y2, aes(x=time, y=y2, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)

#-------------------------------------
out <- list( name = c('Cc','E'), time = seq(0, 200, by=0.5));
res <- simulx( project   = project.file, 
               output    = out, 
               parameter = list(c(a_1=0,b_1=0,a_2=0)))

plot1 <- ggplotmlx() + 
  geom_point(data=res$y1, aes(x=time, y=y1, colour=id)) +
  geom_line( data=res$Cc, aes(x=time, y=Cc, colour=id)) +
  theme(legend.position="none") + ylab("concentration(mg/l)")
plot2 <- ggplotmlx() + 
  geom_point(data=res$y2, aes(x=time, y=y2, colour=id)) +
  geom_line( data=res$E, aes(x=time, y=E, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)


#-------------------------------------
N   <- 20
adm <- list( amount = 25, time = seq(-240, 96, by=24 ));
out <- list( name = c('Cc','E'), time = seq(0, 200, by=0.5));
res <- simulx( project   = project.file, 
               output    = out, 
               treatment = adm,
               group     = list(size=N))

plot1 <- ggplotmlx() + geom_point(data=res$y1, aes(x=time, y=y1, colour=id)) +
  geom_line( data=res$Cc, aes(x=time, y=Cc, colour=id)) +
  theme(legend.position="none") + ylab("concentration (mg/l)")
plot2 <- ggplotmlx() + geom_point(data=res$y2, aes(x=time, y=y2, colour=id)) +
  geom_line( data=res$E, aes(x=time, y=E, colour=id)) +
  theme(legend.position="none") + ylab("PCA (%)")
grid.arrange(plot1,plot2,ncol=2)
