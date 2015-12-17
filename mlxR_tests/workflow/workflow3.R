setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)
project.file <- 'monolixRuns/pkcat_project.mlxtran'

#-------------------------------------

# simulate continuous PK and categorical data for 100 patients 
N=100
out  <- list(name = c('conc','level'), time = c(0,2,4,6,seq(12, 180, by=12)))
p <- c(a_1=0, b_1=0)

res <- simulx(project=project.file,
              group     = list(size = N), 
              output    = out)

# plot the simulated PK data 
plot1 <- ggplotmlx(data=res$conc) + geom_point(aes(x=time, y=conc, colour=id)) +
  geom_line(aes(x=time, y=conc, colour=id)) +
  theme(legend.position="none") + ylab("concentration (mg/l)")

# plot the distribution of the simulated categorical PD data
plot2 <- catplotmlx(res$level)

grid.arrange(plot1, plot2, ncol=2)
