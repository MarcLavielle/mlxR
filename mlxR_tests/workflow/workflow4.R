# setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)

project.file <- 'monolixRuns/pkrtte_project.mlxtran'
#-------------------------------------

# Simulate continuous data (PK) and repeated events (Hemorrhaging) for 200 patiens, 

N=200

out  <- list(name = c('Cc'), time = seq(0,150,by=0.5))
res <- simulx(project=project.file,
              group     = list(size = N),
              output    = out)

# Plot the simuated PK data and the predicted concentrations
plot1 <- ggplotmlx() + 
  geom_line(data=res$Cc, aes(x=time, y=Cc, group=id), colour="black") +
  geom_point(data=res$Concentration, aes(x=time, y=Concentration), colour="red") +
  theme(legend.position="none") + ylab("concentration (mg/l)")
print(plot1)

# Plot the survival function for the first and second events
plot2a <- kmplotmlx(res$Hemorrhaging) + ylim(c(0,1))
plot2b <- kmplotmlx(res$Hemorrhaging, index=2) + ylim(c(0,1))
grid.arrange(plot2a,plot2b,ncol=2)

#------------------------------------------------------------
# Add the label of the treatment group to the simulated data
source("wftools.R")
rh <- addgroup(res$Hemorrhaging, res$treatment,"amt")
ry <- addgroup(res$Concentration,res$treatment,"amount")  
rc <- addgroup(res$Cc,res$treatment,"amount")  

# Plot the simulated PK data with different colors per treatment group
plot3 <- ggplotmlx() + 
  geom_line(data=rc, aes(x=time, y=Cc, group=id, colour=group)) +
  geom_point(data=ry, aes(x=time, y=Concentration, colour=group)) +
  theme(legend.position="none") + ylab("concentration (mg/l)")
print(plot3)

# Plot the survival functions with different colors per treatment group
plot4a <- kmplotmlx(rh) + ylim(c(0,1)) + ylab("Survival (first event)") + theme(legend.position=c(0.1,0.15))
plot4b <- kmplotmlx(rh, index=2) + ylim(c(0,1)) + ylab("Survival (second event)") + theme(legend.position=c(0.1,0.15))
grid.arrange(plot4a,plot4b,ncol=2)

