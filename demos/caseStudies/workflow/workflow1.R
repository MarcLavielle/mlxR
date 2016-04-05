setwd(dirname(parent.frame(2)$ofile))

#-------  Get required libraries
library(gridExtra)
library(plyr)
library(reshape2)
# library(mlxR)


#-------  Define project to be used for simulations
#
project.file <- 'monolixRuns/theophylline_project.mlxtran'


##########################################################
#    	EXAMPLE 1
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#  Covariate WEIGHT is taken from data set
#  Dosage regimen and observation times are taken from data set
#
sim.res1  <- simulx(project = project.file)

print(ggplotmlx(data=sim.res1$y1) + 
        geom_point(aes(x=time, y=y1, colour=id)) +
        geom_line(aes(x=time, y=y1, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

##########################################################
#    	EXAMPLE 2
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#  Covariate WEIGHT is taken from data set
#  Dosage regimen and observation times are taken from data set
#    - and switch off the residual error by setting b = 0


sim.param <- c(b=0.0)
out1  <- list(name = 'Cc', time = seq(0, 25, by=0.1))
outp <- c("ka","V", "Cl","WEIGHT")
sim.res2a  <- simulx(project   = project.file,
                    output    = list(out1,outp),
                    parameter = sim.param)
names(sim.res2a)
head(sim.res2a$parameter)

print(ggplotmlx() + 
        geom_point(data=sim.res2a$y1, aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res2a$Cc, aes(x=time, y=Cc, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

#--  define the observation times
out2  <- list(name = 'y1', time = seq(1, 25, by=2))
sim.res2b  <- simulx(project   = project.file,
                     output    = list(out1, out2),
                     parameter = sim.param)

print(ggplotmlx() + 
        geom_point(data=sim.res2b$y1, aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res2b$Cc, aes(x=time, y=Cc, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

#-- remove the simulated concentrations from the outputs
out0 <- list(name='y1', time='none')
sim.res2c  <- simulx(project   = project.file,
                     output    = list(out1,out0))
names(sim.res2c)

#-- Use the estimated individual parameters (mode = EBE)
sim.param <- "mode"
sim.res2d  <- simulx(project   = project.file,
                     output    = out1,
                     parameter = sim.param)

#-- Use the estimated individual parameters and set b=0
sim.param <- list("mode",c(b=0))
sim.res2e  <- simulx(project   = project.file,
                     output    = out1,
                     parameter = sim.param)

sim.res2f  <- simulx(project   = project.file,
                     settings  = list(out.trt=F))
##########################################################
#      EXAMPLE 3
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#     -  simulate a trial with N individuals: the designs and the weights 
#        of these N patients are sampled from the original dataset 
#        with replacement 

N <- 50
sim.res3  <- simulx(project = project.file,
                    group   = list(size = N))

print(ggplotmlx(data=sim.res3$y1) + 
        geom_point(aes(x=time, y=y1, colour=id)) +
        geom_line(aes(x=time, y=y1, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration") +
        theme(legend.position="none"))


##########################################################
#			EXAMPLE 4
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#  Covariate WEIGHT and dosage regimen are taken from data set
#   -  and define observation times for a given output
#	
out  <- list(name = 'y1', time = (0:12))

sim.res4  <- simulx(project = project.file,
                    output = out)

print(ggplotmlx(data=sim.res4$y1) + 
        geom_point(aes(x=time, y=y1, colour=id)) +
        geom_line(aes(x=time, y=y1, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

##########################################################
#			EXAMPLE 5
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#  Covariate WEIGHT and dosage regimen are taken from data set
#   -  and define observation times for several given outputs
#   -  and provide the indiviual parameters and covariates 
#      in the output
#

out1  <- list(name = 'y1', time = seq(0, 24, by=2))
out2  <- list(name = 'Cc', time = seq(0, 24, by=0.1))
out3  <- list(name = c('V', 'Cl', 'WEIGHT'))

sim.res5  <- simulx(project = project.file,
                    output  = list(out1, out2, out3))

print(sim.res5$parameter)

print(ggplotmlx() + 
        geom_point(data=sim.res5$y1,aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res5$Cc,aes(x=time, y=Cc, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))


##########################################################
#  		EXAMPLE 6
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#  Covariate WEIGHT is taken from data set
#   -  and define observation times for several given outputs
#   -  and specify the administration schedule
#

adm   <- list(time = c(0,6), amount = c(320, 320))

sim.res6a  <- simulx(project   = project.file,
                    treatment = adm,
                    output    = list(out1, out2))

print(ggplotmlx() + 
        geom_point(data=sim.res6a$y1,aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res6a$Cc,aes(x=time, y=Cc, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

#-- simulate a trial with N individuals 
sim.res6b  <- simulx(project   = project.file,
                     group = list(size=N),
                     treatment = adm,
                     output    = list(out1, out2))

print(ggplotmlx() + 
        geom_point(data=sim.res6b$y1,aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res6b$Cc,aes(x=time, y=Cc, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration") +  
        theme(legend.position="none"))

#--  define 2 arms with different treatments
g1 <- list(size=10, treatment=list(time=0, amount=200))
g2 <- list(size=5,  treatment=list(time=6, amount=300))
sim.res6c  <- simulx(project = project.file,
                     group   = list(g1, g2),
                     output  = list(out1, out2))

print(ggplotmlx() + 
        geom_point(data=sim.res6c$y1,aes(x=time, y=y1, colour=group)) +
        geom_line(data=sim.res6c$Cc,aes(x=time, y=Cc, by=id,colour=group)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))


##########################################################
#			EXAMPLE 7
#
#  Simulate model with individual parameters sampled from the 
#  estimated population distribution
#   -  and define observation times for several given outputs
#   -  and specify the administration schedule
#   -  and specify own WEIGHT covariates	
#   -  and increase patient number from the original 12 
#      to 100.

N 		<- 100
weight <- data.frame(id = (1:N), WEIGHT = c(rep(50, N/2), rep(90, N/2)))
outw  <- "WEIGHT"
adm   <- list(time = 0, amount = 500)

sim.res7 <- simulx(project = project.file, 
                   output = list(out0, out2, outw),
                   treatment = adm,
                   parameter = weight)

names(sim.res7)
r <- merge(sim.res7$Cc, sim.res7$parameter)
r$weight <- as.factor(r$WEIGHT)

print(ggplotmlx(data=r) + 
        geom_line(aes(x=time, y=Cc, by=id, colour=weight)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

##########################################################
#			EXAMPLE 8
#
#  1) Simulate model with individual parameters sampled from the 
#  estimated population distribution
#   -  and define observation times for a given output
#   -  and provide the indiviual parameters and covariates 
#      in the output
#   -  and specify own WEIGHT covariates	
#   -  and increase patient number from the original 12 
#      to 100.
#   -  and specify several administration schedules
# 
#  2) Derive the dose-concentration relationship
#

#-- Define the number of patients to simulate
N <- 100

#-- Generate individual weights by sampling a lognormal distribution
sim.weight <- data.frame( id=cbind(c(1:N), WEIGHT=rlnorm(N, log(70), 0.2)) )

#-- Define the output to compute: quantiles 5%, 50%, 95%
outy  <- list(name = c('y1'), time = 120, 
              FUN = "quantile",probs = c(0.05, 0.5, 0.95))

Dose.amount <- c(50, 100, 250, 500, 1000)
Dose.times  <- seq(from = 0, to = 120, by = 12)

#-- Use the same patients for each of the dose levels
s  <- list(seed = 123456)

#-- run simulx with each dose level
sim.data <- NULL
for(n.c in 1:length(Dose.amount)){
  adm   <- list(time = Dose.times, amount = Dose.amount[n.c])
  
  tmp <- simulx(project   = project.file,
                output    = outy,
                treatment = adm,
                parameter = sim.weight,
                settings  = s)
  
  tmp2 			<- tmp$y1
  tmp2['Dose'] 	<- Dose.amount[n.c]
  sim.data <- rbind(sim.data, tmp2)
}

print(ggplotmlx(data=sim.data) +
        geom_line(aes(x=Dose, y=y1.p50)) +
        geom_point(aes(x=Dose, y=y1.p50)) +
        geom_ribbon(aes(x=Dose, ymin=y1.p5, ymax=y1.p95), alpha = 0.3) +
        scale_x_continuous("Dose") + scale_y_continuous("Concentration"))


##########################################################
#			EXAMPLE 9
#
#  Run simulations to test how the sample size affects the 
#  prediction of the mean PK
#  For each sample size we run multiple trial simulation,
#  each with a new seed, and calculate several percentiles 
# 
#  To increase simulation speed the simulation design will
#  be pre-created and pre-loaded

#-- Define the treatment
adm   <- list(time = seq(from = 0, to = 120, by = 12), amount = 300)

#-- Define the number of trial simulations
N.trial <- 100

#-- Define the number of patients to simulate
N <- c(20, 50, 100)

#-- Define the output to compute: quantiles 5%, 50%, 95%
outy  <- list(name = c('y1'), time = 120, 
              FUN = "quantile",probs = c(0.05, 0.5, 0.95))

sim.data <- NULL
for(n.p in 1:length(N)){
  cat("N = ",N[n.p],"\n")
  #-- Generate individual weights
  sim.weight <- data.frame(id=(1:N[n.p]),WEIGHT=rlnorm(N[n.p], log(70), 0.2))
  
  tmp1 <- simulx(project   = project.file,
                 output    = outy,
                 treatment = adm,
                 parameter = sim.weight,
                 nrep      = N.trial)
  
  tmp2 <- tmp1$y1
  tmp2$N.patients<- N[n.p]
  tmp2$rep=tmp2$time=NULL
  sim.data <- rbind(sim.data, tmp2)
}

#-- plot the median
print(ggplotmlx(data=sim.data) + geom_point(aes(x=N.patients, y=y1.p50)))

#-- plot the median and the quantiles 
d <- melt(data=sim.data, id="N.patients", variable.name="percentile")
print(ggplotmlx(data=d) +  geom_point(aes(x=N.patients, y=value, colour=percentile)))



