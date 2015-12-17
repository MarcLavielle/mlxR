# setwd(dirname(parent.frame(2)$ofile))

#-------  Get required libraries
library(gridExtra)
library(plyr)
# # # library("mlxR")


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
#

sim.param <- c(b=0)
out  <- list(name = 'Cc', time = seq(0, 25, by=0.1))
sim.res2  <- simulx(project   = project.file,
                    output    = out,
                    parameter = sim.param)

print(ggplotmlx() + 
        geom_point(data=sim.res2$y1, aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res2$Cc, aes(x=time, y=Cc, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))

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

sim.res6  <- simulx(project   = project.file,
                    treatment = adm,
                    output    = list(out1, out2))


print(ggplotmlx() + 
        geom_point(data=sim.res6$y1,aes(x=time, y=y1, colour=id)) +
        geom_line(data=sim.res6$Cc,aes(x=time, y=Cc, colour=id)) +
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
weight <- list( name = 'WEIGHT', 
                colNames = c('id', 'WEIGHT'),
                value  = cbind(c(1:N),c(rep(50, N/2), rep(90, N/2))))
# weight <- list( name = 'WEIGHT', 
#                 value  = c(rep(50, N/2), rep(90, N/2)))
#psex <- list( name = 'SEX',    value  = c(rep('F',N/2), rep('M',N/2)))

adm   <- list(time = 0, amount = 500)

sim.res7 <- simulx(project = project.file, 
                   output = list(out1, out2),
                   treatment = adm,
                   parameter = weight)

sim.res7$Cc$weight <- 50
sim.res7$Cc$weight[as.numeric(sim.res7$Cc$id)>N/2] <-90
sim.res7$Cc$weight <- as.factor(sim.res7$Cc$weight)

print(ggplotmlx() + 
        geom_line(data=sim.res7$Cc,aes(x=time, y=Cc, by=id, colour=weight)) +
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
sim.weight <- list( name = 'WEIGHT', 
                    colNames = c('id', 'WEIGHT'),
                    value  = cbind(c(1:N),rlnorm(N, log(70), 0.2)))

outy  <- list(name = c('y1'), time = seq(0, 140, by=6))
outi  <- c('WEIGHT', 'ka', 'V', 'Cl')

Dose.amount <- c(50, 100, 250, 500, 1000)
Dose.times  <- seq(from = 0, to = 120, by = 12)

#-- Use the same patients for each of the dose levels
s  <- list(seed = 123456)

#-- run simulx with each dose level
sim.data <- NULL
for(n.c in 1:length(Dose.amount)){
	adm   <- list(time = Dose.times, amount = Dose.amount[n.c])

	tmp <- simulx(project   = project.file,
	              output    = list(outy, outi),
	              treatment = adm,
	              parameter = sim.weight,
	              settings  = s)
	
	tmp2 			<- tmp$y1
	tmp2['Dose'] 	<- Dose.amount[n.c]
	sim.data <- rbind(sim.data, tmp2)
}

#-- Compute statistics

sim.data.stat <- ddply(sim.data, .(time, Dose), summarize,
	median = median(y1),
	p05  = quantile(y1, 0.05),
	p95  = quantile(y1, 0.95),
	Dose = Dose[1]
)

sim.data.stat.ss <- sim.data.stat[sim.data.stat$time == 120, ] 

print(ggplotmlx(data=sim.data.stat.ss) +
  geom_line(aes(x=Dose, y=median)) +
  geom_point(aes(x=Dose, y=median)) +
  geom_ribbon(aes(x=Dose, ymin=p05, ymax=p95), alpha = 0.3) +
  scale_x_continuous("Dose") + scale_y_continuous("Concentration"))


##########################################################
#			EXAMPLE 9
#
#  Run simulations to test how the sample size affects the 
#  prediction of the mean PK
#  For each sample size we run multiple trial simulation,
#  each with a new seed, and calulate the median
# 
#  To increase simulation speed the simulation design will
#  be pre-created and pre-loaded

out  <- list(name = 'y1', time = seq(0, 120, by=12))

Dose.amount <- 300
Dose.times  <- seq(from = 0, to = 120, by = 12)

#-- Define number of trial simulations
N.trial <- 200

#-- Generate seeds for each trial simulation
seed = 123456 + seq(from=1, to=N.trial)

#-- Define the number of patients to simulate
N <- c(10, 30, 50, 100)

sim.data <- NULL
for(n.p in 1:length(N)){
	#-- Generate individual weights
	sim.weight <- list( name     = 'WEIGHT', 
	                    colNames = c('id', 'WEIGHT'),
	                    value    = cbind(c(1:N[n.p]),rlnorm(N[n.p], log(70), 0.2)))
	
	for(n.c in 1:length(Dose.amount)){	
		cat("N = ",N[n.p],"  ;  Dose = ",Dose.amount[n.c],"\n")
		adm   <- list(time = Dose.times, amount = Dose.amount[n.c])

		#-- Here we create and pre-load the design to run the simulations faster
		s <- list(data.in=TRUE, load.design=TRUE)

		dataIn <- simulx(project   = project.file,
			      	       output    = out,
			     		       treatment = adm,
					           parameter = sim.weight,
					           settings  = s)

		for(n.t in 1:N.trial){
		  s <- list(seed=seed[n.t], load.design=FALSE)
		  
		  #-- Run simulation with created and loaded design
		  tmp1 <- simulx(data = dataIn, setting=s)
		  tmp2 <- tmp1$y1
		  tmp2['Dose'] 	<- Dose.amount[n.c]
		  tmp2['Trial.rep'] <- n.t
		  tmp2['N.patients']<- N[n.p]
		  
		  sim.data <- rbind(sim.data, tmp2)
		}
	}
}

#--- Compute statistics
#
sim.data.stat <- ddply(sim.data, .(time, Dose, Trial.rep, N.patients), summarize,
	median = median(y1),
	p05 = quantile(y1, 0.05),
	p95 = quantile(y1, 0.95),
	Dose = Dose.amount[1]
)

sim.data.stat.ss <- sim.data.stat[sim.data.stat$time == 120, ] 

print(ggplotmlx(data=sim.data.stat.ss) +
	geom_point(aes(x=N.patients, y=median)))




