# File generated automatically on 2016-04-01 12:27:52
 
library(mlxR)  
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"theophylline1_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
list.param <- list(populationParameter)
# output 
name<-"concentration"
time<-read.table("output.txt",header=TRUE)
out<-list(name=name,time=time) 

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out)
