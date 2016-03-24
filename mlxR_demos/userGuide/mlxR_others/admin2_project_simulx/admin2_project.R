# File generated automatically on 2016-01-04 17:05:53
 
library(mlxR)  
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"admin2_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
list.param <- list(populationParameter)
# output 
name<-"y1"
time<-read.table("output.txt",header=TRUE)
out<-list(name=name,time=time) 

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out)
