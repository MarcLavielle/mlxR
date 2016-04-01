# File generated automatically on 2016-03-26 16:13:55
 
library(mlxR)  
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"theophylline_project_model.txt"

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
