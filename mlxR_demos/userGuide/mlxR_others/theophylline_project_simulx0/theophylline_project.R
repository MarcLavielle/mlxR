# File generated automatically on 2015-12-30 23:31:04
 
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"theophylline_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
individualCovariate<- read.table('individualCovariate.txt', header = TRUE) 
individualParameter<- read.table('individualParameter.txt', header = TRUE) 
list.param <- list(populationParameter,individualCovariate,individualParameter)
# output 
name<-"y1"
time<-read.table("output.txt",header=TRUE)
out<-list(name=name,time=time) 

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out)
