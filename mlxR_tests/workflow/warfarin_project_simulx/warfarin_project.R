# File generated automatically on 2015-09-30 15:55:31
 
# # # library("mlxR")  
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"warfarin_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
individualCovariate<- read.table('individualCovariate.txt', header = TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
list.param <- list(individualCovariate,populationParameter)
# output 
name<-"y1"
time<-read.table("output1.txt",header=TRUE)
out1<-list(name=name,time=time) 
name<-"y2"
time<-read.table("output2.txt",header=TRUE)
out2<-list(name=name,time=time) 
out<-list(out1,out2)

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out)
