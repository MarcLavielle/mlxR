# File generated automatically on 2016-01-04 18:22:48
 
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"warfarin_seq_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
list.param <- list(populationParameter)
# output 
name<-"PCA"
time<-read.table("output.txt",header=TRUE)
out<-list(name=name,time=time) 

# regressor 
regressor <-read.table("regressor.txt", header = TRUE)

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out,regressor=regressor)
