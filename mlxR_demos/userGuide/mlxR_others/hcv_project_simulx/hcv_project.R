# File generated automatically on 2016-01-04 20:09:37
 
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"hcv_project_model.txt"

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
individualCovariate<- read.table('individualCovariate.txt', header = TRUE) 
list.param <- list(populationParameter,individualCovariate)
# output 
name<-"y1"
time<-read.table("output.txt",header=TRUE)
out<-list(name=name,time=time) 

# regressor 
regressor <-read.table("regressor.txt", header = TRUE)

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out,regressor=regressor)
