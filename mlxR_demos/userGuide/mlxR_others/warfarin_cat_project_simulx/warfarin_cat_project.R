# File generated automatically on 2016-04-01 12:27:58
 
library(mlxR)  
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"warfarin_cat_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
individualCovariate<- read.table('individualCovariate.txt', header = TRUE) 
list.param <- list(populationParameter,individualCovariate)
# output 
name<-"Concentration"
time<-read.table("output1.txt",header=TRUE)
out1<-list(name=name,time=time) 
name<-"Level"
time<-read.table("output2.txt",header=TRUE)
out2<-list(name=name,time=time) 
out<-list(out1,out2)

# call the simulator 
res <- simulx(model=model,treatment=trt,parameter=list.param,output=out)
