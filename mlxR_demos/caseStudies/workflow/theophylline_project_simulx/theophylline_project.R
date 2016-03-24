# File generated automatically on 2015-12-14 17:49:55
 
library(mlxR)  
 
setwd(dirname(parent.frame(2)$ofile)) 

# model 
model<-"theophylline_project_model.txt"

# treatment
trt <- read.table("treatment.txt", header = TRUE) 

# parameters 
originalId<- read.table('originalId.txt', header=TRUE) 
populationParameter<- read.vector('populationParameter.txt') 
individualCovariate<- read.table('individualCovariate.txt', header = TRUE) 
list.param <- list(populationParameter,individualCovariate)
# output 
name<-"y1"
time<-read.table("output.txt",header=TRUE)
out<-list(name=name,time=time) 
