setwd(dirname(parent.frame(2)$ofile)) # set working directory to source file location

d <- readDatamlx(project='monolixRuns/warfarin_PKPD_project.mlxtran')
names(d)
head(d$treatment)
head(d$covariate)
head(d$y1)


#-- reserved key-words for the header:
#   ID,TIME,AMT,ADM,RATE,TINF,Y,YTYPE,X,COV,CAT,OCC,MDV,EVID,ADDL,SS,II,IGNORE

d <- readDatamlx(datafile='monolixRuns/data/warfarin_data.txt', 
                 header=c('id','time','amt','y','ytype','cov','cov','cat'))
names(d)
head(d$treatment)
head(d$covariate)
head(d$y1)


d <- readDatamlx(datafile='monolixRuns/data/warfarin_data.txt', 
                 header=c('id','time','amt','y','ytype','cov','ignore','ignore'))
head(d$covariate)

d <- readDatamlx(datafile='data/warfarin_data_evid.txt', 
                 header=c('id','time','amt','y','ytype','cov','cov','cat','evid'))

d <- readDatamlx(datafile='data/warfarin_data_mdv.txt', 
                 header=c('id','time','amt','y','ytype','cov','cov','cat','mdv'))
head(d$y1)

d <- readDatamlx(datafile="data/dataSS.csv",  
                 header=c('time','y','amt','ss','ii'))
d$treatment

d <- readDatamlx(datafile="data/dataSS.csv",  
                 header=c('time','y','amt','ss','ii'), addl.ss=5)
d$treatment

d <- readDatamlx(datafile="data/dataADDL1.csv",  
                 header=c('time','y','amt','addl','ii'))
d$treatment

d <- readDatamlx(datafile="data/dataADDL2.csv",  
                 header=c('time','y','amt','addl','ii'))
d$treatment
