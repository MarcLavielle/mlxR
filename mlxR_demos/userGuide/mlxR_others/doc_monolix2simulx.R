setwd(dirname(parent.frame(2)$ofile)) # set working directory to source file location

project.file <- 'monolixRuns/theophylline1_project.mlxtran'  #relative path

p <-monolix2simulx(project=project.file)
p <- monolix2simulx(project=project.file,open=TRUE)
p <- monolix2simulx(project=project.file, parameter="mode",open=TRUE)
p <- monolix2simulx(project=project.file, parameter=c(a=0, b=0),open=TRUE)
p <- monolix2simulx(project=project.file, parameter=list("mean",c(a=0, b=0)),open=TRUE)

d <- readDatamlx(project=project.file)
names(d)
r <- simulx(project=project.file)
names(r)

p <- monolix2simulx(project='monolixRuns/warfarin_PKPD_project.mlxtran',open=TRUE)
p <- monolix2simulx(project='monolixRuns/warfarin_cat_project.mlxtran',open=TRUE)
p <- monolix2simulx(project='monolixRuns/PKrtte_project.mlxtran',open=TRUE)
