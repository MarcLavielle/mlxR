setwd(dirname(parent.frame(2)$ofile)) # set working directory to source file location

project.file <- 'monolixRuns/theophylline_project.mlxtran'  #relative path

monolix2simulx(project=project.file,open=TRUE)
monolix2simulx(project=project.file, parameter="mode",open=TRUE)
monolix2simulx(project=project.file, parameter=c(a=0, b=0),open=TRUE)
monolix2simulx(project=project.file, parameter=list("mean",c(a=0, b=0)),open=TRUE)

d <- readDatamlx(project=project.file)
names(d)
r <- simulx(project=project.file)
names(r)

monolix2simulx(project='monolixRuns/warfarin_PKPD_project.mlxtran',open=TRUE)
monolix2simulx(project='monolixRuns/warfarin_cat_project.mlxtran',open=TRUE)
monolix2simulx(project='monolixRuns/PKrtte_project.mlxtran',open=TRUE)
