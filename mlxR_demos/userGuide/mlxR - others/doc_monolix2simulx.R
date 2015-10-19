setwd(dirname(parent.frame(2)$ofile))

project.file <- 'monolixRuns/theophylline_project.mlxtran'
monolix2simulx(project.file)

project.file <- 'monolixRuns/warfarin_project.mlxtran'
monolix2simulx(project.file, parameter="mean")

project.file <- 'monolixRuns/pkcat_project.mlxtran'
monolix2simulx(project.file)

project.file <- 'monolixRuns/pkrtte_project.mlxtran'
monolix2simulx(project.file)
