setwd(dirname(parent.frame(2)$ofile)) # set working directory to source file location

project.file <- 'monolixRuns/theophylline1_project.mlxtran'  #relative path
pop1a <- simpopmlx(n=3, project=project.file)
print(pop1a, digits=3)

pop1b <- simpopmlx(n=3, project=project.file, fim="lin")
print(pop1b, digits=3)

project.file <- 'monolixRuns/theophylline2_project.mlxtran'  #relative path
pop2 <- simpopmlx(n=3, project=project.file)
print(pop2, digits=3)

project.file <- 'monolixRuns/theophylline3_project.mlxtran'  #relative path
pop3 <- simpopmlx(n=3, project=project.file)
print(pop3, digits=3)

param <- data.frame(pop.param=c(1.5, 0.5, 0.02, 0.4, 0.15, 0.2, 0.7),
                    sd=c(0.2, 0.05, 0.004, 0.05, 0.02, 0.02, 0.05),
                    trans=c('N','N','N','L','L','L','N'))
pop4a <- simpopmlx(n=3, parameter=param)
print(pop4a, digits=3)

param$sd=c(0.2, 0.05, 0., 0.05, 0.02, 0.02, 0.0)
pop4b <- simpopmlx(n=3, parameter=param)
print(pop4b, digits=3)

