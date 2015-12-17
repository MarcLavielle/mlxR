setwd(dirname(parent.frame(2)$ofile))


p <- list(name=c('A','lambda','gamma','epsilon','d','omega','tau'), 
          value=c(0.5, 0.4, 0.02, 0.1, 0.01, 0.3, 60))

out <- list(name=c('S', 'E', 'I', 'R'), time=-20:350)

mlxplore(model='seir_model.txt', parameter=p, output=out)