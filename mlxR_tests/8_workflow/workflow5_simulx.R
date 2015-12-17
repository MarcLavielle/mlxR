setwd(dirname(parent.frame(2)$ofile))
library(XML)

#-------------------------------------

p1 <- list( name  = c('V_pop','omega_V', 'beta_V_lw70'),
            value = c(10, 0.1, 0))

N=32
p2 <- list( name   = 'wt',
            colNames = c('id', 'wt'),
            value  = cbind(c(1:N), rnorm(N)*10+70));

res <- simulx( project   = "projects/warfarinPKPD_project.mlxtran", 
               output    = list(name=c('wt','V','IC50')), 
               parameter = list(p1,p2))

#------------------------------------------
print(res$parameter)

