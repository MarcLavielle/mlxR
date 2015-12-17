setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

adm <- list( time   = 4, 
             amount = 100)

p <- list( name  = c('w','pop_Tlag','omega_Tlag','pop_ka','omega_ka','pop_V','omega_V',
                     'beta_V','pop_Cl','omega_Cl','beta_Cl','a','b'), 
           value = c(70, 1, 0.3, 0.8, 0.2, 10, 0.3, 1, 1, 0.2, 0.75, 0., 0.1))

ind <- list( name = c('Tlag','ka','V','Cl'))
f   <- list( name = c('C'), 
             time = seq(0,to=60,by=1))
y   <- list( name = c('C_obs'), 
             time = seq(2,to=50,by=2))
out <- list(ind, y)

g <- list( size      = 4, 
           level     = 'individual', 
           treatment = adm);

res <- simulx( model     = 'model/pk_model.xml',
               parameter = p,
               output    = out,
               group     = g);


#--------------------------------------------------------
plot1=ggplotmlx() + 
  #geom_line(data=res$C, aes(x=time, y=C, colour=id)) +
  geom_point(data=res$C_obs, aes(x=time, y=C_obs, colour=id)) +
  xlab("time (h)") + ylab("concentration") 
print(plot1)

print(res$parameter)
