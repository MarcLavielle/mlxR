setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

y <- list( name='y',
           time=seq(0, 1, by=0.1))

p <- list( name=c('u','v_pop','omega_v','a'),
           value=c(20,10,0.2,0.5))

g <- list( size=10,
           level='individual')


res <- simulx( model='intro2_model.txt',
               parameter=p,
               output=y,
               group=g)

plot1 <- ggplotmlx(data=res$y, aes(x=time, y=y, colour=id)) + 
         geom_line(size=1) + geom_point() 
print(plot1)

