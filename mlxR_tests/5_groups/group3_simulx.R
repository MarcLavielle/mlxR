setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time   = 0, amount = 100)
p <- list(name  = c('Vs','gV','ks','gk','omega_V','w_pop','omega_w','a'), 
          value = c(10,3,0.2,0.3,0.04,70,10,0.5))
 g <- list(size  = c(2,3), level = c('population','covariate'));
#  g <- list(size  = c(1,2,3,1), level = c('longitudinal','population','covariate','individual'));
# g <- list(size  = c(1,1,3,2))
y <- list(name = 'y', time = seq(0, 10, by=1))
s <- list(name=c("k", "V_pop","w", "V"))

res <- simulx(model='group3_model.txt',parameter = p, 
              output=list(y,s),treatment=adm,group=g,settings=list(seed=12345))

print(res$param)
plot1 <- ggplotmlx(data=res$y, aes(x=time, y=y, colour=id)) + 
         geom_line(size=1) + geom_point() 
print(plot1)

