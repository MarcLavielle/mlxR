setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)

#-------------------------------------
p <- c(V_pop=10, omega_V=0.1, beta=1, w_pop=70, omega_w=12, k=0.15, a=0.5)

f   <- list(name='f', time=seq(0, 30, by=0.1))
y   <- list(name='y', time=seq(1, 30, by=3))
ind <- list(name=c('w','V'))
out <- list(ind, f, y)

res1 <- simulx(model     = 'model/hierarchical2.txt', 
               parameter = p, 
               output    = out,
               settings  = list(seed = 123456))

print(res1$parameter)

plot(ggplotmlx() + 
  geom_line( data=res1$f, aes(x=time, y=f), colour="black") + 
  geom_point(data=res1$y, aes(x=time, y=y), colour="red"))

#-------------------------------------
g <- list( size = 5, level = 'covariate')

res2 <- simulx(model     = 'model/hierarchical2.txt', 
               parameter = p, 
               output    = out,
               group     = g,
               settings  = list(seed = 123456))

print(res2$parameter)

plot(ggplotmlx() + geom_line(data=res2$f, aes(x=time, y=f, colour=id), size=0.75) + 
       geom_point(data=res2$y, aes(x=time, y=y, colour=id), size=2))

#-------------------------------------
g <- list( size  = c(2,3), level = c('covariate','individual'))

res3 <- simulx(model     = 'model/hierarchical2.txt', 
               parameter = p, 
               output    = out,
               group     = g,
               settings  = list(seed = 1234))

print(res3$parameter)

#-------------------------------------
g <- list( size  = c(2,2,2), level = c('covariate','individual','longitudinal'))

res4 <- simulx(model     = 'model/hierarchical2.txt', 
               parameter = p, 
               output    = out,
               group     = g,
               settings  = list(seed = 123123))

print(res4$parameter)

plot(ggplotmlx() + geom_line(data=res4$f, aes(x=time, y=f, colour=id), size=0.75) + 
  geom_point(data=res4$y, aes(x=time, y=y, colour=id), size=2))

