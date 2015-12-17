
#-------------------------------------
p <- c(Vs=10, gV=0.1, ws=70, gw=10, omega_w=12, omega_V=0.15, beta=1, k=0.15, a=0.5)
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
ind <- list(name=c('w','V'))
pop <- list(name=c('w_pop','V_pop'))
out <- list(pop, ind, f, y)

res <- simulx(model     = 'model/hierarchical3.txt', 
              parameter = p, 
              output    = out,
              settings  = list(seed = 123456))

print(res$parameter)

plot1 <- ggplotmlx(data=res$f, aes(x=time, y=f)) + geom_line(size=1) + 
  geom_point(data=res$y, aes(x=time, y=y), colour="red") 
print(plot1)


#-------------------------------------
g <- list(size=c(2,3),
          level=c('population','covariate'))

res <- simulx(model     = 'model/hierarchical3.txt', 
              parameter = p, 
              output    = out,
              group     = g,
              settings  = list(seed = 123456))

print(res$parameter)

plot1 <- ggplotmlx(data=res$f, aes(x=time, y=f, colour=id)) + 
  geom_line(size=1) + 
  geom_point(data=res$y, aes(x=time, y=y, colour=id)) 
print(plot1)

#-------------------------------------
g <- list(size=c(2,3),
          level=c('population','individual'))

res <- simulx(model     = 'model/hierarchical3.txt', 
              parameter = p, 
              output    = out,
              group     = g,
              settings  = list(seed = 123456))

print(res$parameter)


