
#-------------------------------------

p <- c(V_pop=10, omega_V=0.3, k=0.1, a=0.5)
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
V <- list(name='V')

res1 <- simulx(model     = 'model/hierarchical1a.txt', 
               parameter = p, 
               output    = list(V, f, y),
               settings  = list(seed = 12345))

print(res1$parameter)

print(ggplotmlx() + 
        geom_line(data=res1$f, aes(x=time, y=f), size=1) + 
        geom_point(data=res1$y, aes(x=time, y=y), colour='red', size=2))

#---------------------------------------
g <- list( size  = 5,
           level = 'individual')

res2 <- simulx(model     = 'model/hierarchical1a.txt', 
               parameter = p, 
               output    = list(V, f, y),
               group     = g,
               settings  = list(seed = 12345))

print(res2$parameter)

print(ggplotmlx() + 
        geom_line(data=res2$f, aes(x=time, y=f, colour=id),size=1) +  
        geom_point(data=res2$y, aes(x=time, y=y, colour=id), size=2))


#---------------------------------------
g <- list( size  = 5,
           level = 'longitudinal')

res3 <- simulx(model     = 'model/hierarchical1a.txt', 
               parameter = p, 
               output    = list(V, f, y),
               group     = g,
               settings  = list(seed = 12345))

print(res3$parameter)

print(ggplotmlx() + 
        geom_line(data=res3$f, aes(x=time, y=f), colour="grey", size=1) +  
        geom_point(data=res3$y, aes(x=time, y=y, colour=id), size=2))

#---------------------------------------
g <- list( size  = c(2,3),
           level = c('individual','longitudinal'))

res4 <- simulx(model     = 'model/hierarchical1a.txt', 
               parameter = p, 
               output    = list(V, f, y),
               group     = g,
               settings  = list(seed = 12345))

print(res4$parameter)

print(ggplotmlx() + 
        geom_line(data=res4$f, aes(x=time, y=f, colour=id), size=1) +  
        geom_point(data=res4$y, aes(x=time, y=y, colour=id), size=2))

#-------------------------------------

p <- c(V_pop=10, omega_V=0.3, beta=1, w=75, w_pop=70, k=0.1, a=0.5)
f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(1, 30, by=3))
V <- list(name='V')

g <- list( size  = 5,
           level = 'individual')

res5 <- simulx(model     = 'model/hierarchical1b.txt', 
               parameter = p, 
               output    = list(V, f, y),
               group     = g,
               settings  = list(seed = 12345))

print(res5$parameter)

print(ggplotmlx() + 
        geom_line(data=res5$f, aes(x=time, y=f, colour=id), size=1) + 
        geom_point(data=res5$y, aes(x=time, y=y, colour=id), size=2))
