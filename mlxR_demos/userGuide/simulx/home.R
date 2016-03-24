f <- list(name='f', time=seq(0, 30, by=0.1))
y <- list(name='y', time=seq(0, 30, by=2))

res <- simulx(model     = 'model/home.txt', 
              parameter = c(A=100, k_pop=4, omega=0.3, c=10, a=2), 
              output    = list(f,y),
              group     = list(size=4, level='individual'),
              settings  = list(seed=12321))

plot(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour=id)) +
       geom_point(data=res$y, aes(x=time, y=y, colour=id)))
     