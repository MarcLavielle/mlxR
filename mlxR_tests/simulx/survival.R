
p <- c(beta = 2.5, lambda=50)
h <- list(name='h', time=seq(0, 60, by=1))
e <- list(name='e', time=0)

res <- simulx(model     = 'model/survival1.txt', 
              settings  = list(seed=123),
              parameter = p, 
              output    = list(h, e))

print(res$e)

hazard  <- res$h
plot(x=hazard$time, y=hazard$h, type='l',
     xlab="time", ylab="hazard")

#-------------------------------------
n.rep <- 100
res <- simulx(model     = 'model/survival1.txt', 
              settings  = list(seed=123),
              parameter = p, 
              output    = list(h, e), 
              group     = list(size = n.rep, level='longitudinal'))

print(res$e[1:10,])

pl1  <- kmplotmlx(res$e, level=0.9)
print(pl1)

#---------------------------------------------
res <- simulx(model     = 'model/survival2.txt', 
              settings  = list(seed=123),
              parameter = p, 
              output    = list(e)) 

print(res$e)

res <- simulx(model     = 'model/survival2.txt', 
              settings  = list(seed=123),
              parameter = p, 
              output    = list(e), 
              group     = list(size=n.rep, level='longitudinal'))

pl2  <- kmplotmlx(res$e, level=0.9)
print(pl2)


