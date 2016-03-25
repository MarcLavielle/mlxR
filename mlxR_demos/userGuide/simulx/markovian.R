library(mlxR)
library(reshape2)

#-------------------------------------

seed <- 12345
p    <- c(p12=0.2, p21=0.15)
y    <- list(name='y', time=seq(1, 200))
res1  <- simulx(model     = 'model/markovianA.txt', 
                parameter = p, 
                output    = y,
                settings  = list(seed=seed))

print(ggplot(data=res1$y) + geom_point(aes(x=time, y=y),size=1))


#-------------------------------------
p    <- c(a=1,b=-0.02,c=0.7,d=-0.01)
f    <- list(name=c('p12','p21'), time=seq(1, 200))
res2 <- simulx(model     = 'model/markovianB.txt', 
               parameter = p, 
               output    = list(y,f),
               settings  = list(seed=seed))

print(ggplot(res2$y) + geom_point(aes(x=time, y=y),size=1))

r <- melt(merge(res2$p12,res2$p21) ,  id = 'time', variable_name = 'f')
print(ggplot(r, aes(time,value)) + geom_line(aes(colour = f),size=1) +
        ylab('transition probabilities') + guides(colour=guide_legend(title=NULL)) +
        theme(legend.position=c(.9, .8)))

#-------------------------------------
q    <- c(q01=0.8, q10=1)
y    <- list(name='y', time=sort(runif(50,min=0,max=200)))
res3  <- simulx(model     = 'model/markovianC.txt', 
                parameter = q, 
                output    = y,
                settings  = list(seed=12345))

print(ggplot(data=res3$y) + geom_point(aes(x=time, y=y)))