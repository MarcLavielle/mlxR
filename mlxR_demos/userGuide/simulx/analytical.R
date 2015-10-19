setwd(dirname(parent.frame(2)$ofile))
library(reshape)
# library(mlxR)
#-----------------------------------------

p <- c( ka = 0.5, V  = 10,  k  = 0.2)

f <- list(name = 'f1', 
          time = seq(0, 25, by=0.1))


res <- simulx(model     = 'model/analytical.txt', 
              parameter = p, 
              output    = f)

plot(ggplotmlx(aes(x=time, y=f1), data=res$f1) + geom_line()) 

#-----------------------------------------

f <- list(name = c('f1','f2'), 
          time = seq(0, 25, by=0.1))

res <- simulx(model     = 'model/analytical.txt', 
              parameter = p, 
              output    = f)

plot(ggplotmlx(aes(x=time, y=f1), data=res$f1) + geom_line() +
       geom_line(aes(x=time, y=f2), data=res$f2, color="red") +
       ylab('concentration'))

#-----------------------------------------

r <- merge(res$f1,res$f2)
r <- melt(r ,  id = 'time', variable_name = 'f')
print(ggplotmlx(r, aes(time,value)) + geom_line(aes(colour = f),size=1) +
        ylab('concentration') + guides(colour=guide_legend(title=NULL)) +
        theme(legend.position=c(.9, .8)))

#-----------------------------------------

f1 <- list(name = 'f1', time = seq(0, 15, by=0.1))
f2 <- list(name = 'f2', time = seq(10, 25, by=1))

res <- simulx(model     = 'model/analytical.txt', 
              parameter = p, 
              output    = list(f1,f2))

plot(ggplotmlx(aes(x=time, y=f1), data=res$f1) + geom_line() +
       geom_line(aes(x=time, y=f2), data=res$f2, color="red")  +
       ylab('concentration'))

#-----------------------------------------

r <- merge(res$f1,res$f2,all=TRUE)
r <- melt(r ,  id = 'time', variable_name = 'f')
r=r[(!is.na(r$value)),]
print(ggplotmlx(r, aes(time,value)) + geom_line(aes(colour = f),size=1) +
        ylab('concentration') + guides(colour=guide_legend(title=NULL)) +
        theme(legend.position=c(.9, .8)))


