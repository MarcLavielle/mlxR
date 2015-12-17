
#-------------------------------------
tr <- list(amount = c(-2, 0.5), 
           time   = c(30, 50))

out  <- list(name = 'h', 
             time = seq(0, 70, by=0.1))

p    <- c( r = 0.1 ) 

res <- simulx(model='model/source1a.txt', 
              parameter=p, 
              output=out, 
              treatment=tr)

print(ggplotmlx(data=res$h) + geom_line(aes(x=time,y=h), colour="magenta" ,size=1))

#-------------------------------------
tr <- list(amount = c(-2, 0.5), 
           rate   = 0.5,
           time   = c(30, 50))

res <- simulx(model='model/source1a.txt', 
              parameter=p, 
              output=out, 
              treatment=tr)

print(ggplotmlx(data=res$h) + geom_line(aes(x=time,y=h), colour="magenta" ,size=1))

#-------------------------------------
ton  <- list(amount = 1, 
             rate   = 1, 
             time   = c(5, 25))

toff <- list(amount = -1, 
             rate   = 0.25, 
             time   = c(10, 40))

res <- simulx(model='model/source1a.txt', 
              parameter=p, 
              output=out, 
              treatment=list(ton, toff))

print(ggplotmlx(data=res$h) + geom_line(aes(x=time,y=h), colour="magenta", size=1))

#-------------------------------------
tr  <- list(amount = c(1,-1,1,-1), 
            rate   = c(1,0.25,1,0.25), 
            time   = c(5, 10, 25, 40))

res <- simulx(model='model/source1a.txt', 
              parameter=p, 
              output=out, 
              treatment=tr)

print(ggplotmlx(data=res$h) + geom_line(aes(x=time,y=h), colour="magenta", size=1))

#-------------------------------------
p    <- c( r=0.1, tau=5, F=2 ) 

res <- simulx(model='model/source2.txt', 
              parameter=p, 
              output=out, 
              treatment=list(ton, toff))

print(ggplotmlx(data=res$h) + geom_line(aes(x=time,y=h), colour="magenta", size=1))

#-------------------------------------
tr <- list(amount = c(-2, 0.5), 
           time   = c(30, 50),
           target = 'h')

res <- simulx(model='model/source1b.txt', 
              parameter=p, 
              output=out, 
              treatment=tr)

print(ggplotmlx(data=res$h) + geom_line(aes(x=time,y=h), colour="magenta" ,size=1))

