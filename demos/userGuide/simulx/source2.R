
#-------------------------------------
s1 <- list(type=1, time=c(10, 30, 40), amount=c(1,-1,1), rate=0.5)
s2 <- list(type=2, time=c( 5, 25, 35), amount=c(1,-0.5,1.5))

fg <- list(name=c('f','g'), time=seq(0, 50, by=0.1))

res <- simulx( model     = "model/source3a.txt",
               parameter = c(k1=0.2, k2=0.1, f0=0, g0=0),
               treatment = list(s1, s2),
               output    = fg)

print(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour="blue")) + 
        geom_line(data=res$g, aes(x=time, y=g, colour="red")) +
        scale_colour_manual(name="",values=c('blue'='blue','red'='red'),labels=c('f','g')) + 
        theme(legend.position=c(0.9, 0.1)))


#-------------------------------------
res <- simulx( model     = "model/source4.txt",
               parameter = c(k1=0.2, k2=0.1, f0=0, g0=0, tau=2),
               treatment = list(s1, s2),
               output    = fg)

print(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour="blue")) + 
        geom_line(data=res$g, aes(x=time, y=g, colour="red")) +
        scale_colour_manual(name="",values=c('blue'='blue','red'='red'),labels=c('f','g')) + 
        theme(legend.position=c(0.9, 0.1)))

#-------------------------------------
s1 <- list(target="f", time=c(10, 30, 40), amount=c(1,-1,1), rate=0.5)
s2 <- list(target="g", time=c( 5, 25, 35), amount=c(1,-0.5,1.5))

res <- simulx( model     = "model/source3b.txt",
               parameter = c(k1=0.2, k2=0.1, f0=0, g0=0),
               treatment = list(s1, s2),
               output    = fg)

print(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour="blue")) + 
        geom_line(data=res$g, aes(x=time, y=g, colour="red")) +
        scale_colour_manual(name="",values=c('blue'='blue','red'='red'),labels=c('f','g')) + 
        theme(legend.position=c(0.9, 0.1)))
