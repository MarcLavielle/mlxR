
vl = list(time=c(0,10,20,40),name='occ')
p <- c(a_pop=0, oa0=0.05, oa1=0.2)
f <- list(name='f', time=seq(0, 60, by=0.5))
a <- list(name="a")

res1 <- simulx( model      = "model/iovI1.txt",
               varlevel    = vl,
               parameter   = p,
               output      = list(f,a),
               settings    = list(seed=12345))

print(res1$parameter)

print(ggplotmlx(data=res1$f) + geom_line(aes(x=time, y=f)))

#------------------------------------------------

res2 <- simulx( model       = "model/iovI1.txt",
                varlevel    = vl,
                parameter   = p,
                output      = list(f,a),
                group       = list(size=5, level='individual'),
                settings    = list(seed=12345))

print(ggplotmlx(data=res2$f) + geom_line(aes(x=time, y=f, colour=id)))

