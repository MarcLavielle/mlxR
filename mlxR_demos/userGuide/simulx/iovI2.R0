
adm <- list(amount=100, time=seq(0,132,by=12))
occ <- list(time=seq(0,96,by=48),name='occ')

p <- c(V_pop=10, o_V0=0.1, o_V1=0.3, Cl_pop=1, o_Cl0=0.2)

f  <- list(name='C', time=seq(0, 150, by=1))
pk <- list(name=c("V","Cl"))

res1 <- simulx( model       = "model/iovI2.txt",
                treatment   = adm,
                varlevel    = occ,
                parameter   = p,
                output      = list(f,pk),
                settings    = list(seed = 123456))

print(res1$varlevel)
print(res1$parameter)
print(ggplotmlx(data=res1$C) + geom_line(aes(x=time, y=C)))

#-----------------------------------------------
res2 <- simulx( model       = "model/iovI2.txt",
                treatment   = adm,
                varlevel    = occ,
                parameter   = p,
                output      = list(f,pk),
                group       = list(size=2, level='individual'),
                settings    = list(seed = 123456))

print(res2$parameter)
print(ggplotmlx(data=res2$C) + geom_line(aes(x=time, y=C, colour=id)))


#-----------------------------------------------
occ.g1  <- list(time=seq(0,96,by=48), name='occ')
occ.g2  <- list(time=seq(0,108,by=36),name='occ')

g1 <- list(varlevel = occ.g1, level='individual')
g2 <- list(varlevel = occ.g2, level='individual')

res3 <- simulx( model       = "model/iovI2.txt",
                treatment   = adm,
                parameter   = p,
                output      = list(f,pk),
                group       = list(g1, g2),
                settings    = list(seed = 123123))

print(res3$parameter)
print(ggplotmlx(data=res3$C) + geom_line(aes(x=time, y=C, colour=id))+ 
        facet_grid(. ~ id))


#-----------------------------------------------
occ = inlineDataFrame("
  id time occ
   1    0   1
   1   48   2
   1   96   3
   2    0   1
   2   36   2
   2   72   3
   2  108   4
")

res4 <- simulx( model       = "model/iovI2.txt",
                treatment   = adm,
                varlevel    = occ,
                parameter   = p,
                output      = list(f,pk),
                settings    = list(seed = 123123))

print(res4$parameter)
print(ggplotmlx(data=res4$C) + geom_line(aes(x=time, y=C, colour=id))+ 
        facet_grid(. ~ id))
