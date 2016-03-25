
adm  <- list(amount=100, time=seq(0,132,by=12))
occ1 <- list(time=seq(0,96,by=48), name='occ1')
occ2 <- list(time=seq(0,120,by=12),name='occ2')

p <- c(V_pop=10, o_V0=0.1,  o_V1=0.3, o_V2=0.1, 
       Cl_pop=1, o_Cl0=0.2, o_Cl1=0.1)

f <- list(name='C', time=seq(0, 150, by=1))
pk <- list(name=c("V","Cl"))

res1 <- simulx( model       = "model/iovII.txt",
                treatment   = adm,
                varlevel    = list(occ1, occ2),
                parameter   = p,
                output      = list(f,pk),
                settings    = list(seed = 12345))

print(res1$parameter)
print(ggplotmlx(data=res1$C) + geom_line(aes(x=time, y=C)))

#-----------------------------------------------
res2 <- simulx( model       = "model/iovII.txt",
                treatment   = adm,
                varlevel    = list(occ1, occ2),
                parameter   = p,
                output      = list(f,pk),
                group       = list(size=2, level='individual'),
                settings    = list(seed = 12345))

print(res2$parameter)
print(ggplotmlx(data=res2$C) + geom_line(aes(x=time, y=C, colour=id)))


#-----------------------------------------------
occ1.g1 <- list(time=seq(0, 96,by=48), name='occ1')
occ1.g2 <- list(time=seq(0,108,by=36), name='occ1')
occ2    <- list(time=seq(0,120,by=12), name='occ2')

g1 <- list(varlevel = list(occ1.g1,occ2), level='individual', size=3)
g2 <- list(varlevel = list(occ1.g2,occ2), level='individual', size=2)

res3 <- simulx( model       = "model/iovII.txt",
                treatment   = adm,
                parameter   = p,
                output      = list(f,pk),
                group       = list(g1, g2),
                settings    = list(seed = 32323))

print(res3$parameter)
print(ggplotmlx(data=res3$C) + geom_line(aes(x=time, y=C, colour=id))+ 
        facet_grid(. ~ id))


#-----------------------------------------------
vl = inlineDataFrame("
 id time occ1 occ2
  1    0    1    1
  1   12    1    2
  1   24    1    3
  1   36    1    4
  1   48    2    5
  1   60    2    6
  1   72    2    7
  1   84    2    8
  1   96    3    9
  1  108    3   10
  1  120    3   11
  2    0    1    1
  2   12    1    2
  2   24    1    3
  2   36    2    4
  2   48    2    5
  2   60    2    6
  2   72    3    7
  2   84    3    8
  2   96    3    9
  2  108    4   10
  2  120    4   11
")

res4 <- simulx( model       = "model/iovII.txt",
                treatment   = adm,
                varlevel    = vl,
                parameter   = p,
                output      = list(f,pk),
                settings    = list(seed = 32323))

print(res4$parameter)
print(ggplotmlx(data=res4$C) + geom_line(aes(x=time, y=C, colour=id))+ 
        facet_grid(. ~ id))
