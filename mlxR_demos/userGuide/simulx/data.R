
#-------------------------------------

p = read.table("data/data1.txt",header=TRUE)

p <- inlineDataFrame("
 id     V      k
  1    12   0.15
  2     9   0.25
  3     8   0.15
  4    11   0.20 
")

adm <- list(time=0, amount=100)

C <- list(name="C", time=seq(0, 10, by=1))

res1 <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = adm)

print(ggplotmlx(data=res1$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

res1b <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = adm,
               group = list(size=7))

print(ggplotmlx(data=res1b$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

p <- inlineDataFrame("
 id     V      
  1    12   
  2     9   
  3     8   
  4    11   
")
res1c <- simulx(model     = "model/groupI.txt", 
               parameter = list(p,c(k=0.2)), 
               output    = C, 
               treatment = adm)

print(ggplotmlx(data=res1c$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))


#-------------------------------------
V <- inlineDataFrame("
 id     V
  1    12
  2     9
  3     8
  4    11 
")
k <- c(k=0.2)
p <- list(V, k)


res2 <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = adm)

print(ggplotmlx(data=res2$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))


res2b <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = adm,
               group = list(size=7))


print(ggplotmlx(data=res2b$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))

#-------------------------------------
adm <- inlineDataFrame("
id  time  amount  rate 
 1      1    100   Inf 
 1     12     50   10  
 1     24    100   10 
 2      6     75   15 
 2     18    100   Inf 
 2     24     75   15 
")

p   <- c(V=10, k=0.15)
C  <- list(name="C", time=seq(0, 50, by=0.1))


res3 <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = adm,
               settings = list(load.design=TRUE))

print(ggplotmlx(data=res3$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))


res3b <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = adm,
               group = list(size=5))

print(ggplotmlx(data=res3b$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))


res3c <- simulx(model     = "model/groupI.txt", 
               parameter = p, 
               output    = C, 
               treatment = list(adm, list(time=c(20,40),amount=50)),
               settings = list(load.design=TRUE))
print(ggplotmlx(data=res3c$C, aes(x=time, y=C, colour=id)) + geom_line(size=1))


#-------------------------------------

p   <- c(V=10, k=0.15, a=0.2)
adm <- list(time=0, amount=100)

design.y  <- inlineDataFrame("
  id  time
   1     3  
   1     6  
   1     9  
   1    12  
   2     2  
   2    10  
   2    18 
")
y  <- list(name="y",  time=design.y)



res4 <- simulx(model     = "model/groupII1.txt", 
               parameter = p, 
               output    = y, 
               treatment = adm)

print(ggplotmlx(data=res4$y, aes(x=time,y=y,color=id))+geom_line(size=1) + geom_point())



res4b <- simulx(model     = "model/groupII1.txt", 
               parameter = p, 
               output    = y, 
               treatment = adm,
               group = list(size=5))

print(ggplotmlx(data=res4b$y, aes(x=time,y=y,color=id))+geom_line(size=1) + geom_point())


#-------------------------------------
adm <- inlineDataFrame("
 id  time amount rate
  1     1    100    0
  1    12     50   10 
  1    24    100   10
  2     6     75   15
  2    18    100    0
  2    24     75   15
")

p.pop <- c(V_pop=10, omega_V=0.3, a=0.2)

p.indiv <- inlineDataFrame("
id   w     k  
 1  75   0.5   
 2  60   0.4   
")


design.y  <- inlineDataFrame("
  id  time
   1     3  
   1    15  
   1    27  
   1    30  
   2     2  
   2    18  
   2    24 
")
out.y <- list(name="y",  time=design.y)
out.C <- list(name="C", time=seq(0, 50, by=1))


res5 <- simulx(model     = "model/groupII2.txt", 
               parameter = list(p.pop,p.indiv), 
               output    = list(out.C, out.y), 
               treatment = adm)
                            
print(ggplotmlx(data=res5$C, aes(x=time, y=C, colour=id)) + geom_line(size=1) +
         geom_point(data=res5$y, aes(x=time, y=y, colour=id)))



res5b <- simulx(model     = "model/groupII2.txt", 
               parameter = list(p.pop,p.indiv), 
               output    = list(out.C, out.y), 
               treatment = adm,
               group = list(size=5))

print(ggplotmlx() + geom_line(data=res5b$C, aes(x=time,y=C,colour=id),size=1) +
        geom_point(data=res5b$y, aes(x=time, y=y, colour=id)))


res5c <- simulx(model     = "model/groupII2.txt", 
                parameter = list(p.pop,p.indiv), 
                output    = list(out.C, out.y), 
                treatment = list(adm, list(time=c(20,40),amount=50)),
                group = list(size=5))

print(ggplotmlx() + geom_line(data=res5c$C, aes(x=time,y=C,colour=id),size=1) +
        geom_point(data=res5b$y, aes(x=time, y=y, colour=id)))

