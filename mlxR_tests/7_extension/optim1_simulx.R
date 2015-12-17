
myModel = inlineModel("
[LONGITUDINAL]
input = {V, k}
EQUATION:
Cc = pkmodel(V,k)
")

adm <- list(time=seq(0, 200, by=12), 
            amount=100)

p <- list(name=c('V', 'k'), 
          value=c(10, 0.2))

Cc <- list(name='Cc',time=seq(100, 200, by=10))

g <- list(size = 100, level='longitudinal')


#---------------------------------------------
M=20

res <- simulx(model=myModel,parameter=p,output=Cc,treatment=adm,group=g)
ptm <- proc.time()
for(i in seq(1,M)){
  res <- simulx(model=myModel,parameter=p,output=Cc,treatment=adm,group=g)
}
print(proc.time() - ptm)



s <- list(data.in=TRUE, load.design=TRUE)
dataIn <- simulx(model=myModel,parameter=p,output=Cc,treatment=adm,group=g,settings=s)
ptm <- proc.time()
for(i in seq(1,M)){
  dd=simulx(data=dataIn)  
}
print(proc.time() - ptm)

ptm <- proc.time()
for(i in seq(1,M)){
  dd=simulx(data=dataIn, settings=list(load.design=FALSE))  
}
print(proc.time() - ptm)

ptm <- proc.time()
for(i in seq(1,M)){
  dd <- simulx(data=dataIn, settings=list(load.design=TRUE))
}
print(proc.time() - ptm)

