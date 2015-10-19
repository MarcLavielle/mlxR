
myModel = inlineModel("
[LONGITUDINAL]
input = {Mtt, Ktr, ka, V, Vm, Km, k12, k21}
EQUATION:
C = pkmodel(Mtt, Ktr, ka, V, Vm, Km, k12, k21)
[INDIVIDUAL]
input = {V_pop, omega_V}
DEFINITION:
V = {distribution=lognormal, prediction=V_pop, sd=omega_V}
")


adm <- list(time=seq(0, 200, by=12), amount=100)
p   <- c(V_pop=10, omega_V=0.2, Mtt=2, Ktr=0.5, ka=1, Vm=10, Km=1, k12=0.5, k21=0.3)
C   <- list(name='C',time=seq(100, 200, by=10))
g   <- list(size = 100, level='individual')

res <- simulx(model     = myModel,
              parameter = p,
              output    = C,
              treatment = adm,
              group     = g)

#---------------------------------------------
M=100

ptm <- proc.time()
for(i in seq(1,M)){
  res <- simulx(model     = myModel,
                parameter = p,
                output    = C,
                treatment = adm,
                group     = g)
}
print(proc.time() - ptm)

#---------------------------------------------
ptm <- proc.time()
for(i in seq(1,M)){
  res <- simulx(model     = myModel,
                parameter = p,
                output    = C,
                treatment = adm,
                group     = g,
                settings  = list(load.design=FALSE))
}
print(proc.time() - ptm)

#---------------------------------------------
ptm <- proc.time()
for(i in seq(1,M)){
  res <- simulx(model     = myModel,
                parameter = p,
                output    = C,
                treatment = adm,
                group     = g,
                settings  = list(load.design=TRUE))
}
print(proc.time() - ptm)

#---------------------------------------------
dataIn <- simulx(model     = myModel,
                 parameter = p,
                 output    = C,
                 treatment = adm,
                 group     = g,
                 settings  = list(data.in=TRUE))

ptm <- proc.time()
for(i in seq(1,M)){
  dd <- simulx(data=dataIn, settings=list(load.design=FALSE))
}
print(proc.time() - ptm)

#---------------------------------------------
ptm <- proc.time()
for(i in seq(1,M)){
  dd=simulx(data=dataIn, settings=list(load.design=TRUE))  
}
print(proc.time() - ptm)
