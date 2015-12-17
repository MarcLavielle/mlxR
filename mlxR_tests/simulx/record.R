
myModel = inlineModel("
[LONGITUDINAL]
input = {V, k, a}
EQUATION:
C = pkmodel(V,k)
DEFINITION:
y = {distribution=normal, prediction=C, sd=a}

[INDIVIDUAL]
input = {V_pop, omega_V, w}
EQUATION:
Vpred=V_pop*(w/70)
DEFINITION:
V = {distribution=lognormal, prediction=Vpred, sd=omega_V}
")

adm1 <- list(time=seq(0,to=60,by= 6), amount= 50, rate=25)
adm2 <- list(time=seq(0,to=60,by=12), amount=100, rate=50)
y <- list(name='y', time=seq(0, 72, by=6))
p <- c(V_pop=10, omega_V=0.3, w=75, k=0.2, a=0.5)
g1 <- list(treatment=adm1,size=3,level='individual')
g2 <- list(treatment=adm2,size=2,level='individual')

filename <- "data/record_data.txt"

res <- simulx(model     = myModel, 
              parameter = p, 
              output    = y, 
              group     = list(g1,g2), 
              settings  = list(record.file=filename))
              
file.show(filename)
#writeLines(readLines(filename))
#cat(readChar(filename, 1e5))
