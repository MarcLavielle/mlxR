
model1 <- inlineModel("
[LONGITUDINAL]
input = {V, Cl, a1}

EQUATION:
Cc = pkmodel(V, Cl)

DEFINITION:
y1 ={distribution=lognormal, prediction=Cc, sd=a1}
")

adm  <- list(amount=100, time=seq(0,50,by=12))
p <- c(V=10, Cl=1, a1=0.1)
y1 <- list(name=c('y1'), time=seq(5,to=50,by=5))

res1a <- simulx( model     = model1,
                 treatment = adm,
                 parameter = p,
                 output    = y1,
                 settings  = list(seed = 32323))

writeDatamlx(res1a,result.file   = "res1a.csv")
head(read.table("res1a.csv", header=T, sep=","))

writeDatamlx(res1a, result.file  = "res1a.txt", sep="\t")

writeDatamlx(res1a,result.folder = "res1a")
list.files(path="res1a")

writeDatamlx(res1a,result.file   = "res1a.csv",result.folder="res1a")

res1b <- simulx( model         = model1,
                 treatment     = adm,
                 parameter     = p,
                 output        = y1,
                 result.file   ="res1b.csv",
                 result.folder ="res1b",
                 settings      = list(seed = 32323))


res1b <- simulx( model         = model1,
                 treatment     = adm,
                 parameter     = p,
                 output        = y1,
                 result.file   ="res1b.csv",
                 result.folder ="res1b",
                 settings      = list(seed = 32323))

head(read.table("res1b.csv", header=T, sep=","))
list.files(path="res1b")


#---------------------------------------------------

model2 <- inlineModel("
[LONGITUDINAL]
input = {V, Cl, EC50, a1, a2}

EQUATION:
Cc = pkmodel(V, Cl)
E  = 100*Cc/(Cc+EC50)

DEFINITION:
y1 ={distribution=lognormal, prediction=Cc, sd=a1}
y2 ={distribution=normal,    prediction=E,  sd=a2}

[INDIVIDUAL]
input={V_pop,o_V,Cl_pop,o_Cl,EC50_pop,o_EC50}

DEFINITION:
V   ={distribution=lognormal, prediction=V_pop,   sd=o_V}
Cl  ={distribution=lognormal, prediction=Cl_pop,  sd=o_Cl}
EC50={distribution=lognormal, prediction=EC50_pop,sd=o_EC50}
")

p <- c(V_pop=10, o_V=0.1, Cl_pop=1, o_Cl=0.2, EC50_pop=3, o_EC50=0.2, a1=0.1, a2=1)
y2 <- list(name='y2', time=seq(2,to=50,by=6))

res2a <- simulx( model     = model2,
                 treatment = adm,
                 parameter = p,
                 group     = list(size=5, level="individual"),
                 output    = list(y1,y2))

writeDatamlx(res2a, result.file   = "res2a.csv")
head(read.table("res2a.csv", header=T, sep=","))
writeDatamlx(res2a, result.folder = "res2a")


g1 = list(size=5, level="individual", treatment=list(amount=100, time=seq(0,50,by=12)))
g2 = list(size=3, level="individual", treatment=list(amount=50,  time=seq(0,50,by=12)))
res2b <- simulx( model     = model2,
                 parameter = p,
                 group     = list(g1,g2),
                 nrep      = 10,
                 output    = list(y1,y2))

writeDatamlx(res2b, result.file   = "res2b.csv")
head(read.table("res2b.csv", header=T, sep=","))

#-------------------------------------

model3 <- inlineModel("
[LONGITUDINAL]
input = {V, Cl, EC50, a1, a2}

EQUATION:
Cc = pkmodel(V, Cl)
E  = 100*Cc/(Cc+EC50)

DEFINITION:
y1 ={distribution=lognormal, prediction=Cc, sd=a1}
y2 ={distribution=normal,    prediction=E,  sd=a2}

[INDIVIDUAL]
input={V_pop,o_V,Cl_pop,o_Cl,o1_Cl,EC50_pop,o_EC50}

DEFINITION:
V   ={distribution=lognormal, prediction=V_pop, sd=o_V}
Cl  ={distribution=lognormal, prediction=Cl_pop, varlevel={id,id*occ}, sd={o_Cl, o1_Cl}}
EC50={distribution=lognormal, prediction=EC50_pop,sd=o_EC50}
")

p <- c(V_pop=10, o_V=0.1, Cl_pop=1, o_Cl=0.2, o1_Cl=0.1, 
       EC50_pop=3, o_EC50=0.2, a1=0.1, a2=1)

pk <- list(name=c("V","Cl","EC50"))

occ <- list(time=c(0, 15, 30), name='occ')

res3 <- simulx( model       = model3,
                treatment   = adm,
                parameter   = p,
                output      = list(pk, y1, y2),
                group       = list(size=5, level="individual"),
                varlevel    = occ)

writeDatamlx(res3,result.file="res3.csv")

