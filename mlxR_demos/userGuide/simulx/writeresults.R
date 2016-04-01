# library(mlxR)

pkpd.model <- inlineModel("
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
adm  <- list(amount=100, time=seq(0,50,by=12))
y1 <- list(name=c('y1'), time=seq(5,to=50,by=5))
y2 <- list(name='y2', time=seq(2,to=50,by=6))

res1 <- simulx(model     = pkpd.model,
              treatment = adm,
              parameter = p,
              group     = list(size=5, level="individual"),
              output    = list(y1,y2),
              result.file   ="res1a.csv",
              settings      = list(seed = 32323))
names(res1)
print(head(read.table("res1a.csv", header=T, sep=",")))

res1 <- simulx(model     = pkpd.model,
              treatment = adm,
              parameter = p,
              group     = list(size=5, level="individual"),
              output    = list(y1,y2),
              result.file   ="res1a.txt",
              settings      = list(seed = 32323, sep="\t", digits=1))
print(head(read.table("res1a.txt", header=T, sep="\t")))

res1 <- simulx(model     = pkpd.model,
              treatment = adm,
              parameter = p,
              group     = list(size=5, level="individual"),
              output    = list(y1,y2),
              result.folder ="res1a",
              settings      = list(seed = 32323))
print(list.files(path="res1a"))

res1 <- simulx(model     = pkpd.model,
              treatment = adm,
              parameter = p,
              group     = list(size=5, level="individual"),
              output    = list(y1,y2),
              result.file   ="res1b.csv",
              result.folder ="res1b",
              settings      = list(seed = 32323))
print(head(read.table("res1b.csv", header=T, sep=",")))
print(list.files(path="res1b"))

res1 <- simulx(model     = pkpd.model,
               treatment = adm,
               parameter = p,
               group     = list(size=5, level="individual"),
               output    = list(y1,y2, c("V" ,"Cl", "EC50")),
               result.file   ="res1c.csv",
               settings      = list(seed = 32323))
print(head(read.table("res1c.csv", header=T, sep=",")))

#--------------------------------------------
res2 <- simulx(model     = pkpd.model,
              treatment = adm,
              parameter = p,
              group     = list(size=5, level="individual"),
              output    = list(y1,y2),
              settings  = list(seed = 32323))

writeDatamlx(res2, result.file = "res2.csv")
print(head(read.table("res2.csv", header=T, sep=",")))
writeDatamlx(res2, result.folder = "res2")
print(list.files(path="res2"))

