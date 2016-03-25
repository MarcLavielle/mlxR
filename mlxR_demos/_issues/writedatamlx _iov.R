

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
head(read.table("res3.csv", header=T, sep=","))

