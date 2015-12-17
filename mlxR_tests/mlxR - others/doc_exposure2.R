# # library("mlxR")
library(gridExtra)

#-----------------------------
myModel <- inlineModel("
[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(type=1, target=Ad)
depot(type=2, target=Ac)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac =  ka*Ad - (Cl/V)*Ac
Cc = Ac/V
")

#-----------------------------
p <- c(ka=0.5, V=10, Cl=1)

adm1 <- list(time=seq(0,200,by=8), amount=100, type=1)
adm2 <- list(time=seq(3,200,by=12), amount=50, type=2, tinf=1)
adm <- list(adm1, adm2)
out <- list(name="Cc", time=seq(0,200,by=0.5))
res <- simulx(model=myModel, parameter=p, output=out, treatment=adm)
print(ggplotmlx(data=res$Cc) + geom_line(aes(x=time,y=Cc)))

adm1 <- list(tfd=0, ii=8, amount=100, type=1)
adm2 <- list(tfd=3, ii=12, amount=50, type=2, tinf=1)
adm <- list(adm1, adm2)
out <- list(name="Cc", time='steady.state', ntp=200, tol=0.01)

res <- exposure(model=myModel, parameter=p, output=out, treatment=adm)

print(res$Cc)
print(ggplotmlx(data=res$output$Cc) + geom_line(aes(x=time,y=Cc)))