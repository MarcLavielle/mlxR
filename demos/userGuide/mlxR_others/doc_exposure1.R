# library(mlxR)

#-----------------------------
myModel <- inlineModel("
[LONGITUDINAL]
input = {ka, V, Cl}
EQUATION:
Cc = pkmodel(ka, V, Cl)
")

#-----------------------------
p <- c(ka=0.5, V=10, Cl=1)
adm1 <- list(time=0, amount=100)

out1a <- list(name="Cc", time=seq(0, 24, by=0.2))
res1a <- exposure(model=myModel, parameter=p, output=out1a, treatment=adm1)
print(res1a$Cc)

out1b <- list(name="Cc", time=c(seq(0, 12, by=0.2), seq(12.4, 24, by=0.4)) )
res1b <- exposure(model=myModel, parameter=p, output=out1b, treatment=adm1)
print(res1b$Cc)

#-----------------------------
adm2 <- list(time=seq(0,160,by=8), amount=100)
out2 <- list(name="Cc", time=seq(160, 168, by=0.1))

res2 <- exposure(model=myModel, parameter=p, output=out2, treatment=adm2)

print(res2$Cc)
ggplotmlx(data=res2$output$Cc) + geom_line(aes(x=time,y=Cc))

#-----------------------------
adm3 <- list(tfd=0, ii=8, amount=100)
out3a <- list(name="Cc", time='steady.state')

res3a <- exposure(model=myModel, parameter=p, output=out3a, treatment=adm3)

print(res3a$Cc)
ggplotmlx(data=res3a$output$Cc) + geom_line(aes(x=time,y=Cc))

#-----------------------------
out3b <- list(name="Cc", time='steady.state', tol=0.001)
res3b <- exposure(model=myModel, parameter=p, output=out3b, treatment=adm3)
print(res3b$Cc)

#-----------------------------
p3 <- inlineDataFrame("
                      id   ka   V   Cl
                      1  0.5  10    1
                      2  0.5   8  0.8
                      ")
res3c <- exposure(model=myModel, parameter=p3, output=out3b, treatment=adm3)
print(res3c$Cc)
ggplotmlx(data=res3c$output$Cc) + geom_line(aes(x=time,y=Cc, color=id))

#-----------------------------
g1 <- list(treatment=list(ii=8, amount=100))
g2 <- list(treatment=list(ii=4, amount=50))
out4 <- list(name="Cc", time='steady.state', tol=0.001)

res4 <- exposure(model=myModel, parameter=p, output=out4, group=list(g1,g2))
print(res4$Cc)
ggplotmlx(data=res4$output$Cc) + geom_line(aes(x=time,y=Cc, color=group))

#-----------------------------
# g1 <- list(treatment=list(time=seq(0,160,by=8), amount=100))
# g2 <- list(treatment=list(time=seq(0,164,by=4), amount=50))
# out4 <- list(name="Cc", time=seq(160, 168, by=0.1))
# 
# res4 <- exposure(model=myModel, parameter=p, output=out4, group=list(g1,g2))
# print(res4$Cc)
# ggplotmlx(data=res4$output$Cc) + geom_line(aes(x=time,y=Cc, color=id))
