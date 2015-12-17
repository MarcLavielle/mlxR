myModel <- inlineModel("
[LONGITUDINAL]
input = {F, Tlag, ka, V, k}

PK:
depot(type=1, target=Ad, p=F, Tlag)
depot(type=2, target=Ac)

EQUATION:
  ddt_Ad = -ka*Ad
ddt_Ac =  ka*Ad - k*Ac
Cc = Ac/V
")
#-------------------------------------

adm1 <- list(time=c(6, 36), amount=40, type=1)
adm2 <- list(time=c(12,42), amount=c(20,30), rate=c(5, 10), type=2)

trt  <- list(adm1, adm2)

p    <- list(name=c("Tlag","F","ka","V","k"), value=c(2,0.7,1,10,0.1))
Cc   <- list(name="Cc",time=seq(0, 60, by=0.1))
res  <- simulx(model=myModel,parameter=p,output=Cc,treatment=trt)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc)) + geom_line())
