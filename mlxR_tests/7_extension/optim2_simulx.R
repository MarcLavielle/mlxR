
myModel = inlineModel("
[LONGITUDINAL]
input = {V, k}
EQUATION:
Cc = pkmodel(V,k)
")

adm <- list(time=seq(0, 200, by=12), amount=100)

N <- 100
V_pop <- 10
k_pop <- 0.1
omega_V <- 0.3
omega_k <- 0.2
V=V_pop*exp(rnorm(N,0,omega_V))
k=k_pop*exp(rnorm(N,0,omega_k));
pv=matrix(ncol=3,nrow=N,byrow=FALSE,data=c(seq(1,N),V,k))
p  <- list(name=c('V', 'k'), colNames=c("id","V","k"), value=pv)
Cc <- list(name='Cc',time=seq(200, 224, by=2))
s <- list(data.in=TRUE)
dataIn <- simulx(model=myModel,parameter=p,output=Cc,treatment=adm,settings=s)

M=200
s <- list(load.design=FALSE)
dd <- list(length=M)
for(m in seq(1,M)){
  V=V_pop*exp(rnorm(N,0,omega_V))
  k=k_pop*exp(rnorm(N,0,omega_k));
  pvm=matrix(ncol=2,nrow=N,byrow=FALSE,data=c(V,k))
  dataIn$individual_parameters$value <- pvm
  ddm=simulx(data=dataIn, settings=s)  
  dd[[m]]=ddm  
}
