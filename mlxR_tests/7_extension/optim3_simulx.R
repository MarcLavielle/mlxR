
myModel = inlineModel("

[LONGITUDINAL]
input = {V, k}
EQUATION:
D=100
Cc = D/V*exp(-k*t)

[INDIVIDUAL]
input = {V_pop, omega_V, k_pop, omega_k}
DEFINITION:
V = {distribution=logNormal, reference=V_pop, sd=omega_V}
k = {distribution=logNormal, reference=k_pop, sd=omega_k}

[POPULATION]
input = {Vs, gV, ks, gk}
DEFINITION:
V_pop = {distribution=normal, mean=Vs, sd=gV}
k_pop = {distribution=normal, mean=ks, sd=gk}
")

N <- 200
adm <- list( time=0, amount=100)

p   <- list( name=c('Vs', 'gV', 'ks', 'gk', 'omega_V', 'omega_k'), 
             value=c(10, 0.5, 0.1, 0.01, 0.3, 0.3))

Cc  <- list( name='Cc', time=seq(0, 30, by=1))

g <- list(size = N, level="individual",output=Cc,treatment=adm)

p0 <- p
p0$value[c(2,4)]=0
res <- simulx(model=myModel,parameter=p0,group=g)

t <- Cc$time
n <- length(t)
m <- matrix(res$Cc[,3],nrow=n)
prct=apply(m,1,quantile, probs = c(0.1,0.5,0.9),  na.rm = TRUE)

M=200
P1 <- matrix(nrow=n,ncol=M)
P2 <- matrix(nrow=n,ncol=M)
P3 <- matrix(nrow=n,ncol=M)

g <- list(size = N, level="individual",output=Cc,treatment=adm)
s1 <- list(data.in=TRUE)
dataIn <- simulx(model=myModel,parameter=p,group=g,settings=s1)

s3 <- list(load.design=FALSE)
for(i in seq(1,M)){
   dd=simulx(data=dataIn,settings=s3)  
  v=dd[[1]]$Cc
  m <- data.frame(matrix(unlist(v), nrow=n, byrow=F))
  pri <- apply(m,1,quantile, probs = c(0.1,0.5,0.9),  na.rm = TRUE)
  P1[,i] <- pri[1,]
  P2[,i] <- pri[2,]
  P3[,i] <- pri[3,]
}

c1 <- apply(P1,1,quantile, probs = c(0.05,0.95),  na.rm = TRUE)
c2 <- apply(P2,1,quantile, probs = c(0.05,0.95),  na.rm = TRUE)
c3 <- apply(P3,1,quantile, probs = c(0.05,0.95),  na.rm = TRUE)

#--  plot results -------------------------------------
par(mfrow = c(1,2))
plot(t,prct[3,],xlab="time (hours)",ylab="concentration (mg/l)",ylim=c(0,17),type="l", lwd=2, col="blue")
lines(t,prct[1,], type="l", lwd=2, col="green")
lines(t,prct[2,], type="l", lwd=2, col="red")
legend(x="topright",legend=c("90%","50%","10%"),col=c("blue","red","green"), lty=1)
title(main="10%, 50% and 90% order quantiles")

plot(t,c3[2,], xlab="time (hours)",ylab="",ylim=c(0,17), type="l", col="white")
polygon(c(t,rev(t)),c(c3[1,],rev(c3[2,])),col="cadetblue3",border = NA)
polygon(c(t,rev(t)),c(c2[1,],rev(c2[2,])),col="salmon2",border = NA)
polygon(c(t,rev(t)),c(c1[1,],rev(c1[2,])),col="darkolivegreen2",border = NA)
title(main="90% confidence intervals")
#legend(x="topright",legend=c("90%","50%","10%"),col=c("blue","red","green"), lty=1)

