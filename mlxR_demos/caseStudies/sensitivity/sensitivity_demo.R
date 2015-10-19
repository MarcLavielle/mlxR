library(mlxR)
library(sensitivity)
# library(randtoolbox)

#-----------------------------
sensModel <- function(x){
  
  myModel <- inlineModel("
 [LONGITUDINAL]
 input = {ka, V, k}
 EQUATION:
 Cc = pkmodel(ka, V, k)
 ")
  
  N <- dim(x)[1]
  x <- cbind((1:N),x)
  p <- list(name=c("ka", "V", "k"), 
            colNames=c("id", "ka", "V", "k"), 
            value=x)
  adm1 <- list(tfd=0, ii=12, amount=5)
  
  res <- exposure(model     = myModel, 
                  parameter = p, 
                  output    = list(name="Cc", time="steady.state"),
                  treatment = adm1)
  
  if (i.out=="tmax"){
    r <- res$Cc$tmax
  }else if(i.out=="cmax"){
    r <- res$Cc$cmax
  }else if(i.out=="auc"){
    r <- res$Cc$auc
  }else if(i.out=="all"){
    r <- res$Cc
  }
  return(r)
}


#-----------------------------
i.out <- "all"
r <- sensModel(matrix(c(0.2,0.5,10,10,0.2,0.2),nrow=2))
print(r)

#-----------------------------
l1 <- list(min = 0.2, max = 0.5)
l2 <- list(min = 5, max = 10)
l3 <- list(min = 0.05, max = 0.1)

i.out <- "auc"
x <- fast99(model = sensModel, factors = 3, n = 500,
            q = "qunif", q.arg = list(l1,l2,l3) )
print(x)
plot(x)

i.out <- "tmax"
x <- fast99(model = sensModel, factors = 3, n = 500,
            q = "qunif", q.arg = list(l1,l2,l3) )
print(x)
plot(x)

#-----------------------------
mySim <- function(r,n){
  M <- length(r)
  X <- data.frame(matrix(runif(M * n), nrow = n))
  for (m in (1:M)){
    rm <- r[[m]]
    X[,m] <- X[,m]*(rm$max-rm$min) + rm$min
  }
  return(X)
}
#-----------------------------

X1 <- mySim(list(l1,l2,l3),n=200)
X2 <- mySim(list(l1,l2,l3),n=200)
x <- sobolEff(model=sensModel, X1=X1, X2=X2, order=1, nboot=500)
print(x)
plot(x, ylim=c(-0.2,1))


