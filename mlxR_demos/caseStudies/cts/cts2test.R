library(mlxR)
library(gridExtra)

#-------------------------------------
M <- 2000
vN <- c(25, 50, 75, 100)

adm.amount <- c(0, 25, 50, 100)
adm.time   <- seq(0,200,by=12)

e <- list(name=c('e'), time=0)
surv.t <- c(100,200)

pop.param   <- c(
  Tk0_pop   = 3,    omega_Tk0   = 0.2,
  V_pop     = 10,   omega_V     = 0.2,
  Cl_pop    = 1,    omega_Cl    = 0.2,
  Imax_pop  = 0.8,  omega_Imax  = 0.5,
  E0_pop    = 100,  omega_E0    = 0.1,
  IC50_pop  = 4,    omega_IC50  = 0.1,
  kout_pop  = 0.1,  omega_kout  = 0.1,
  alpha_pop = 0.5,  omega_alpha = 0.1,
  beta_pop  = 0.02, omega_beta  = 0,
  b         = 0.1
)

adm    <- list(time=adm.time, amount=NULL)
adm.n  <- length(adm.amount)
size.n <- length(vN)
surv.n <- length(surv.t)
R <- array(dim=c(M,adm.n,size.n,surv.n))

#---------------------------------------------------
ptm <- proc.time()
for (l in seq(1,size.n)){
  N <- vN[l]
  cat(paste0("\nN = ",N,"\n")) 
  g <- list(size=N, level='individual'); 
  for (k in seq(1,adm.n)){
    adm$amount <- adm.amount[k]
    cat(paste0("\nN = ",N," k = ",k,"\n")) 
    
    dataIn <- simulx(model     = "model/cts2III.txt",
                     parameter = pop.param,
                     treatment = adm,
                     output    = e, 
                     group     = g,
                     settings  = list(data.in=TRUE, load.design=TRUE))
    
    for (m in seq(1,M)){
      s <- 1000*l+100*k+10*m
      res <- simulx(data = dataIn, settings = list(seed=s))   
      te <- res$e$time[seq(2,2*N,by=2)]
      for (j in seq(1,surv.n))  
        R[m,k,l,j] <- mean(te>=surv.t[j]) 
    }
  }
} 
print(proc.time() - ptm)

#----------------------------------------------
xl   <- rep(NA,adm.n)
for (k in seq(1,adm.n))
  xl[k] <- paste0(adm.amount[k]," mg")

for (j in seq(1,surv.n)){
  par(mfrow = c(2,2), oma=c(0,0,2,0), mar=c(3,3,2,2) )
  for (l in seq(1,size.n)){
    skj <- data.frame(R[,,l,j])
    names(skj) <- xl
    boxplot(skj, ylim=c(0.2,1))
    title(main=paste0("N = ",vN[l]))
  }
  title(main=paste0("Survival at t=",surv.t[j]),outer=T,cex.main=2)
}

#----------------------------------------------
S <- 0.2
j <- 2
P.res <- NULL
yl    <- rep(NA,size.n)

for (l in seq(1,size.n)){
  yl[l] <- paste0("N=",vN[l])
  skj <- data.frame(R[,,l,j])
  dkj <- skj[,2:adm.n] -skj[,1]
  P.res <- rbind(P.res,colMeans(dkj>S))
}
colnames(P.res) <- xl[2:adm.n]
rownames(P.res) <- yl
print(P.res)

