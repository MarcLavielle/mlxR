modelR_pk1 <- function(parameter,dose,time)
{
  with(as.list(parameter),{
    V_pred<- V_pop*(w/70) 
    V <- V_pred*exp(rnorm(1)*omega_V)
    k <- k_pop*exp(rnorm(1)*omega_k)
    t1 <- time[[1]]
    t2 <- time[[2]]
    A1 <- rep(0,length(t1))
    A2 <- rep(0,length(t2))
    d.time <- dose$time
    d.amt <- dose$amount
    nd <- length(d.time)
    for (m in (1:nd)){
      jm1 <- which(t1>=d.time[m])
      A1[jm1] <- A1[jm1] + d.amt[m]*exp(-k*(t1[jm1]-d.time[m]))
      jm2 <- which(t2>=d.time[m])
      A2[jm2] <- A2[jm2] + d.amt[m]*exp(-k*(t2[jm2]-d.time[m]))
    }
    y2 <- A2/V + rnorm(length(A2),0,a)
    C <- data.frame(time=t1, C=A1/V)
    y <- data.frame(time=t2, y=y2) 
    r <- list(C,y,V,k)
    return(r)
  })
}

