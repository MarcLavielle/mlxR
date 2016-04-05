modelR_pk3 <- function(parameter,dose,time)
{
  with(as.list(parameter),{
    w <- w_pop + rnorm(1)*omega_w
    V <- V_pop*(w/w_pop)*exp(rnorm(1)*omega_V)
    k <- k_pop*exp(rnorm(1)*omega_k)
    t <- time[[1]]
    A <- rep(0,length(t))
    d.time <- dose$time
    d.amt <- dose$amount
    nd <- length(d.time)
    for (m in (1:nd)){
      jm <- which(t>=d.time[m])
      A[jm] <- A[jm] + d.amt[m]*exp(-k*(t[jm]-d.time[m]))
    }
    r <- list(C=data.frame(time=t, C=A/V), w=w, V=V, k=k)
    return(r)
  })
}

