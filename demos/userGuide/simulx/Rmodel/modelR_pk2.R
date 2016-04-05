modelR_pk2 <- function(parameter,dose,time)
{  
  MMmod <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      k <- Cl/V
      ddt_Ad = -ka*Ad
      ddt_Ac = ka*Ad - k*Ac + k21*Ap - k12*Ac + k31*Aq - k13*Ac
      ddt_Ap = -k21*Ap + k12*Ac 
      ddt_Aq = -k31*Aq + k13*Ac
      return(list(c(ddt_Ad, ddt_Ac, ddt_Ap, ddt_Aq)))
    })
  }
  
  yini  <- c(Ad=0, Ac=0, Ap=0, Aq=0)
  
  t <- time[[1]]
  
  t <- sort(c(t,dose$data$time))
  out   <- lsode(yini, t, MMmod, parameter, events=dose)
  i0 <- which(diff(t)==0)
  out <- out[-i0,]
  
  C <- data.frame(time=out[,"time"], C=out[,"Ac"]/parameter["V"])
  r <- list(C=C)
  return(r)
}

