modelR_reg1 <- function(parameter,regressor,time)
{  
  with(as.list(parameter),{
    t <- time[[1]]
    regt <- regressor[[1]]$time
    regC <- regressor[[1]]$value
    C <- approx(regt,regC,t)$y
    E = Emax*C/(C+EC50)
    r <- list(C=data.frame(time=t, C=C),
              E=data.frame(time=t, E=E))
    return(r)
  })
}