simulx2surv <- function(e)
{
  id<- as.numeric(e$id)
  N  <- length(unique(id))
  t1 <- NULL
  t2 <- NULL
  ev <- NULL
  for (i in 1:N){
    ri <- e[which (id==i),]
    ji <- which(ri$e==1)
    if (length(ji>0)){
      ji1 <- ji[1]
      if (ji1==2){
        t1 <- c(t1,ri$time[1])
        t2 <- c(t2,ri$time[2])
        ev <- c(ev,c(1))
      }else{
        ji1 <- ji[1]
        t1 <- c(t1,ri$time[c(1,(ji1-1))])
        t2 <- c(t2,ri$time[c(ji1-1,ji1)])
        ev <- c(ev,c(0,1))
      }
    }else{
      t1 <- c(t1,ri$time[1])
      t2 <- c(t2,ri$time[length(ri$time)])
      ev <- c(ev,0)
    }
  }
  r <- list(t1=t1,t2=t2,e=ev)
  return(r)
}
