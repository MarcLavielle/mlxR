# ' @export


statmlx <- function(r, FUN="mean", probs=c(0.05, 0.5, 0.95), 
                    type="continuous", surv.time=NULL)
{
  if (any(!(FUN %in% c("mean","sd","median","var","quantile"))))
    stop("\n\n possible values for 'FUN' are {'mean','sd','median','var','quantile'} ")
  if (any(!(type %in% c('continuous','event'))))
    stop("\n\n possible values for 'type' are {'continuous','event'} ")
  # if (any(type=="survival") && is.null(time)) 
  #   stop("\n\n a time value, or a vector of times, should be provided when 'survival' is defined as 'FUN'")
  
  r <- r[!is.na(names(r))]
  
  if (type=="event"){
    r <- r[r$time>0,]
    it <- which(names(r) == "time")
    ie <- which(!(names(r) %in% c("pop","rep","id","time","group")))
    ig <- r[-c(it,ie)]
    uid <-  cumsum(!duplicated(ig)) 
    y <- cbind(uid,r)
    
    surv.time <- c(100,200)
    rrr <- list(uid)
    ev.nb <- aggregate(r[ie], by=rrr, "sum")
    names(ev.nb)[2] <- "nbEv"
    rev <- cbind(unique(ig), ev.nb[2]) 
    nev <- names(rev)
    ev.min <- aggregate(r[it], by=rrr, "min")
    if (!is.null(surv.time)){
      for (k in (1:length(surv.time))){
        survk <- (ev.min$time >= surv.time[k])*1
        rev   <- cbind(rev, survk)
      }
      nev <- c(nev,paste0("S",surv.time))
    }
    names(rev) <- nev
    # ev.t1 <- ev.min
    # ev.t1[ev.nb==0] <- NA
    
    r <- rev
  }
  
  list.FUN <- c("mean","sd","median","var") 
  l.FUN <- length(FUN)
  n.FUN <- NULL
  for (j in (1:l.FUN)){
    if (FUN[j] %in% list.FUN)
      n.FUN <- c(n.FUN,FUN[j])
    else 
      n.FUN <- c(n.FUN,paste0("p",probs*100))
  }
  
  rrr <- list()
  nr <- NULL
  if (!is.null(r$pop)){
    rrr[[length(rrr)+1]] <- r$pop 
    nr <- c(nr, "pop")
  }
  if (!is.null(r$rep)){
    rrr[[length(rrr)+1]] <- r$rep 
    nr <- c(nr, "rep")
  }
  if (!is.null(r$time)){
    rrr[[length(rrr)+1]] <- r$time 
    nr <- c(nr, "time")
  }
  if (!is.null(r$group)){
    rrr[[length(rrr)+1]] <- r$group 
    nr <- c(nr, "group")
  }  
  r$pop <- r$rep <- r$group <- r$time <- r$id <- NULL
  
  sr <- NULL
  for (j in (1:l.FUN)){
    statj <- FUN[j]
    if (statj %in% list.FUN) {
      if (length(rrr)>0)
        X <- aggregate(r, by=rrr, statj)
      else
        X <- as.data.frame(t(sapply(r,statj)))
      n.srj <- paste0(names(r),".",statj)
    } else {
      if (length(rrr)>0){
        aX <- aggregate(r, by=rrr, statj, probs=probs)
        nX <- length(aX)
        X <- NULL
        for (k in (1:nX))
          X <- cbind(X, aX[[k]])
        X <- as.data.frame(X)
      } else {
        aX <- sapply(r,statj, probs=pr)
        X <- as.data.frame(t(as.vector(aX)))
      }
      aa <- apply(expand.grid(names(r),paste0("p",probs*100)), 1, paste, collapse=".")  
      n.srj <- t(matrix(aa,nrow=ncol(r)))
    }
    names(X) <- c(nr,n.srj)
    if (j==1)
      sr <- X
    else 
      sr <- merge(sr, X)
  }
  return(sr)
}



