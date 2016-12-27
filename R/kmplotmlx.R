#' Kaplan Meier plot
#' 
#' Plot empirical survival functions using the Kaplan Meier estimate.
#' 
#' See http://simulx.webpopix.org/mlxr/kmplotmlx/ for more details.
#' @param r a data frame with a column \samp{id}, a column \samp{time}, 
#' a column with values and possibly a column \samp{group}.
#' @param index an integer: \code{index=k} means that the survival function for the k-th event is displayed. 
#' Default is \code{index=1}.
#' @param level a number between 0 and 1:  confidence interval level. 
#' @param cens if \code{TRUE} right censoring times are diplayed.
#' @param plot if \code{TRUE} the estimated survival function is displayed, if \code{FALSE}
#' the values are returned
#' 
#' @return 
#' a ggplot object if \code{plot=TRUE} ; otherwise, a list with fields:
#' \itemize{
#'   \item \code{surv} a data frame with columns  \code{T} (time), \code{S} (survival), possibly \code{(S1, S2)} (confidence interval) and possibly \code{group} 
#'   \item \code{cens} a data frame with columns  \code{T0} (time), \code{S0} (survival) and possibly \code{group} 
#' }
#' @examples
#' \dontrun{
#' tteModel1 <- inlineModel("
#'   [LONGITUDINAL]
#'   input = {beta,lambda}  
#'   EQUATION:
#'   h=(beta/lambda)*(t/lambda)^(beta-1)
#'   DEFINITION:
#'   e = {type=event, maxEventNumber=1, rightCensoringTime=70, hazard=h}
#'   ")
#'
#'   p1   <- c(beta=2.5,lambda=50)
#'   e    <- list(name='e', time=0)
#'   res1 <- simulx(model=tteModel1, parameter=p1, output=e, group=list(size=100))
#'   pl1  <- kmplotmlx(res1$e,level=0.95)
#'   print(pl1)
#' 
#'   p2   <- c(beta=2,lambda=45)
#'   g1   <- list(size=50, parameter=p1)
#'   g2   <- list(size=100, parameter=p2)
#'   res2 <- simulx(model=tteModel1, output=e, group=list(g1,g2))
#'   pl2  <- kmplotmlx(res2$e)
#'   print(pl2)
#' }
#' @importFrom ggplot2 ggplot geom_point theme aes geom_line xlab ylab
#' @importFrom stats qnorm
#' @export         
kmplotmlx  <-  function(r, index=1, level=NULL, time=NULL, cens=TRUE, plot=TRUE)
{ 
  r.name <- attr(r,"name")
  if (length(r.name)>1 || !any(names(r)==r.name)) {
    if (any(names(r)=="status"))
      r.name <- "status"
    if (any(names(r)=="event"))
      r.name <- "event"
  }
  names(r)[names(r)==r.name] <- "y"
  
  if (!is.null(r$rep)) {
    if (!is.null(level)) {
      if (is.null(time)) 
        time <- seq(min(r$time), max(r$time), length.out=100)
      cens <- FALSE
    }
  } else {
    r$rep <- 1
  }
  urep <- unique(r$rep)
  nrep <- length(urep)
  
  D <- D0 <- NULL
  for (jrep in (1:nrep)) {
    rj <- r[r$rep==urep[jrep],]
    uid <- unique(rj$id)
    N <- length(uid)
    i0 <- NULL
    
    if (is.numeric(index)) {
      test.index <- FALSE
      r0 <- r1 <- NULL
      for (i in seq(1,N)) {
        ri <- rj[rj$id==uid[i],]
        cyi <- cumsum(ri$y)
        if (max(cyi)>1)
          test.index <- TRUE
        ri$y <- 1
        if (any(index<=cyi)) {
          it <- min(which(index<=cyi))
          r1 <- rbind(r1,ri[it,]) 
        } else {
          di <- dim(ri)[1]
          r0 <- rbind(r0,ri[di,])  
        }
      }
      if (!is.null(r0)) {
        names(r0)[names(r0)=="y"] <- "c"
        r0$d <- 0
      }
      if (!is.null(r1)) {
        names(r1)[names(r1)=="y"] <- "d"
        r1$c <- 0
      }
      re <- rbind(r1,r0)
      re <- re[with(re, order(time)), ]
      
      if (any( "group" %in% names(re) )) {
        g=as.numeric(levels(re$group))[re$group]
      } else {
        g=rep(1,length(re$id))
      }
      ng=max(g)
      re$id <- NULL
      
      S <- Se <- T <- G <- NULL
      S0 <- T0 <- G0 <- NULL
      t0=min(rj$time)
      for (kg in seq(1,ng)) {
        rk<-re[g==kg,]
        Nk <- dim(rk)[1]
        ut <- uniquemlx(rk$time)
        tu <- ut$uniqueValue
        iu <- ut$sortIndex
        nt <- length(tu)
        ru <- data.frame(time=c(t0,tu),d=0,c=0)
        nj <- Nk
        for (j in seq(1,nt)) {
          ru$c[j+1] <- sum(rk$c[iu==j])
          ru$d[j+1] <- sum(rk$d[iu==j])
        }
        nj <- Nk
        sek <- vector(length=nt+1)
        Sk <- vector(length=nt+1)
        Sk[1] <- 1
        sek[1] <- 0
        V <- 0
        for (j in seq(2,nt+1)) {
          nj <- nj - ru$c[j-1] - ru$d[j-1]
          pj <- (nj - ru$d[j])/nj
          Sk[j] <- Sk[j-1]*pj
          V <- V + ru$d[j]/nj/(nj - ru$d[j])
          sek[j] <- sqrt(V)/(1-Sk[j])  # logit 
        }
        sek[which(is.nan(sek))] <- 0
        
        if (is.null(time)) {
          S <- c(S,rep(Sk,each=2))
          S <- S[-length(S)]
          Se <- c(Se,rep(sek,each=2))
          Se <- Se[-length(Se)]
          Tk<-c(t0,rep(tu,each=2))
        } else {
          Sa <- approx(x=c(t0,tu),y=Sk,xout=time, rule=1)$y
          sea <- approx(x=c(t0,tu),y=sek,xout=time, rule=1)$y
          S <- c(S,Sa)
          Se <- c(Se,sea)
          Tk<-time
        }
        T<-c(T,Tk)
        G<-c(G,rep(kg,length(Tk)))
        
        if (cens) {
          i0 <- which(ru$c>0)
          if (length(i0)>0) {
            T0 <- c(T0,ru$time[i0])
            S0 <- c(S0,Sk[i0])
            G0 <- c(G0, rep(kg, length(i0) ))
          }
        }
      }
    } else if (index=="numberEvent") {
      level=NULL
      if (any( "group" %in% names(r) )) {
        g=as.numeric(levels(rj$group))[rj$group]
      } else {
        g=rep(1,length(rj$id))
      }
      ng=max(g)
      
      t0=min(rj$time)
      S <- T <- G <- NULL
      for (kg in seq(1,ng)) {
        rk<-rj[g==kg,]
        uidk <- unique(rk$id)
        Nk <- dim(rk)[1]
        ut <- sort(unique(c(t0,rk$time)))
        nt <- length(ut)
        cy <- cw <- 0
        for (i in uidk) {
          ri <- rj[rj$id==i,]
          cyi <- completemlx(xi=cumsum(ri$y),ti=ri$time,t=ut)
          cy <- cy + cyi$x*cyi$w
          cw <- cw + cyi$w
        } 
        cy <- cy/cw
        
        if (is.null(time)) {
          Sk <- rep(cy,each=2)
          S <- c(S,Sk[-length(Sk)])
          Tk <- rep(ut,each=2)
          Tk <- Tk[-1]
        } else {
          Sa <- approx(x=ut,y=cy,xout=time, rule=1)$y
          S <- c(S,Sa)
          Tk<-time
        }
        T<-c(T,Tk)
        G<-c(G,rep(kg,length(Tk)))
      }
      
    } else {
      cat("\nindex should be an integer of the string 'numberEvent'\n")
      return()
    }
    
    Dk <- data.frame(time=T,S)
    if (plot==TRUE | ng>1) 
       Dk <- cbind(data.frame(group=factor(G)), Dk)
    if (nrep>1)
      Dk <- cbind(data.frame(rep=jrep), Dk)
        if (!is.null(level)) {
      alpha <- (1-level)/2
      s1 <- log(S/(1-S)) + Se*qnorm(alpha)  # logit 
      s2 <- log(S/(1-S)) + Se*qnorm(1-alpha)
      Dk$S1 <- 1/(1+exp(-s1))
      Dk$S2 <- 1/(1+exp(-s2))
    }
    D <- rbind(D, Dk)
    
    if (length(i0)>0) {
      D0k <- data.frame(time=T0,S0)
      if (plot==TRUE | ng>1) 
        D0k <- cbind(data.frame(group=factor(G0)), D0k)
      if (nrep>1)
        D0k <- cbind(data.frame(rep=jrep), D0k)
      D0 <- rbind(D0, D0k)
    }
    
  }
  
  
  if (plot==TRUE) {
    if (nrep==1) {
      plot1=ggplotmlx(data=D) +  geom_line(aes(x=time, y=S, colour=group), size=1)
    } else {
      plot1=ggplotmlx(data=D) +  geom_line(aes(x=time, y=S, colour=group, group=interaction(group,as.factor(rep))), size=1)
    }
    if (!is.null(level)){
      plot1=plot1+geom_line(aes(x=time, y=S1, colour=group), linetype="dotted", size=0.8) +
        geom_line(aes(x=time, y=S2, colour=group), linetype="dotted", size=0.8)
    }
    if (index=="numberEvent") {
      plot1 <- plot1 + xlab("time") + ylab("mean number of events per individual") 
    } else if (test.index==FALSE) {
      plot1 <- plot1 + xlab("time") + ylab("survival") 
    } else {
      plot1 <- plot1 + xlab("time") + ylab(paste0("survival (event number ", index, ")" ))
    }
    
    if (length(i0)>0)
      plot1 <- plot1 + geom_point(data=D0, aes(x=time,y=S0, colour=group), size=4)
    
    if (ng>1) {
      plot1 <- plot1 + theme(legend.position=c(0.1,0.4))
    }else{
      plot1 <- plot1 + theme(legend.position="none")
    }  
    res <- plot1
  } else {
    res <- list(surv=D, cens=D0)
  }
  return(res)
}



#--------------------------------------------------------
uniquemlx <- function(x) { 
  d <- !duplicated(x) 
  u=list(uniqueValue=x[d], firstIndex=which(d), sortIndex=match(x,x[d])) 
  return(u)
}


completemlx <- function(xi,ti,t) {
  nt <- length(t)
  nti <- length(ti)
  x <- rep(xi[nti],nt)
  for (k in (1: (nti-2))) {
    itk <- which( t>=ti[k] & t < ti[k+1])
    x[itk] <- xi[k]
  }
  w <- rep(1,nt)
  if (xi[nti]==0)
    w[which(t>ti[nti])] <- 0
  return(list(w=w, x=x))
}
