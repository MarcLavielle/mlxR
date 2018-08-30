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
#' @param time a vector of time points where the survival function is evaluated.
#' @param cens if \code{TRUE} right censoring times are diplayed.
#' @param plot if \code{TRUE} the estimated survival function is displayed, if \code{FALSE}
#' the values are returned
#' @param color  color to be used for the plots (default="#e05969")
#' @param group  variable to be used for defining groups (by default, \samp{group} is used when it exists)
#' @param facet  makes subplots for different groups if \code{TRUE} 
#' @param labels  vector of strings 
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
#' @importFrom ggplot2 ggplot geom_point theme aes geom_line xlab ylab facet_wrap facet_grid
#' @importFrom stats qnorm approx
#' @export         
kmplotmlx  <-  function(r, index=1, level=NULL, time=NULL, cens=TRUE, plot=TRUE, 
                        color="#e05969", group=NULL, facet=TRUE, labels=NULL)
{ 
  y <- NULL
  if (is.vector(r))
    r <- data.frame(time=r, y=1)
  if (dim(r)[2] ==1) {
    names(r) <- "time"
    r$y <- 1
  }
  r.name <- attr(r,"name")
  if (length(r.name)>1 || !any(names(r)==r.name)) {
    if (any(names(r)=="status")) {
      r.name <- "status"
    } else if (any(names(r)=="event")) {
      r.name <- "event"
    } else {
      dn <- setdiff(names(r),c("id","time","group",group))
      if (length(dn)==1)
        r.name <- dn
    }
  }
  names(r)[names(r)==r.name] <- "y"
  
  if (!is.null(r$group) & is.null(group)) {
    group <- "group"
  } else if (length(group)==1 && group=="none") {
    group = NULL
  }
  
  if (is.null(r$id)) 
    r$id <- (1: dim(r)[1])
  
  if (length(unique(r$id)) == dim(r)[1]) {
    r0 <- r
    r0$time <- 0
    r0$y <- 0
    r <- rbind(r0,r)
  }
  
  if (is.data.frame(group)) {
    attr.name <- attr(r,"name")
    r <- merge(r,group,by="id",sort=FALSE)
    attr(r,"name") <- attr.name
    group <- names(group)
    group <- group[-which(group=="id")]
  }
  
  if (!is.null(labels)) {
    if (length(group)==1) 
      labels <- ifelse(is.list(labels),labels, list(labels))
    for (k in (1: length(group)))
      r[[group[k]]] <- factor(r[[group[k]]], labels=labels[[k]])
  }
  
  if (!is.null(group)) {
    ig <- interaction(r[group])
    ig <- factor(ig)
  } else {
    n.tot <- dim(r)[1]
    ig <- factor(rep(1, n.tot))
    group <- "group"
    r$group <- ig
  }
  r$id <- interaction(ig,r[["id"]])
  r$col <- ig
  ug <- levels(ig)
  ng <- length(ug)
  
  
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
  
  if (index=="numberEvent") {
    r0 <- subset(r, y==0 & time>0)
    index="numberEvent0"
    if (!is.null(r0) & length(unique(r0$time))>1) {
      a=aggregate(r$time, list((!r$y==0),r$id, r$rep), function(x){ if (length(x)==0) -Inf else max(x)}, drop=FALSE)
      a0 <- a$x[seq(1,length(a$x),by=2)]
      a1 <- a$x[seq(2,length(a$x),by=2)]
      if (length(unique(a0[which(a0>a1)]))>1)
        index="numberEvent"
    }
  }
  
  D <- D0 <- NULL
  for (jrep in (1:nrep)) {
    rj <- r[r$rep==urep[jrep],]
    uid <- unique(rj$id)
    N <- length(uid)
    i0 <- NULL
    
    if (is.numeric(index)) {
      test.index <- FALSE
      r0 <- r1 <- r10 <- NULL
      for (i in seq(1,N)) {
        ri <- rj[rj$id==uid[i],]
        cyi <- cumsum(ri$y)
        if (max(cyi)>1)
          test.index <- TRUE
        ri$y <- 1
        if (any(index<=cyi)) {
          it <- min(which(index<=cyi))
          if (is.null(r1))
            r1 = ri[it,]
          else
            r1[dim(r1)[1]+1,] = ri[it,]
        } else {
          if (is.null(r0))
            r0 = ri[dim(ri)[1],]
          else
            r0[dim(r0)[1]+1,] = ri[dim(ri)[1],]
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
      
      g=re$col
      
      re$id <- NULL
      
      S <- Se <- T <- G <- NULL
      S0 <- T0 <- G0 <- NULL
      t0=min(rj$time)
      if (min(rj$time[rj$y==1]) < min(rj$time[rj$y==0]))
        t0 <- 0
      for (kg in ug) {
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
          tu <- ru$time[-1]
          Tk<-c(t0,rep(tu,each=2))
        } else {
          urt <- sort(unique(r$time))
          Sku <- approx(x=ru$time,y=Sk,xout=urt,method="constant",rule=2)$y
          Sa <- approx(x=urt,y=Sku,xout=time, rule=2)$y
          seku <- approx(x=ru$time,y=sek,xout=urt,method="constant",rule=2)$y
          sea <- approx(x=urt,y=seku,xout=time, rule=2)$y
          S <- c(S,Sa)
          Se <- c(Se,sea)
          Tk<-time
        }
        T<-c(T,Tk)
        jk1 <- which(ig==kg)[1]
        Gk <- r[group][rep(jk1,length(Tk)),]
        if (!is.data.frame(Gk)) {
          Gk <- data.frame(Gk)
          names(Gk) <- names(r[group])
        }
        G <- rbind(G,Gk)
        
        if (cens) {
          if (!is.null(time))
            i0 <- which(ru$c>0 & ru$time>=min(time) & ru$time<=max(time))
          else
            i0 <- which(ru$c>0)
          if (length(i0)>0) {
            T0 <- c(T0,ru$time[i0])
            S0 <- c(S0,Sk[i0])
            G0k <- r[group][rep(jk1,length(i0)),]
            if (!is.data.frame(G0k)) {
              G0k <- data.frame(G0k)
              names(G0k) <- names(r[group])
            }
            G0 <- rbind(G0, G0k)
          }
        }
      }
    } else if (index=="numberEvent0") {
      
      
      g <- rj$col
      t0=min(rj$time)
      S <- T <- G <- NULL
      for (kg in ug) {
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
          Sa <- approx(x=ut,y=cy,xout=time, rule=2)$y
          S <- c(S,Sa)
          Tk<-time
        }
        T<-c(T,Tk)
        jk1 <- which(ig==kg)[1]
        Gk <- r[group][rep(jk1,length(Tk)),]
        if (!is.data.frame(Gk)) {
          Gk <- data.frame(Gk)
          names(Gk) <- names(r[group])
        }
        G <- rbind(G,Gk)
      }
      
    } else if (index=="numberEvent") {
      
      
      srj <- aggregate(rj$y, list(rj$id), sum)
      k.max <- max(srj[,2])
      
      urt <- sort(unique(r$time))
      t0 <- urt[1]
      if (is.null(time)) 
        Tk<-c(t0,rep(urt[-1],each=2))
      else 
        Tk<-time
      
      
      S.tot <- 0
      for (k in (1:k.max)) {
        r0 <- r1 <- NULL
        for (i in seq(1,N)) {
          ri <- rj[rj$id==uid[i],]
          cyi <- cumsum(ri$y)
          ri$y <- 1
          if (any(k<=cyi)) {
            it <- min(which(k<=cyi))
            if (is.null(r1))
              r1 = ri[it,]
            else
              r1[dim(r1)[1]+1,] = ri[it,]
          } else {
            if (is.null(r0))
              r0 = ri[dim(ri)[1],]
            else
              r0[dim(r0)[1]+1,] = ri[dim(ri)[1],]
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
        
        g=re$col
        
        re$id <- NULL
        
        S <- NULL
        for (kg in ug) {
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
          Sk <- vector(length=nt+1)
          Sk[1] <- 1
          for (j in seq(2,nt+1)) {
            nj <- nj - ru$c[j-1] - ru$d[j-1]
            pj <- (nj - ru$d[j])/nj
            Sk[j] <- Sk[j-1]*pj
          }
          
          Sku <- approx(x=ru$time,y=Sk,xout=urt,method="constant",rule=2)$y
          if (is.null(time)) {
            S <- c(S,rep(Sku,each=2))
            S <- S[-length(S)]
          } else {
            Sa <- approx(x=urt,y=Sku,xout=time, rule=2)$y
            S <- c(S,Sa)
          }
        }
        S.tot <- S.tot + (1-S)
      }
      S <- S.tot
      T <- G <- NULL
      for (kg in ug) {
        T<-c(T,Tk)
        jk1 <- which(ig==kg)[1]
        Gk <- r[group][rep(jk1,length(Tk)),]
        if (!is.data.frame(Gk)) {
          Gk <- data.frame(Gk)
          names(Gk) <- names(r[group])
        }
        G <- rbind(G,Gk)
      }
      
      
      
    } else {
      cat("\nindex should be an integer of the string 'numberEvent'\n")
      return()
    }
    
    Dk <- data.frame(time=T,S)
    if (plot==TRUE | ng>1) 
      Dk <- cbind(G, Dk)
    if (nrep>1)
      Dk <- cbind(data.frame(rep=factor(jrep)), Dk)
    if (!is.null(level)) {
      alpha <- (1-level)/2
      s1 <- log(S/(1-S)) + Se*qnorm(alpha)  # logit 
      s2 <- log(S/(1-S)) + Se*qnorm(1-alpha)
      nan.s1 <- which(is.nan(s1))
      nan.s2 <- which(is.nan(s2))
      s1[is.nan(s1)] <- s1[nan.s1-1]
      s2[is.nan(s2)] <- s2[nan.s2-1]
      Dk$S1 <- 1/(1+exp(-s1))
      Dk$S2 <- 1/(1+exp(-s2))
    }
    D <- rbind(D, Dk)
    
    if (exists("T0")  && !is.null(T0)) {
      D0k <- data.frame(time=T0,S0)
      if (plot==TRUE | ng>1) 
        D0k <- cbind(G0, D0k)
      if (nrep>1)
        D0k$rep <- factor(jrep)
      D0 <- rbind(D0, D0k)
    }
    
  }
  
  S1 <- S2 <- Dgrep <- NULL
  
  if (plot==TRUE) {
    if (facet==TRUE) {
      if (nrep==1) {
        plot1=ggplotmlx(data=D) +  geom_line(aes(x=time, y=S), color=color, size=1)
      } else {
        plot1=ggplotmlx(data=D) +  geom_line(aes(x=time, y=S, group=rep), color=color, size=1)
      }
      if (!is.null(level)){
        plot1=plot1+geom_line(data=D,aes(x=time, y=S1), linetype="dotted", color=color, size=0.8) +
          geom_line(data=D,aes(x=time, y=S2), linetype="dotted", color=color, size=0.8)
      }
      
    } else {
      D$group <- interaction(D[group])
      if (nrep==1) {
        plot1=ggplotmlx() +  geom_line(data=D, aes(x=time, y=S, color=group), size=1)
      } else {
        D$Dgrep <- interaction(D$group,D$rep)
        plot1 <- ggplotmlx() +  geom_line(data=D, aes(x=time, y=S, colour=group, group=Dgrep), size=1)
      }
      if (!is.null(level)){
        if (nrep==1) {
          plot1=plot1+geom_line(data=D, aes(x=time, y=S1, colour=group), linetype="dotted", size=0.8) +
            geom_line(data=D, aes(x=time, y=S2, colour=group), linetype="dotted", size=0.8)
        } else {
          plot1=plot1+geom_line(data=D, aes(x=time, y=S1, colour=group, group=Dgrep), linetype="dotted", size=0.8) +
            geom_line(data=D, aes(x=time, y=S2, colour=group, group=Dgrep), linetype="dotted", size=0.8)
        }
      }
    }
    
    if (index=="numberEvent" | index=="numberEvent0") {
      plot1 <- plot1 + xlab("time") + ylab("mean number of events per individual") 
    } else if (test.index==FALSE) {
      plot1 <- plot1 + xlab("time") + ylab("survival") 
    } else {
      plot1 <- plot1 + xlab("time") + ylab(paste0("survival (event number ", index, ")" ))
    }
    
    if (exists("S0") && !is.null(S0)) {
      if (facet==TRUE) {
        plot1 <- plot1 + geom_point(data=D0, aes(x=time,y=S0), color=color, size=3)
      } else {
        D0$group <- interaction(D0[group])
        plot1 <- plot1 + geom_point(data=D0, aes(x=time,y=S0, colour=group), size=3)
      }
    }
    
    plot1 <- plot1 + theme(legend.position="none")
    
    if (ng>1) {
      if (facet==TRUE) {
        if (length(group)==1) 
          plot1 <- plot1 + facet_wrap(group)
        if (length(group)==2) 
          plot1 <- plot1 + facet_grid(paste(group[1],"~",group[2]))
      } else {
        plot1 <- plot1 + theme(legend.justification=c(0,0), legend.position=c(0.05,0.1))
      }
    }
    
    res <- plot1
  } else {
    res <- list(surv=D, cens=D0)
  }
  return(res)
}


completemlx <- function(xi,ti,t) {
  nt <- length(t)
  nti <- length(ti)
  x <- rep(xi[nti],nt)
  for (k in (1: (nti-1))) {
    itk <- which( t>=ti[k] & t < ti[k+1])
    x[itk] <- xi[k]
  }
  w <- rep(1,nt)
  if (xi[nti]==0)
    w[which(t>ti[nti])] <- 0
  return(list(w=w, x=x))
}
