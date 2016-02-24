# returns the indices corresponding to the positions of s in the array x
findstrcmp <- function(x, s , not=FALSE ){
  if(not){
    which( x != s ) 
  } else {
    which( x == s )
  }
  
}

funique <- function(A){
  #   [C,IA,IC] = unique(A) also returns index vectors IA and IC such that
  #   C = A(IA) and A = C(IC).  
  #
  # [C,IA,IC]= unique( A , 'first' );
  C  <- unique( A )
  IA <- match(data.frame(t(C)), data.frame(t(A)))
  IC <- match(data.frame(t(A)), data.frame(t(C)))
  #   IA   = match( C, A )
  #   IC   = match( A , C )
  ans  =
    list(arg1=C, arg2=IA, arg3=IC)
}

fsort <- function(X){
  #   [Y,I] = sort(X,DIM,MODE) also returns an index matrix I.
  #   If X is a vector, then Y = X(I).    
  
  Y    = sort( X )
  I    = match( Y, X )
  ans  = list(arg1=Y, arg2=I)
}

mklist <- function(x)
{
  s <- list()
  if (is.list(x) && is.null(names(x))){
    n <- length(x)
    for (k in (1:n)){
      xk <- x[[k]]
      if (is.list(xk) && is.null(names(xk))){
        nk <- length(xk)
        for (j in (1:nk)){
          xkj <- xk[[j]]
          if (!is.null(xkj)){
            if (is.list(xk) && is.null(names(xk)))
              s[[length(s)+1]]=xkj
            else
              s[[length(s)+1]]=list(xkj)
          }
        }
      } else {
        if (!is.null(xk))
          s[length(s)+1]=list(xk)         
      }
    }
  } else {
    if (!is.null(x))
      s[length(s)+1]=list(x)
    else
      s <- NULL
  }
  for (k in (1:length(s))){
    sk <- s[[k]]
    if (is.list(sk)){
      if (!is.null(sk$colNames)){
        s[[k]] <- data.frame(sk$value)
        names(s[[k]]) <- sk$colNames
      } else if (!is.null(sk$header)){
        s[[k]] <- data.frame(sk$value)
        names(s[[k]]) <- sk$header
      } else if (!is.null(sk$value) && is.null(sk$time)){
        s[[k]] <- sk$value
        names(s[[k]]) <- sk$name
      }
    }
  }
  return(s)
}


dpopid <- function(x,s)
{
  n <- length(x)
  r <- list(name=s,j=NULL,id=NULL,N=NULL,pop=NULL,npop=NULL)
  for (k in (1:n)){
    xk <- x[[k]]
    if (is.list(xk)) {
      if (!is.data.frame(xk)){
        if (!is.null(xk$time) && is.data.frame(xk$time) )
          xk <- xk$time
        else
          xk <- NULL
      }
      if (!is.null(xk$id)){
        idk <- as.factor(unique(xk$id))
        if (!is.null(r$id)) {
          if (!identical(idk,r$id))
            stop(paste("Different id's are defined in ",s))
        } else
          r$id <- as.factor(idk)
        r$N <- unique(c(r$N,length(idk)))
        if (length(r$N)>1)
          stop(paste('Different numbers of subjects are defined in ',s))
        r$j <- c(r$j,k)
      }
      if (!is.null(xk$pop)){
        r$npop <- unique(c(r$npop,length(unique(xk$pop))))
        if (length(r$npop)>1)
          stop(paste('Different numbers of populations are defined in ',s))
        r$pop <- c(r$pop,k)
      }
    } # else if (is.list(xk))
  }
  return(r)
}  


resample.data  <- function(data,idOri,N)
{
  n <- length(unique(idOri))
  
  iop.replace <- FALSE
  if (iop.replace==TRUE){
    new.id <- sample(1:n,N,replace=TRUE)
  } else{
    K <- floor(N/n)
    new.id <- c(rep(1:n,K),sort(sample(1:n,N-K*n,replace=FALSE)))
  }
  
  data$N <- NULL
  data$idOri <- NULL  
  for  (j in (1:length(data))){
    dataj <- data[[j]]
    for  (k in (1:length(dataj))){
      datak <- dataj[[k]]
      
      if (is.data.frame(datak)){
        if (!is.null(datak$id)){
          ik  <- which(names(datak)=="id")
          idk <- datak$id
          dkv=NULL
          for (i in 1:N){
            ji <- which(idk==new.id[i])
            dkji <- datak[ji,]
            dkji[,ik] <- i
            dkv <- rbind(dkv,dkji,deparse.level=0)
          } 
          data[[j]][[k]] <- dkv
        }
      }else if (is.list(datak)){
        for (m in (1:length(datak))){
          datam <- datak[[m]]
          if (is.data.frame(datam) && !is.null(datam$id)){
            ik  <- which(names(datam)=="id")
            idk <- datam$id
            dkv=NULL
            for (i in 1:N){
              ji <- which(idk==new.id[i])
              dkji <- datam[ji,]
              dkji[,ik] <- i
              dkv <- rbind(dkv,dkji,deparse.level=0)
            } 
            data[[j]][[k]][[m]] <- dkv
          }
        }
      }
    }
  }
  data$id <- data.frame(newId=seq(1:N),oriId=idOri[new.id])
  return(data)
}


unlistRec <- function(x,s=NULL)
{
  if (is.list(x) && is.null(names(x))){
    n <- length(x)
    for (k in (1:n)){
      r <- unlist(x[[k]],s)
    }
  } else {
    s[[length(s)+1]]=x
    z <- list(x,s)
    return(z)
  }
}