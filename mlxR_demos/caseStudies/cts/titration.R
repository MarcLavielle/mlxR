titration <- function(model=NULL,group=NULL,treatment=NULL,
                      parameter=NULL,output=NULL,rule=NULL, 
                      settings=NULL,regressor=NULL,varlevel=NULL)
{   
#   settings$load.design=TRUE
  if (!is.null(names(rule))) 
    rule=list(rule) 
  
  rule <- format.rule(rule)
  nr <- length(rule$time)
  if (any("size" %in% names(group)))
  {N <- group$size}else{N <- 1}
  trtk <- format.trt(treatment,seq(1,N))
  trtk <- trtk[[1]]
  outk <- list(name=rule$name)
  nor  <- length(unique(rule$name))
  for (k in seq(1,nr)){
    outk$time <- rule$time[1:k]
    resk <- simulx(model=model,treatment=trtk,output=outk,
                  parameter=parameter,settings=settings,
                  regressor=regressor,varlevel=varlevel)
    tk <- rule$time[k]
    jk <- which(!is.na(rule$jcond[k,]))
    for (j in seq(1,length(jk))){
      jkj <- jk[[j]]
      nk <- rule$name[jkj]
      eval(parse(text=paste0("foo <- resk$",nk)))
      foo <- foo[foo$time==tk,]
      eval(parse(text=paste0(nk," <- foo$",nk)))
      ik <- which(eval(rule$condition[jkj]))
      jtrtik <- which( (trtk$id %in% ik) &  (trtk$time>tk) )
      trtk$amount[jtrtik] <- trtk$amount[jtrtik]*rule$factor[jkj]
    }
    
  } 
  
  res <- simulx(model=model,treatment=trtk,output=output,
                parameter=parameter,settings=settings,
                regressor=regressor, varlevel=varlevel)
  for (j in (seq(1,nor)))
     rnj<- rule$name[[j]]
     Rj <- resk[[j]] 
     Rj <- Rj[with(Rj, order(id, time)), ]
    rj <- res[[rnj]]
    if (!is.null(rj)){
      r=merge(Rj,rj[,1:2],by.x=c("id","time"),by.y=c("id","time"),all.y=TRUE)
      rrj <- r[,rnj]
      ij <- which(!is.na(rrj))
      rj[ij,rnj] <- rrj[ij]
      res[[rule$name[[j]]]] <- rj
    }
  return(res)
}


# ------------------------------------------
format.rule <- function(r0)
{  
  nr <- length(r0)
  for (k in seq(1,nr)){
    mk <- data.frame(id=rep(k,length(r0[[k]]$time)),time=r0[[k]]$time)
    if (k==1){
      M <- mk
    }else{
      M <- merge(M,mk,by.x="time",by.y="time",all=TRUE)
    }
  }
  M2 <- M
  M2$time=NULL
  r1 <- list(time=M$time, 
             jcond=M2)
  cond <- rep(NA,nr)
  name <- rep(NA,nr)
  fact <- rep(NA,nr)
  for (k in seq(1,nr)){
    cond[k] <- parse(text=r0[[k]]$condition)
    name[k] <- r0[[k]]$name
    fact[k] <- r0[[k]]$fact
  }    
  r1$condition <- cond 
  r1$factor <- fact 
  r1$name <- name 
  
  return(r1)
}  

format.trt <- function(treatment,uN) 
{
  N <- length(uN)
  if (!is.null(names(treatment))){  
    treatment <- list(treatment) 
  }
  
  for (k in seq(1,length(treatment))){
    trtk <- treatment[[k]]
    if (any('colNames' %in% names(trtk))){
      pp <- as.data.frame(trtk$value)
      names(pp) <- trtk$colNames
      trtk=pp
    }else if (!is.data.frame(trtk)){
      trtk <- as.data.frame(trtk)
      n <- nrow(trtk)
      trtk <- trtk[rep(1:n,each=N),] 
      trtk$id <- rep(uN,n)
    }
    names(trtk)[names(trtk)=="amt"] <- "amount"
    names(trtk)[names(trtk)=="adm"] <- "type"
    if (any("tinf" %in% names(trtk))){
      names(trtk)[names(trtk)=="tinf"] <- "rate"
      trtk$rate <- trtk$amount/trtk$rate
    }
    if (!any("rate" %in% names(trtk)))
      trtk$rate <- Inf
    treatment[[k]] <- trtk   
  }
  return(treatment)
}
