
convertmlx <- function(data, dataIn){
  
  g <- dataIn$group
  iop.gout <- 0
  for(k in seq(1,length(g))){
    g[[k]]$output=NULL
    if (prod(g[[k]]$size) > 1)
      iop.gout <- 1
  }
  
  cv <- dataIn$catvar
  var <- dataIn$variability
  if (length(unique(var$id))==1)
    var$id <- NULL
  
  iop.group <- 0
  if (length(g)>1){
    gr=numeric(0)
    for(k in seq(1,length(g))){
      pgk <- prod(g[[k]]$size)
      gr=c(gr,rep(k,pgk))
#       if(max(pgk)>1)
        iop.group=1
    }
  }  
  
  gr=numeric(0)
  for(k in seq(1,length(g))){
    gr=c(gr,rep(k,prod(g[[k]]$size)))
  } 
  
  dd=list()
  if (sum(gr)==1){
    df <- data.frame()
  }else{
    df <- NULL
  }
  for(k in seq(1,length(data))){
    ak=data[[k]]
    nk =length(ak$value)
    vk=numeric(0)
    idk=numeric(0)
    tk=numeric(0)
    gk=numeric(0)
    for(i in seq(1,nk)){
      vki = ak$value[[i]]
      vk=c(vk, vki)
      nki=length(vki)
      idk=c(idk, rep(i,nki))
      if(iop.group==1)
        gk=c(gk, rep(gr[i],nki))
      
      tki = ak$time[[i]]
      tk=c(tk, tki)
    }
    ick <- which(ak$name==cv$name)
    if (length(ick)>0){
      vk <- cv$categories[[ick]][vk]
    }else{
      if (isfield(ak,"categories")){
        vk <- ak$categories[vk]
      }
    }
    if(length(tk)>0){
      iop.tk=1
    }else{
      iop.tk=0
    }
    if(length(unique(idk))>1){
      iop.id=1
    }else{
      iop.id=0
    }
    if(iop.id==0){
      if(iop.tk==1){
        dk=data.frame(time=tk, value=vk)
      }else{
        dk=data.frame(value=vk)
      }
    }else{
      if(iop.group==0){
        if(iop.tk==1){
          dk=data.frame(id=factor(idk), time=tk, value=vk)
        }else{
          dk=data.frame(id=factor(idk), value=vk)
        }
      }else{
        if(iop.tk==1){
          dk=data.frame(id=factor(idk), group=factor(gk), time=tk, value=vk)
        }else{
          dk=data.frame(id=factor(idk), group=factor(gk), value=vk)
        }
      }
    }
    names(dk)[names(dk)=="value"] <- ak$name
    if (iop.tk==0){
      if(iop.id==0){
        df <- c(df,dk)
      }else{
        if (is.null(df)){
          df <- dk
        }else{
          df <- cbind(df,dk)
          
          j1 <- which(names(df)=="id")
          if (length(j1>1))
            j1 <- j1[-1]
          
          j2 <- which(names(df)=="group")
          if (length(j2>1))
            j2 <- j2[-1]
          df <- df[-c(j1,j2)]
          
        }
      }
    }
    if(iop.id==0)
      df <- data.frame(df)
    
    attr(dk,"name")=ak$name
    dd[[ak$name]] = dk
  }
  
  #   if (length(df)>0){
  #     if (length(df[[1]])>1)
  #       dd$parameter = df
  #   }
  if (length(df)>1)
    dd$parameter = df
  
  if (!is.null(var)){
    v <- data.frame(var$value)
    names(v) <- var$colNames
    if (iop.group==1){
      names(v)[names(v)=="id"] <- "group"
      vval <- var$value
      jid <- which(var$colNames=="id")
      mval <- NULL
      for (k in (1:length(g))){
        ik <- which(vval[,jid]==k)
        mk <- matrix( rep( t( vval[ik,] ) , g[[k]]$size ) , 
                      ncol = ncol(vval) , byrow = TRUE )
        mval <- rbind(mval, mk)
      }
      vv <- data.frame(mval)
      names(vv) <- names(v)
      vv$group <- NULL
    }else{
      vv <- v
      vv$id <- NULL
    }
    for(k in seq(1,length(dd))){
      if (!isfield(dd[[k]],"time")){
        vdk <- cbind(vv, dd[[k]])
        j=which(names(vdk)=="group")
        if (length(j)>0){
          u=(1:dim(vdk)[2])
          vdk <- vdk[,c(j,u[-j])]
        }
        j=which(names(vdk)=="id")
        if (length(j)>0){
          u=(1:dim(vdk)[2])
          vdk <- vdk[,c(j,u[-j])]
        }
        dd[[k]] <- vdk
      }
    }
    dd$varlevel <- v
    
  }
  if (iop.gout==1)
    dd$group=g
  
  return(dd)
}