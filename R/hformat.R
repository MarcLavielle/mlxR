hformat  <-  function(list.input)
{
  #hformat 
  treatment=list.input$treatment
  parameter=list.input$parameter
  output=list.input$output
  regressor=list.input$regressor
  varlevel=list.input$varlevel
  group=list.input$group
  
  nv=c('treatment','parameter','output','regressor','varlevel')
  lv=list(treatment,parameter,output,regressor,varlevel)
  
  #------------------------------------
  if (!is.null(group)){
    if (!is.null(names(group))){  
      group <- list(group) 
    }
    N=length(group)
    
    model.info <- parse.model(list.input$model)
    for (k in (1:N)){
      if (!isfield(group[[k]],'size')){
        group[[k]]$size <- rep(1, length(model.info$level))
        group[[k]]$level <- model.info$level
      }else{
        if (!isfield(group[[k]],'level')){
          group[[k]]$level <- model.info$level
          if (length(group[[k]]$size)!=length(group[[k]]$level))
            stop("'level' and 'size' defined in 'group' have different lengths.", call.=FALSE)      
          if (length(model.info$level)>1){
            msg <- cat("levels of randomization have not been defined in group",k,
                       '\n levels associated with size = ',group[[k]]$size,' are:\n',
                       model.info$level,'\n\n')
            w1 <- warning(msg,call.=FALSE)
          }
        }else{
          if (isfield(group[[k]],'size') & (length(group[[k]]$size)!=length(group[[k]]$level)))
            stop("'level' and 'size' defined in 'group' have different lengths.", call.=FALSE)      
          gk <- group[[k]]$level
          sk <- rep(1, length(model.info$level))
          for (jk in (1:length(gk))){
            rk <- grep(gk[jk],model.info$level)
            if (length(rk)>0)
              sk[rk] <- group[[k]]$size[jk]
          }
          group[[k]]$size <- sk
          group[[k]]$level <- model.info$level
        }
      }
    }
  }else{
    N=NULL
  }
  
  for (k in seq(1,length(lv))) {
    lvk <- lv[[k]]
    if (!is.null(names(lvk))){  
      lvk <- list(lvk) 
      lv[[k]] <- lvk
    }
    for (j in seq(1,length(lvk))) {
      lvkj <- lvk[[j]]
      dkj <- dim(lvkj)
      #     if (!is.null(dkj)){
      #   N=c(N,dkj[1])
      if (isfield(lvkj,"id")){
        N <- c(N,length(unique(lvkj$id)))
      }else if(isfield(lvkj,"design")){
        N <- c(N,length(unique(lvkj$design$id)))
      }else if(isfield(lvkj,"time")){
        if (isfield(lvkj$time,"id")){
          N <- c(N,length(unique(lvkj$time$id)))
          names(lvkj)[names(lvkj)=="time"]<-"design"
          lv[[k]][[j]]=lvkj
        }
      }else if(isfield(lvkj,'header')){
        warning("deprecated syntax:  use 'colNames' instead of 'header'",immediate.=TRUE)
        names(lvkj)[names(lvkj)=="header"]<-"colNames"
        lv[[k]][[j]]=lvkj
      }
      
      if (isfield(lvkj,'colNames')){
        N=c(N,length(unique(lvkj$value[,1]))) #assuming that first column = id
      }
    }
  }    
  
  if (is.null(N)){
    N=1
  }else{
    N=unique(N)
    if (length(N)>1){
      stop("different numbers of individuals and/or groups are defined \n",call.="FALSE")
    }
  }
  
  if (is.null(group)){
    group <- vector("list",N)
    for (i in seq(1,N))
      group[[i]] <- list(size=1)
  }
  
  #---  parameters
  iv <- which(nv=="parameter")
  parameter <- vector("list", N)
  if (!is.null(lv[[iv]])){
    parameter <- format.parameter(parameter,lv[[iv]],seq(1,N))
  }
  for (i in seq(1,N)){
    gi <- group[[i]]
    if (isfield(gi,'parameter')) {
      parameter <- format.parameter(parameter,gi$parameter,i)
      group[[i]]$parameter <- NULL
    }
  }
  parameter <- merge.parameter(parameter)
  list.output=list(parameter=parameter)  
  
  #---  outputs
  output <- list(individual=vector("list", N),group=vector("list", N))
  for (i in seq(1,N)){
    output$individual[[i]]=list(name=NULL,time=NULL)
    output$group[[i]]=list(name=NULL)
  }
  iv <- which(nv=="output")
  output <- format.output(output,lv[[iv]],seq(1,N))
  for (i in seq(1,N)){
    gi <- group[[i]]
    if (isfield(gi,'output')) {
      output <- format.output(output,gi$output,i)
    }
    group[[i]]$output <- output$group[[i]]$name
  }
  output <- output$individual
  list.output$output <- output
  
  #---  treatments
  iv <- which(nv=="treatment")
  if (!is.null(lv[[iv]])){
    treatment <- format.treatment(lv[[iv]],seq(1,N))
  }
  for (i in seq(1,N)){
    gi <- group[[i]]
    if (isfield(gi,'treatment')) {
      pgi <- format.treatment(gi$treatment,i)
      treatment <- c(treatment, pgi)
      group[[i]]$treatment <- NULL
    }
  }
  if (!is.null(treatment)){
    r <- merge.treatment(treatment,N)
    list.output <- c(list.output, r)
  }
  
  #---  regressor
  iv <- which(nv=="regressor")
  if (!is.null(lv[[iv]])){
    regressor <- format.regressor(lv[[iv]],seq(1,N))
  }
  for (i in seq(1,N)){
    gi <- group[[i]]
    if (isfield(gi,'regressor')) {
      pgi <- format.regressor(gi$regressor,i)
      regressor <- c(regressor, pgi)
      group[[i]]$regressor <- NULL
    }
  }
  if (!is.null(regressor))
    list.output$regressor=merge.regressor(regressor,N)
  
  #---  varlevel
  iv <- which(nv=="varlevel")
  if (!is.null(lv[[iv]])){
    varlevel <- format.varlevel(lv[[iv]],seq(1,N))
  }
  for (i in seq(1,N)){
    gi <- group[[i]]
    if (isfield(gi,'varlevel')) {
      pgi <- format.varlevel(gi$varlevel,i)
      varlevel <- c(varlevel, pgi)
      group[[i]]$varlevel <- NULL
    }
  }
  if (!is.null(varlevel))
    list.output$varlevel <- merge.varlevel(varlevel,N)
  
  
  #--------------------
  list.output$group=group
  return(list.output)
}

#-----------------------------
format.treatment <- function(treatment,uN) 
{
  N <- length(uN)
  if (!is.null(names(treatment))){  
    treatment <- list(treatment) 
  }
  
  for (k in seq(1,length(treatment))){
    trtk <- treatment[[k]]
    if (isfield(trtk,'colNames')){
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
    if (isfield(trtk,"tinf")){
      names(trtk)[names(trtk)=="tinf"] <- "rate"
      trtk$rate <- trtk$amount/trtk$rate
    }
    if (!isfield(trtk,"rate"))
      trtk$rate <- Inf
    treatment[[k]] <- trtk   
  }
  return(treatment)
}
#--------------------------
merge.treatment <- function(treatment,N)  
{
  p <- vector("list",N)
  ptr <- treatment[[1]]
  np <- length(treatment)
  if (np>1){
    for (j in seq(2,np)){
      ptr=rbind(ptr,treatment[[j]])
    }
  }
  
  if (isfield(ptr,"target")){
    list.target <- levels(ptr$target)
    nt <- length(list.target)
    depot <- vector("list", nt)
    for (k in seq(1,nt)){
      depot[[k]] <- list(type=k, target=list.target[k])
    }
    ptr$type    <- as.numeric(ptr$target)
    ptr$target  <- NULL
  }else{
    depot<-NULL
  }
  
  for (i in seq(1,N)){
    pi <- NULL
    ij <- which(ptr$id==i)
    if (length(ij)>0) {
      pi <- ptr[ij,]
    }
    pi$id=NULL
    
    if (!isfield(pi,"type"))
      pi$type <- 1
    
    if (!is.null(pi$time)){
      pi <- pi[with(pi,order(time)),]
      p[[i]] <- as.list(pi)
    }
  }
  r <- list(treatment=p, depot=depot)
  return(r)
}

#-----------------------------
format.parameter <- function(parameter,param,uN) 
{
  N <- length(uN)
  if (!is.null(names(param))){  
    param=list(param) 
  }
  
  for (k in seq(1,length(param))){
    paramk <- param[[k]]
    if (isfield(paramk,'colNames')){
      pp=as.data.frame(paramk$value)
      names(pp)=paramk$colNames
      paramk=pp
    }
    if (!is.data.frame(paramk)){
      if (!is.list(paramk)){
        paramk <- list(name=names(paramk),value=as.vector(paramk))
      }
      pk <- rep(paramk$value,N)
      pk <- t(matrix(pk,ncol=N))
      pk <- cbind(uN,pk)
      pk <- data.frame(pk)
      names(pk) <- c('id',paramk$name)
      paramk <- pk
    }else{
      if (!isfield(paramk,"id")){
        paramk <- paramk[rep(1,each=N),] 
        paramk$id <- uN
      }
    }
    for (i in uN){
      pki <- paramk[paramk$id==i,]
      if (is.null(parameter[[i]])){
        parameter[[i]] <- pki
      }else{
        parameter[[i]] <- merge(parameter[[i]],pki)
      }
    }
  }
  return(parameter)
}
#-----------------------------
merge.parameter <- function(parameter) 
{
  if (!is.null(parameter[[1]])){
    N <- length(parameter)
    pp <- parameter[[1]]
    if (N>1){
      for (i in seq(2,N)){
        pp <- rbind(pp, parameter[[i]])
      }
    }
    parameter <- pp[with(pp,order(id)),]
    #    parameter$id <- NULL  
  }else{
    parameter=NULL
  }
  return(parameter)
}

#-----------------------------
format.output <- function(output, out,uN) 
{
  N <- length(uN)
  if (!is.null(names(out))){  
    out=list(out) 
  }
  
  ioutput <- output$individual
  goutput <- output$group
  
  for (k in seq(1,length(out))){
    outk <- out[[k]]
    if (!isfield(outk,"name"))
      outk <- list(name=outk)
    okname <- unlist(outk$name)
    nok <- length(okname)
    if (isfield(outk,'colNames')){
      pp=as.data.frame(outk$value)
      names(pp)=outk$colNames
      outk$design=pp
    }
    if (isfield(outk,'design')){      
      id <- unique(outk$design$id)
      for (i in id){
        ji <- which(outk$design$id == i)
        ti <- sort(outk$design$time[ji])
        oitime <- vector("list" , nok)
        for (j in seq(1,nok)){
          oitime[[j]] <- ti
        }
        goutput[[i]]$name=c(goutput[[i]]$name,okname)
        ioutput[[i]]$name=c(ioutput[[i]]$name,okname)
        ioutput[[i]]$time=c(ioutput[[i]]$time,oitime)
      }
      
    }else{ 
      for (i in uN){
        if (isfield(outk,"time")){
          oitime <- vector("list" , nok)
          for (j in seq(1,nok)){
            oitime[[j]] <- sort(outk$time)
          }
          ioutput[[i]]$time=c(ioutput[[i]]$time,oitime)
          ioutput[[i]]$name=c(ioutput[[i]]$name,okname)
        }
        goutput[[i]]$name=c(goutput[[i]]$name,okname)
      }
    }
  }
  output$individual <- ioutput 
  output$group <- goutput
  return(output)
}

#-----------------------------
format.regressor <- function(reg, uN) 
{
  N <- length(uN)
  if (!is.null(names(reg))) 
    reg=list(reg) 
  
  regressor <- vector("list",length(reg))
  
  for (k in seq(1,length(reg))){
    regk <- reg[[k]]
    if (!is.data.frame(regk)){
      if (isfield(regk,'colNames')){
        mk <- regk$value
        colNames=regk$colNames
      }else{
        nk <- length(regk$time)
        idk <- rep(uN,each=nk)
        mk <- cbind(regk$time,regk$value)
        mk <- mk[rep(1:nk,N),]
        mk <- cbind(idk, mk)
        colNames <- c("id","time",regk$name)
      }
      regk <- data.frame(mk)
      names(regk) <- colNames
    }else{
      colNames <- names(regk)
      #      mk <- data.matrix(regk)
      mk <- matrix(as.numeric(unlist(regk)),nrow=nrow(regk))
    } 
    regressor[[k]] <- regk
  }
  return(regressor)
}

#-----------------------------
merge.regressor <- function(reg, N) 
{
  if (length(reg)>1){
    r <- NULL
    for (i in seq(1,N)){
      ri <- NULL
      for (k in seq(1,length(reg))){
        rik <- reg[[k]][reg[[k]]$id==i,]
        if (nrow(rik)>0)
          if (is.null(ri)){
            ri <- rik
          }else{
            ri <- merge(ri,rik,
                        by.x=c("id","time"), by.y=c("id","time"),
                        all.x=TRUE,all.y=TRUE) 
            ri <- ri[order(ri$time),]
          }
      }
      r <- rbind(r,ri)
    }
  }else{
    r <- reg[[1]]
  }
  
  #  m <- data.matrix(r)
  m <- matrix(as.numeric(unlist(r)),nrow=nrow(r))
  colNames <- names(r)
  colTypes <- rep("x",length(colNames))
  colTypes[colNames=="id"] <- "id"
  colTypes[colNames=="time"] <- "time"
  regressor <- list(colNames=colNames, colTypes=colTypes, value=m)
  return(regressor)
}

#-----------------------------
format.varlevel <- function(var, uN) 
{
  N <- length(uN)
  if (!is.null(names(var))) 
    var=list(var) 
  
  varlevel <- vector("list",length(var))
  
  for (k in seq(1,length(var))){
    vark <- var[[k]]
    if (!is.data.frame(vark)){
      if (isfield(vark,'colNames')){
        mk <- vark$value
        colNames=vark$colNames
      }else{
        nk <- length(vark$time)
        idk <- rep(uN,each=nk)
        occk <- rep(1:nk,N)
        mk <- vark$time[occk]
        mk <- cbind(idk, mk, occk)
        colNames <- c("id","time",vark$name)
      }
      vark <- data.frame(mk)
      names(vark) <- colNames
    }
    #     else{
    #       colNames <- names(vark)
    #       mk <- matrix(as.numeric(vark),nrow=nrow(vark))
    #      mk <- data.matrix(vark)
    #    } 
    varlevel[[k]] <- vark
  }
  return(varlevel)
}

#-----------------------------
merge.varlevel <- function(var, N) 
{
  if (length(var)>1){
    r <- NULL
    for (i in seq(1,N)){
      ri <- NULL
      for (k in seq(1,length(var))){
        rik <- var[[k]][var[[k]]$id==i,]
        if (nrow(rik)>0)
          if (is.null(ri)){
            ri <- rik
          }else{
            ri <- merge(ri,rik,
                        by.x=c("id","time"), by.y=c("id","time"),
                        all.x=TRUE,all.y=TRUE) 
            for (j in seq(1,ncol(ri))){
              ij <- c(which(!is.na(ri[,j])),nrow(ri)+1)
              
              for (l in seq(1,length(ij)-1)){
                ijl <- ij[l]
                if (ij[l+1]-ijl>1)
                  ri[seq(ijl+1,ij[l+1]-1),j] <- ri[ijl,j]
              }
            }
          }
      }
      r <- rbind(r,ri)
    }
  }else{
    r <- var[[1]]
  }
  
  m <- matrix(as.numeric(unlist(r)),nrow=nrow(r))
  #m <- data.matrix(r)
  colNames <- names(r)
  colTypes <- rep("OCC",length(colNames))
  colTypes[colNames=="id"] <- "id"
  colTypes[colNames=="time"] <- "time"
  varlevel <- list(colNames=colNames, colTypes=colTypes, value=m)
  return(varlevel)
}

#-----------------------------
parse.model <- function(model) 
{
  con        = file(model, open = "r")
  lines      = readLines(con, warn=FALSE)
  close(con)
  ip <- grep(";",lines, fixed=TRUE)
  if (length(ip)>0){
    for (k in (1:length(ip))){
      ll <- lines[ip[k]]
      il <- regexpr(";",ll)
      il <- il[1]
      if (il>1){
        lines[ip[k]] <- substr(ll,start=1,stop=(il-1))
      }else{
        lines[ip[k]] <- ''
      }
    }
  }
  
  i1 <- grep("[POPULATION]",lines, fixed=TRUE)
  i2 <- grep("[COVARIATE]",lines, fixed=TRUE)
  i3 <- grep("[INDIVIDUAL]",lines, fixed=TRUE)
  i4 <- grep("[LONGITUDINAL]",lines, fixed=TRUE)
  
  level <- NULL
  if (length(i1)>0)
    level <- c(level,'population')
  if (length(i2)>0)
    level <- c(level,'covariate')
  if (length(i3)>0)
    level <- c(level,'individual')
  if (length(i4)>0)
    level <- c(level,'longitudinal')
  
  fs <- fsort(c(i1, i2, i3, i4))
  model.info <- list(level=level[fs[[2]]])  
  return(model.info)
  
}  