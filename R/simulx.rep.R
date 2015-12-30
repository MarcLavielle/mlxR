#' Convert a Monolix Project  into an executable for the simulator  Simulx 
#' @param project : the name of a Monolix project 
#' @param parameter : string $(NameOfTypeOfParameter), the type of specific parameters to use 
#'                   example: "mode", "mean"...
#' @param group : a list with the number of subjects 
#' @param open : load the R script created if \code{open=TRUE}
#' @param r.data : read the data if \code{r.data=TRUE}
#' @param add : add tables in the output folder
#' @return  creates a folder projectNameR  containing files : 
#' \itemize{
#'   \item \code{projectName.R} :  executable R code for the simulator,
#'   \item \code{treatment.txt} :  contains the treatment informations,
#'   \item \code{populationParameter.txt} : contains the  population parameters estimated from Monolix,
#'   \item \code{individualParameter.txt} : contains the  individual parameters (mode/mean) estimated from Monolix (if used for the simulation),
#'   \item \code{individualCovariate.txt} : contains the individual covariates,
#'   \item \code{originalId.txt} : contains the original id's when group is used with a different size than the original one,
#'   \item \code{outputi.txt} : contains the output number i informations (time, id),
#'   \item \code{$(NameOfTypeOfParameter)s.txt} : contains the specific parameter used.
#' }       
#'  
#' @return A list of data frames. Each data frame is an output of simulx the mlxtran model
#' the data inputs: treatment, parameters, output of monolix, group... 
#' 
#' @export

simulx.rep <-function(project=NULL,fim=NULL,npop=1,nrep=1,kw.max=100,sep=" ",write.simul=FALSE,folder=NULL,
                      stat=NULL, probs=c(0.1, 0.5, 0.9), ci.prob=0.9, 
                      model=NULL, parameter=NULL,treatment=NULL,regressor=NULL, 
                      varlevel=NULL,group=NULL,output=NULL,r.data=TRUE,disp.iter=FALSE,l.max=25000)
{ 
  #------- project to be converted into Simulx project
  myOldENVPATH = Sys.getenv('PATH');
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  if  (!is.null(names(group)))
    group <- list(group)
  nout0 <- NULL
  for (k in (1:length(output))){
    outputk <- output[[k]]
    if (!is.null(outputk$time) && length(outputk$time)==0){
      output[[k]]$time=0
      nout0 <- c(nout0,output[[k]]$name)
    }
  }
  
  if (!(is.null(project)))  {
    if (npop>1 && is.null(fim))
      fim <- "needed"
    ans <- processing_monolix(project=project,
                              model=NULL,
                              treatment=treatment,
                              parameter=parameter,
                              output=output,
                              group=group,
                              r.data=r.data,
                              fim=fim)
    model         <- ans$model
    treatment     <- ans$treatment
    parameter     <- ans$param
    output        <- ans$output
    group         <- ans$group
    regressor     <- ans$regressor
    fim           <- ans$fim
    infoParam     <- ans$infoParam
    N <- length(ans$id[,1])
    
    populationParameter <- parameter[[1]]
    individualCovariate <- parameter[[2]]
    individualParameter <- parameter[[3]]  
    ########
    param0 <- NULL
    if (!is.null(individualCovariate))
      param0 <- append(param0, list(individualCovariate))
    
    if (!is.null(individualParameter))
      param0 <- append(param0, list(individualParameter))
    
    if (!is.null(fim)){
      pop.mat <- sim.pop(npop,populationParameter,infoParam,fim,kw.max)
      param.list <- append(param0,list(pop.mat[1,]))  
    }else{
      param.list <- append(param0,list(populationParameter))  
    }
    
  } else {
    if (npop>1 & is.null(fim))
      stop('The covariance matrix of the population parameters is requested for simulating several replicates 
           of the population')
    param.list <- parameter
    if (is.null(group)){
      N <- 1
      for (k in (1:length(param.list))){
        if (is.list(param.list[[k]]) && !is.null(param.list[[k]]$id))
          N <- length(param.list[[k]]$id)
      }
    } else
      N <- group$size
  }
  
  if (npop>1 & is.null(fim))
    stop('The covariance matrix of the population parameters is requested for simulating several replicates 
           of the population')
  
  if (write.simul==TRUE){
    if (is.null(folder)){
      mlxtranpath <- dirname(project)
      mlxtranfile = file_path_sans_ext(basename(project))
      mypath <- getwd()
      Rproject <- file.path(mypath,paste0(mlxtranfile,"_simrep"))
    }else{
      Rproject <- folder
    }
    #     if (file.exists(Rproject) )
    unlink(Rproject, recursive = TRUE, force = TRUE)
    dir.create(Rproject, showWarnings = FALSE, recursive = FALSE, mode = "0777")
    #     dir.create(Rproject, showWarnings = FALSE, recursive = FALSE, mode = "0777")
  }
  ########
  #   plevel <- c((1-ci.prob)/2, (1+ci.prob)/2)
  
  #   g <- group
  #   if (!is.null(g))
  #     g$size <- 1
  r0 <- simulx(model=model,
               treatment=treatment, 
               output=output, 
               parameter=param.list, 
               group=list(size=1,level="individual"))
  if (!is.null(r0$parameter))
    names(r0)[which(names(r0)=="parameter")] <- "individual"
  nr <- length(r0)
  r.names <- names(r0)
  if (!is.null(r0$individual)){
    p.names <- names(r0$individual)
    h.names <- setdiff(r.names,p.names)
    r.ind <- which(is.element(r.names,h.names))
  } else {
    r.ind <- (1:length(r.names))
    h.names <- r.names
  }
  t.names <- setdiff(h.names,nout0)
  t.ind <- r.ind[which(is.element(h.names,t.names))]
  
  s.ind <- which(r.names!="parameter" & r.names!="individual")
  s.names <- r.names[s.ind]
  
  ns <- length(s.ind)
  nt <- length(t.ind)
  
  if (write.simul==TRUE){
    f.names <- t.names
    for (k in (1:nt))
      f.names[k] <- file.path(Rproject,paste0(t.names[k],".txt"))
    R.complete <- NULL
  }else{
    R.complete <- list()
  } 
  dataIn <- simulx(model=model,
                   treatment=treatment, 
                   output=output, 
                   parameter=param.list, 
                   group=group,
                   settings=list(data.in=TRUE,load.design=TRUE))
  
  if (nrep>1 & write.simul==TRUE){
    lt <- 0
    for (k in (1:length(r0)))
      lt <- lt + length(r0[[k]]$time)
    ltot <- lt*N*nrep
    nsub <- ceiling(ltot/l.max)
    nrep.sub  <- floor(nrep/nsub)
    nsub <- ceiling(nrep/nrep.sub)
    nrep.subf <- nrep-nrep.sub*nsub+nrep.sub
    #     print(c(nsub,nrep.sub,nrep.subf, nrep, ((nsub-1)*nrep.sub+nrep.subf)))
  }else{
    nsub <- 1
    nrep.sub  <- nrep
    nrep.subf <- nrep
  }
  
  R.complete <- list()
  if (disp.iter==TRUE && nsub>1)
    cat("\nNumber of iterations : ", nsub,"\n")
  s <- NULL
  for (i.pop in (1:npop)) {
    if (disp.iter==TRUE & npop>1){
      if (nrep>1)
        cat("\npopulation: ",i.pop,"\n")
      else
        cat("population: ",i.pop,"\n")
    }
    if (!is.null(fim))
      param.list <- append(param0,list(pop.mat[i.pop,]))  
    dataIn <- simulx(model=model,
                     treatment=treatment, 
                     output=output, 
                     parameter=param.list, 
                     group=group,
                     settings=list(data.in=TRUE,load.design=FALSE))
    nrep.subi <- nrep.sub
    irw <- 0
    for (i.sub in (1:nsub)) {
      res <- NULL
      if (disp.iter==TRUE && nsub>1)
        cat("iteration : ", i.sub,"\n")
      #       if (i.sub==3)
      #         browser()
      if (i.sub==nsub)
        nrep.subi <- nrep.subf
      for (i.rep in (1:nrep.subi)) {
        irw <- irw + 1
        if (disp.iter==TRUE && nrep>1)
          cat("replicate: ",irw,"\n")
        
        r <- simulx(data=dataIn)
        
        if (!is.null(stat)){
          if (npop>1) s.pop=i.pop else s.pop=NULL
          if (nrep>1) s.rep=irw else s.rep=NULL
          s <- statmlx(r[t.ind], s.pop, s.rep, s, stat, probs)
        }
        if (write.simul==TRUE || is.null(stat)){
          r <- r[t.ind]
          if (nrep>1){
            for (k in (1:nt))
              r[[k]] <- cbind(list(rep=irw), r[[k]])
          }
          if (i.rep==1) {
            res <- r
          } else {
            for(k in (1:nt))
              res[[k]] <- rbind(res[[k]],r[[k]])
          }
        }
      }
      if (write.simul==TRUE || is.null(stat)){
        ncol.ind <- c("pop","rep","id")
        if (!is.null(res$parameter))
          names(res)[which(names(res)=="parameter")] <- "individual"
        if (write.simul==FALSE){
          for(k in (1:nt)){
            Rk <- res[[k]]
            if (npop>1)
              Rk <- cbind(list(pop=i.pop),Rk)
            if (i.pop==1){
              R.complete[[k]] <- Rk
            }else{
              R.complete[[k]] <- rbind(R.complete[[k]],Rk)
            }
          }
        } else {
          for (k in (1:nt)){
            rk <- data.frame(lapply(res[[k]], function(y) if(is.numeric(y)) signif(y, 5) else y)) 
            if (npop>1)
              rk <- cbind(list(pop=i.pop),rk)
            if (nrep==1)
              rk$rep <- NULL
            if (is.null(fim) | npop==1)
              rk$pop <- NULL
            if (i.pop==1 & i.sub==1)
              write.table(rk,f.names[k],row.names=FALSE,quote=FALSE,sep=sep)
            else
              write.table(rk,f.names[k],append=TRUE,sep=sep,row.names=FALSE,col.names=FALSE,quote=FALSE)
          }
        }
      }
    }
  }
  ##
  if (!is.null(stat)){
    R.complete <- s
    names(R.complete) <- t.names
  }
  
  ##
  if (!is.null(fim)){
    pop <- as.data.frame(pop.mat)
    pop <- format(pop, digits = 5, justify = "left")
    pop <- cbind(pop=(1:npop),pop)
    if (write.simul==FALSE){
      if (is.null(stat))
        names(R.complete) <- t.names
    }else{
      fname <- file.path(Rproject,"population.txt")
      write.table(pop,fname,,row.names=FALSE,quote=FALSE,sep=sep)
    }
    R.complete$population <- pop
  }else if (write.simul==FALSE && is.null(stat)){
    names(R.complete) <- t.names
    for(k in (1:nt))
      R.complete[[k]]$pop <- NULL
  }
  
  Sys.setenv(LIXOFT_HOME="")
  Sys.setenv('PATH'=myOldENVPATH);
  return(R.complete)
}


################################################"

sim.pop <- function(n,mu,infop,fim,kw.max){
  
  p1.name <- names(fim$se)
  np <- length(p1.name)
  p1.trans=rep("N",np)
  p2.name <- sub("_pop","",p1.name)
  i.pop <- match(infop$name,p2.name)
  i1 <- which(!is.na(i.pop))
  p1.trans[i.pop[i1]] <- infop$trans[i1]
  i.omega <- grep("omega_",p1.name)
  p1.trans[i.omega] <- "L"
  i.omega2 <- grep("omega2_",p1.name)
  p1.trans[i.omega2] <- "L"
  i.corr <- c(grep("r_",p1.name),grep("corr_",p1.name))
  p1.trans[i.corr] <- "R"
  #
  inan <- which(is.nan(as.matrix(fim$mat)),arr.ind=TRUE)
  if (dim(inan)[1]>0)
    i1 <- which(as.vector(table(inan[,1]))<np)
  else
    i1 <- (1:np)
  corr1 <- fim$mat[i1,i1]
  if (length(which(is.na(corr1)))>0)
    stop("ERROR:  the correlation matrix of the estimates contains NaN")
  se1 <- fim$se[i1]
  tr1 <- p1.trans[i1]
  mu1 <- mu[i1]
  #
  set <- se1
  mut <- mu1
  iL <- which(tr1=="L")
  set[iL] <- se1[iL]/mu1[iL]
  mut[iL] <- log(mu1[iL])
  iG <- which(tr1=="G")
  set[iG] <- se1[iG]/(mu1[iG]*(1-mu1[iG]))
  mut[iG] <- log(mu1[iG]/(1-mu1[iG]))
  iR <- which(tr1=="R")
  set[iR] <- se1[iR]*2/(1 - mu1[iR]^2)
  mut[iR] <- log((mu1[iR]+1)/(1-mu1[iR]))
  iP <- which(tr1=="P")
  set[iP] <- se1[iP]/dnorm(qnorm(mu1[iP]))
  mut[iP] <- qnorm(mu1[iP])
  Rt <- chol(corr1)*set
  K <- length(i1)
  
  n.corr <- length(i.corr)
  if (n.corr>0){
    corr.name <- p1.name[i.corr]
    g=gregexpr("_",corr.name)
    nk1 <- vector(length=n.corr)
    nk2 <- vector(length=n.corr)
    for (k in (1:n.corr)){
      if (length(g[[k]])>2)
        stop('ERROR :  you should not use parameter names with "_" ')
      cnk <- corr.name[k]
      gk <- g[[k]][1:2]
      nk1[k] <- substr(cnk,gk[1]+1,gk[2]-1)
      nk2[k] <- substr(cnk,gk[2]+1,nchar(cnk))
    }
    nvar <- unique(c(nk1,nk2))
    ir1 <- match(nk1,nvar)
    ir2 <- match(nk2,nvar)
    ind1.r <- matrix(c(ir2,ir1),ncol=2)
    ind2.r <- matrix(c(ir1,ir2),ncol=2)
    n.r <- length(nvar)
    R.r <- diag(rep(1,n.r))
  }
  #
  n1 <- n
  s.res <- NULL
  kw <- 0
  while(n1 > 0){
    kw <- kw+1
    if (kw> kw.max)
      stop("Maximum number of iterations reached: could not draw definite positive correlation matrix")
    x=matrix(rnorm(K*n1),ncol=K)
    st <- t(t(x%*%Rt) + mut)
    st[,iL] <- exp(st[,iL])
    st[,iG] <- 1/(1+exp(-st[,iG]))
    st[,iP] <- pnorm(st[,iP])
    st[,iR] <- (exp(st[,iR])-1)/(exp(st[,iR])+1)
    s <- t(replicate(n1, mu))  
    s[,i1] <- st
    if (n.corr>0){
      for (i in (1:n1)){
        sri <- s[i,i.corr]
        R.r[ind1.r] <- sri
        R.r[ind2.r] <- sri
        R.eig <- eigen(R.r, symmetric=TRUE, only.values=TRUE)
        if (min(R.eig$values)>0){
          n1 <- n1-1
          s.res <- rbind(s.res,s[i,])
        }
      }
    } else {
      n1 <- 0
      s.res <- s
    }
    
  }
  return(s.res)
}

###
statmlx <- function(r, ipop, irep, s=list(), stat, probs)
{
  list.stat <- c("mean","sd","median","var") 
  l.stat <- length(stat)
  n.stat <- NULL
  for (j in (1:l.stat)){
    if (stat[j] %in% list.stat)
      n.stat <- c(n.stat,stat[j])
    else 
      n.stat <- c(n.stat,paste0("p",probs*100))
  }
  
  rs <- list()  
  for (j in (1:length(r))){
    rj <- r[[j]]  
    rj <- rj[,!colnames(rj) == "id"]
    srj <- NULL
    if (names(r)[j]=="parameter"){
      n.srj <- NULL
      for (k in (1:l.stat)){
        stat.k <- stat[k]       
        if (stat.k %in% list.stat)
          srj <- rbind(srj,sapply(rj,stat.k))
        else
          srj <- rbind(srj,sapply(rj,stat.k,probs=probs))
        #         srj <- as.data.frame(srj)
        if (stat.k %in% list.stat)
          n.srj <- rbind(n.srj,paste0(names(rj),".",stat.k))
        else{
          aa <- apply(expand.grid(names(rj),paste0("p",probs*100)), 1, paste, collapse=".")  
          n.srj <- rbind(n.srj,t(matrix(aa,nrow=ncol(rj))))
        }
      }
      srj <- as.vector(srj)
      names(srj) <- n.srj
      srj <- as.data.frame(t(srj))
    } else {
      iy <- which(!(names(rj) == "time"))
      ny0 <- names(rj)[iy]
      names(rj)[iy] <- "y"
      for (k in (1:l.stat)){
        stat.k <- stat[k]
        if (stat.k %in% list.stat)
          srjk <- aggregate(y~time,data=rj,stat.k)
        else 
          srjk <- aggregate(y~time,data=rj,stat.k,probs=probs)
        if (k==1)
          srj <- as.data.frame(cbind(srjk[[1]],srjk[[2]]))
        else
          srj <- cbind(srj,srjk[[2]])
      }
      names(srj) <- c("time",paste0(ny0,".",n.stat))
      srj <- as.data.frame(srj)
    }
    if (!is.null(irep))
            srj <- cbind(list(rep=irep) , srj)
    if (!is.null(ipop))
             srj <- cbind(list(pop=ipop) , srj)
    if (length(s)<j){
      sj <- srj
    } else {
      sj <- rbind(s[[j]],srj)
    }
    rownames(sj) <- NULL
    s[[j]] <- sj
  }
  return(s)
}

###
statmlx1 <- function(r, ipop, irep, npop, nrep, s=list(), stat, probs)
{
  list.stat <- c("mean","sd","median","var") 
  l.stat <- length(stat)
  n.stat <- NULL
  for (j in (1:l.stat)){
    if (stat[j] %in% list.stat)
      n.stat <- c(n.stat,stat[j])
    else 
      n.stat <- c(n.stat,paste0("p",probs*100))
  }
  
  r$parameter <- NULL
  rs <- list()  
  for (j in (1:length(r))){
    rj <- r[[j]]
    rj <- rj[,!colnames(rj) == "id"]
    srj <- NULL
    if (!is.data.frame(rj)){
      for (k in (1:l.stat)){
        stat.k <- stat[k]
        if (stat.k %in% list.stat)
          srj <- c(srj,eval(call(stat.k,rj)))
        else 
          srj <- c(srj,eval(call(stat.k,rj,probs=probs)))
      }
      srj <- data.frame(matrix(srj,nrow=1,dimnames=list(NULL,n.stat)))
    } else {
      iy <- which(!(names(rj) == "time"))
      ny0 <- names(rj)[iy]
      names(rj)[iy] <- "y"
      for (k in (1:l.stat)){
        stat.k <- stat[k]
        if (stat.k %in% list.stat)
          srjk <- aggregate(y~time,data=rj,stat.k)
        else 
          srjk <- aggregate(y~time,data=rj,stat.k,probs=probs)
        if (k==1)
          srj <- as.data.frame(cbind(srjk[[1]],srjk[[2]]))
        else
          srj <- cbind(srj,srjk[[2]])
      }
      names(srj) <- c("time",n.stat)
    }
    if (nrep>1)
      srj <- cbind(list(rep=irep) , srj)
    if (npop>1)
      srj <- cbind(list(pop=ipop) , srj)
    if (length(s)<j){
      sj <- srj
    } else {
      sj <- rbind(s[[j]],srj)
    }
    s[[j]] <- sj
  }
  return(s)
}
