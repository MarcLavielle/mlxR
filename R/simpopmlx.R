#' Population parameters simulation
#' 
#' Draw population parameters using the covariance matrix of the estimates.
#' 
#' See https://simulx.lixoft.com/mlxr-documentation/ for more details.
#' @param n the number of vectors of population parameters (default = 1), 
#' @param project a Monolix project, assuming that the Fisher information Matrix was estimated by Monolix.
#' @param fim the Fisher Information Matrix estimated by Monolix. fim={"sa", "lin"} (default="sa") 
#' @param parameter  a data frame with a column \samp{pop.param} (no default), a column \samp{sd} (no default), 
#' possibly a column \samp{trans} (default ='N') and possibly columns \samp{lim.a} (default=0) and
#' \samp{lim.b} (default=1). Only when project is not used. 
#' @param corr correlation matrix of the population parameters (default = identity). Only when project is not used.
#' @param kw.max maximum number of trials for generating a positive definite covariance matrix (default = 100) 
#' @examples
#' \dontrun{
#' project.file <- 'monolixRuns/theophylline1_project.mlxtran'  #relative path
#' pop1 <- simpopmlx(n=3, project=project.file)
#' }
#' @importFrom stats dnorm qnorm pnorm rnorm
#' @export
simpopmlx <- function(n=1,project=NULL,fim="needed",parameter=NULL,corr=NULL,kw.max=100) {
  
  # !! RETRO-COMPTATIBILITY ========================================================== !!
  if (.useLixoftConnectors() && !.checkLixoftConnectorsAvailibility())
    return()
  # !! =============================================================================== !!  
  
  mu <- parameter$pop.param
  sd <- parameter$sd
  trans <- parameter$trans
  if (is.null(parameter$lim.a))  {
    lim.a <- 0
  } else {
    if (!is.null(parameter$trans))
    lim.a <- parameter$lim.a[trans=="G"]
  }
  if (is.null(parameter$lim.b))   {
    lim.b <- 1
  } else {
    if (!is.null(parameter$trans))
      lim.b <- parameter$lim.b[trans=="G"]
  }
  
  if (!is.null(project)){
    ans <- processing_monolix(project=project, fim=fim, create.model=FALSE,error.iov=FALSE)
    if (is.null(mu))
    mu <- ans$param[[1]]
    fim   <- ans$fim
    if (is.null(sd)) {
      if (is.null(fim))
        stop("The standard errors have not been estimated.", call.=FALSE)
      sd=fim$se
    }
    if (is.null(corr))
      corr=fim$mat
    np <- length(mu)    
    infoParam <- ans$infoParam
    pname <- names(mu)
    trans=rep("N",np)
    p2.name <- sub("_pop","",pname)
    i.pop <- match(infoParam$name,p2.name)
    i1 <- which(!is.na(i.pop))
    trans[i.pop[i1]] <- infoParam$trans[i1]
    if (is.null(infoParam$limits)) {
      lim.a <- 0
      lim.b <- 1
    } else {
      lim.a <- unlist(lapply(infoParam$limits, function(x) return(x[1])))
      lim.b <- unlist(lapply(infoParam$limits, function(x) return(x[2])))
    }
    
  } else { 
    pname <- row.names(parameter)
    np <- length(mu)    
  } 
  
  i.omega <- c(grep("^omega_",pname),grep("^omega2_",pname))
  i.corr <- unique(c(grep("^r_",pname),grep("^corr_",pname)))
  if (!is.null(project) | is.null(trans)){
    if (is.null(trans))
      trans=rep("N",np)
    trans[i.omega] <- "L"
    trans[i.corr] <- "R"
  }    
  
  if (is.null(corr)){
    d <- rep(1,np)
    corr <- diag(d)
    isd0 <- which(sd==0)
    if (length(isd0)>0){
      corr[isd0,] <- NaN
      corr[,isd0] <- NaN
    }
  }

  #
  inan <- which(is.nan(as.matrix(corr)),arr.ind=TRUE)
  if (dim(inan)[1]>0)
    i1 <- which(as.vector(table(inan[,1]))<np)
  else
    i1 <- (1:np)
  
  if (!is.null(corr))
    corr1 <- corr[i1,i1]
  else
    corr1 <- diag(rep(1,length(i1)))
  
  if (length(which(is.na(corr1)))>0)
    stop("ERROR:  the correlation matrix of the estimates contains NaN")
  se1 <- sd[i1]
  tr1 <- trans[i1]
  mu1 <- mu[i1]
  #
  set <- se1
  mut <- mu1
  iL <- which(tr1=="L")
 # set[iL] <- se1[iL]/mu1[iL]
 # mut[iL] <- log(mu1[iL])
  set[iL] <- sqrt(log(1+(se1[iL]/mu1[iL])^2))
  mut[iL] <- log(mu1[iL]) - (set[iL]^2)/2
  iG <- which(tr1=="G")
  set[iG] <- se1[iG]*(lim.b-lim.a)/((mu1[iG]-lim.a)*(lim.b-mu1[iG]))
  mut[iG] <- log((mu1[iG]-lim.a)/(lim.b-mu1[iG]))
  iR <- which(tr1=="R")
  set[iR] <- se1[iR]*2/(1 - mu1[iR]^2)
  mut[iR] <- log((mu1[iR]+1)/(1-mu1[iR]))
  iP <- which(tr1=="P")
  set[iP] <- se1[iP]/dnorm(qnorm(mu1[iP]))
  mut[iP] <- qnorm(mu1[iP])
  Rt <- chol((set%*%t(set))*corr1)
  #Rt <- chol(corr1)*set
  K <- length(i1)
  
  n.corr <- length(i.corr)
  if (n.corr>0){
    corr.name <- pname[i.corr]
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
    st[,iG] <- (lim.a + lim.b*exp(st[,iG]))/(1+exp(st[,iG]))
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
  s.res <- as.data.frame(s.res)
  names(s.res) <- pname
  
  if (nrow(s.res)>1)
    s.res <- cbind(list(pop=(1:nrow(s.res))),s.res)
  return(s.res)
}
