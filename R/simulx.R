#' Simulation of mixed effects models and longitudinal data
#'
#' Compute predictions and sample data from \code{Mlxtran} and \code{PharmML} models
#' 
#' simulx takes advantage of the modularity of hierarchical models for simulating 
#' different components of a model: models for population parameters, individual 
#' covariates, individual parameters and longitudinal data.
#' 
#' Furthermore, \code{simulx} allows to draw different types of longitudinal data, 
#' including continuous, count, categorical, and time-to-event data.
#' 
#' The models are encoded using either the model coding language \samp{Mlxtran} or the 
#' markup language \samp{PharmML}. These models are automatically converted into C++ codes, 
#' compiled on the fly and linked to R using the \samp{Rcpp} package. 
#' That allows one to implement very easily complex models and to take advantage 
#' of the numerical sovers used by the C++ \samp{MlxLibrary}.
#' 
#' See http://simulx.webpopix.org for more details.      
#' @param model a \code{Mlxtran} or \code{PharmML} model used for the simulation
#' @param output a list (or list of lists) with fields: 
#' \itemize{
#'   \item \code{name}: a vector of output names
#'   \item \code{time}: a vector of times (only for the longitudinal outputs)
#' }
#' @param data a list
#' @param parameter a vector of parameters with their names and values
#' @param treatment a list with fields
#' \itemize{
#'   \item \code{time} : a vector of input times,
#'   \item \code{amount} : a scalar or a vector of amounts,
#'   \item \code{rate} : a scalar or a vector of infusion rates (default=\code{Inf}),
#'   \item \code{tinf} : a scalar or a vector of infusion times (default=0),
#'   \item \code{type} : the type of input (default=1),
#'   \item \code{target} : the target compartment (default=NULL). 
#' }
#' @param group a list, or a list of lists, with fields: 
#' \itemize{
#'   \item \code{size} : size of the group (default=1),
#'   \item \code{level} : level(s) of randomization,
#'   \item \code{parameter} : if different parameters per group are defined,
#'   \item \code{output} : if different outputs per group are defined,
#'   \item \code{treatment} : if different treatements per group are defined,
#'   \item \code{regressor} : if different regression variables per group are defined.
#' }
#' @param regressor a list, or a list of lists, with fields
#' \itemize{
#'   \item \code{name} : a vector of regressor names,
#'   \item \code{time} : a vector of times,
#'   \item \code{value} : a vector of values.
#' }
#' @param varlevel a list, or a list of lists, with fields
#' \itemize{
#'   \item \code{name} : a vector of names of variability levels,
#'   \item \code{time} : a vector of times that define the occasions.
#' }
#' @param project the name of a Monolix project
#' @param settings a list of optional settings
#' \itemize{
#'   \item \code{record.file} : name of the datafile where the simulated data is written (string),
#'   \item \code{seed} : initialization of the random number generator (integer),
#'   \item \code{load.design} : TRUE/FALSE (if load.design is not defined, a test is automatically performed to check if a new design has been defined),
#'   \item \code{data.in} : TRUE/FALSE (default=FALSE)
#'   \item \code{Nmax} : maximum group size used in a single call of mlxCompute (default=100)
#' }       
#' 
#' @return A list of data frames. Each data frame is an output of simulx
#' 
#' @export
simulx <- function(model=NULL,group=NULL,treatment=NULL,parameter=NULL,output=NULL,
                   data=NULL, project=NULL, settings=NULL, regressor=NULL, varlevel=NULL)
{ 
  #--------------------------------------------------
  #  simulx.R is governed by the CeCILL-B license. 
  #  You can  use, modify and/ or redistribute the software under the terms of 
  #  the CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
  #  http://www.cecill.info/index.en.html
  #
  #  simulx.R was developed by Marc Lavielle and the Inria popix team for the DDMoRe project. 
  #--------------------------------------------------
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  #   Nmax <- 10
  test.group <- FALSE
  if ((!is.null(group))  & (is.null(settings$data.in)) & (is.null(settings$record.file))){
    if  (!is.null(names(group)))
      group <- list(group)
    N <- 0
    M <- length(group)
    for (m in (1:M))
      N <- N + prod(group[[m]]$size)
    if ((!is.null(settings)) & (!is.null(settings$Nmax))){
      Nmax <- settings$Nmax
    }else{
      Nmax <- 100
    }
    if (N > Nmax)
      test.group <- TRUE
  }
  
  
  if (test.group==TRUE){
    r <- NULL
    rgr <- list()
    for (m in (1:M)){
      gm <- group[[m]]
      sm <- group[[m]]$size
      Nm <- prod(sm)
      if (!isfield(gm,'level')){
        model.info <- parse.model(model)
        gm$level <- model.info$level
      }
      gm$level <- tolower(gm$level)
      sms <- NULL
      sml <- NULL
      i1 <- which(gm$level=="population")
      if (length(i1)==1){
        sms <- c(sms,sm[i1])
        sml <- c(sml,"population")
      }
      i1 <- which(gm$level=="covariate")
      if (length(i1)==1){
        sms <- c(sms,sm[i1])
        sml <- c(sml,"covariate")
      }
      i1 <- which(gm$level=="individual")
      if (length(i1)==1){
        sms <- c(sms,sm[i1])
        sml <- c(sml,"individual")
      }
      i1 <- which(gm$level=="longitudinal")
      if (length(i1)==1){
        sms <- c(sms,sm[i1])
        sml <- c(sml,"longitudinal")
      }
      gm$size <- sms
      gm$level <- sml
      smk <- sms
      if (length(smk)>1){
        nm2 <- prod(smk[2:length(sm)])
      }else{
        nm2 <- 1
      }
      Km <- ceiling(sms[1]/ceiling(Nmax/nm2))
      smk[1] <- ceiling(Nmax/nm2)
      gmk <- gm
      gmk$size <- smk
      nmk <- prod(smk)
      settings$data.in=TRUE
      dataIn <- simulxunit(model=model,group=gmk,treatment=treatment,parameter=parameter,
                           output=output,data=data,project=project,settings=settings,
                           regressor=regressor,varlevel=varlevel)
      settings$data.in=FALSE
      Nmk <- 0
      for (k in (1:Km)){
        if (!is.null(settings$seed))
          settings$seed <- settings$seed +10
        Nmk <- Nmk + nmk
        rmk <- simulxunit(data=dataIn,settings=settings)
        Ntest <- NULL
        if (Nmk > Nm )
          Ntest <- Nm
        if (M>1){
          r <- mergeres(r,rmk,m=m,N=Ntest)
        }else{
          r <- mergeres(r,rmk,N=Ntest)          
        }
      }
      rgr[[m]] <- list(size=gm$size, level=gm$level)
    }
    r$group <- rgr
  }else{
    r <- simulxunit(model=model,group=group,treatment=treatment,parameter=parameter,
                    output=output,data=data,project=project,settings=settings,
                    regressor=regressor,varlevel=varlevel)
  }
  Sys.setenv(LIXOFT_HOME="")
  return(r)
}

simulxunit <- function(model=NULL,group=NULL,treatment=NULL,parameter=NULL,output=NULL,
                       data=NULL, project=NULL, settings=NULL, regressor=NULL, varlevel=NULL)
{ 
  #--------------------------------------------------
  # Manage settings
  #--------------------------------------------------
  cc  <-  processing_setting(settings)
  s <- cc[[2]]
  data.in <- cc[[1]]
  
  
  if(is.null(data)){
    
    dataIn          <- list()    
    #--------------------------------------------------
    #    MODEL
    #--------------------------------------------------
    if (!(is.null(model))) {
      if(model=="pkmodel"){
        model = generateModelFromPkModel(parameter,output) 
      } else {
        model_ext <- file_ext(model)
        if(model_ext=="xml"){
          model = pharmml2mlxtran(model)
        }
      }
    }
    
    #--------------------------------------------------
    #     Monolix project
    #--------------------------------------------------
    if (!(is.null(project)))  {
      ans           <- processing_monolix(project,model,treatment,parameter,output,group)
      model         <- ans$model
      treatment     <- ans$treatment
      parameter     <- ans$param
      output        <- ans$output
      group         <- ans$group
    }
    iop.group <- 1
    if (is.null(group))
      iop.group <- 0
    #--------------------------------------------------
    lv <- list(treatment=treatment,
               parameter=parameter,
               output=output,
               regressor=regressor,
               varlevel=varlevel,
               group=group,
               model=model
    )
    
    doseRegimen <- lv$treatment
    lv  <- hformat(lv)
    
    dataIn    <-  hgdata(lv)
    dataIn$model <- model
    dataIn$iop.group <- iop.group
  }else{
    dataIn <- data
    iop.group <- data$iop.group
#     group=NULL
  }
  
  if (length(s)==0){
    argList <- list(DATA=dataIn) 
  } else {
    argList <- list(DATA=dataIn, SETTINGS=s)       
  }

  if(data.in==F){
    dot_call <- .Call
    dataOut  <- dot_call( "mlxComputeR", argList, PACKAGE = "mlxComputeR" )
     dataOut  <- convertmlx(dataOut,dataIn,iop.group)
#      dataOut  <- convertmlx(dataOut,dataIn)
    if (!(is.null(project)))  {
      nd <- length(dataOut)
      if(!is.null(doseRegimen)){
        dataOut[[nd+1]] <- doseRegimen
        names(dataOut)[nd+1] <- "treatment"
      }
    }
    return(dataOut)
  }else{
    return(dataIn)
  }
}

mergeres <- function(r,s,m=NULL,N=NULL){
  K <- length(s)
  if (is.null(r)){
    if (!is.null(m)){
      for (k in (1:K))
        if ("id" %in% names(s[[k]])){
          s[[k]]$group <- factor(m) 
          i1 <- which(names(s[[k]])=="id")
          i2 <- which(names(s[[k]])=="group")
          s[[k]] <- s[[k]][,c((1:i1),i2,((i1+1):(i2-1)))]
        }  
    }
    u <- s
  }else{
    u <- r
    for (k in (1:K)){
      if (is.data.frame(s[[k]])){
        if (!is.null(m))
          s[[k]]$group <- factor(m)      
        
        if ("id" %in% names(s[[k]])){
          ir <- as.numeric(tail(r[[k]]$id,1))
          skid <- as.numeric(s[[k]]$id) + ir
          s[[k]]$id <- factor(skid) 
          if (!is.null(N))
            s[[k]] <- s[[k]][skid<=N,]
        }
        #         u[[k]] <- merge(r[[k]],s[[k]],all=TRUE)
        u[[k]] <- rbind(r[[k]],s[[k]])
        attr(u[[k]],"name")=attr(s[[k]],"name")
      }
    }
  } 
  return(u)
}

