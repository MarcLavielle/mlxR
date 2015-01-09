#' Compute predictions and sample data from Mlxtran and PharmML models
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
    
  }else{
    dataIn=data
    group=NULL
  }
  
  if (length(s)==0){
    argList <- list(DATA=dataIn) 
  } else {
    argList <- list(DATA=dataIn, SETTINGS=s)       
  }
  
  if(data.in==F){
    dot_call <- .Call
    dataOut  <- dot_call( "mlxComputeR", argList, PACKAGE = "mlxComputeR" )
    Sys.setenv(LIXOFT_HOME="")
    dataOut  <- convertmlx(dataOut,dataIn)
    if (!(is.null(project)))  {
      nd <- length(dataOut)
      dataOut[[nd+1]] <- doseRegimen
      names(dataOut)[nd+1] <- "doseRegimen"
    }
    return(dataOut)
  }else{
    return(dataIn)
  }
}
