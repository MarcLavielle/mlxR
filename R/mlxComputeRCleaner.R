#' @importFrom methods setRefClass
mlxComputeR.Unload <- function (arg=NULL) {
  dot_call <- .Call 
  if(is.loaded('mlxComputeRUnload', PACKAGE='mlxComputeR'))
  {
    dot_call("mlxComputeRUnload", PACKAGE = "mlxComputeR") 
  }
}
cleanerEnv<-environment()
mlxComputeRcleanUp<- setRefClass("mlxComputeRcleanUp",
                        methods = list(
                          initialize = function(...) {                     
                            reg.finalizer(cleanerEnv,mlxComputeR.Unload,onexit =TRUE)}
                        ))

.onLoad<-function(libname, pkgname){ 
    mlxComputeRCleaner <-mlxComputeRcleanUp$new()
}


#' mlxComputeRCleaner manages several sessions of mlxComputeR
#' 
#' @param arg argument
