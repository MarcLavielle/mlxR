#' @importFrom methods setRefClass new getGeneric

cleanerEnv <- environment()
mlxComputeRcleanUp <- setRefClass("mlxComputeRcleanUp",
                                 methods = list(
                                   initialize = function(...) {                     
                                     reg.finalizer(cleanerEnv, .unloadPackage, onexit =TRUE)}
                                 ))

.onLoad <-function(libname, pkgname){
  
  # environment cleaner:
  mlxComputeRCleaner <- mlxComputeRcleanUp$new()

  
  # set notification options:
  notificationOptions <- getOption("lixoft_notificationOptions")
  
  if (is.null(notificationOptions$errors))
    notificationOptions$errors <- 0
  if (is.null(notificationOptions$warnings))
    notificationOptions$warnings <- 0
  if (is.null(notificationOptions$info))
    notificationOptions$info <- 0
  
  options(lixoft_notificationOptions = notificationOptions)
  
}

.onUnload <- function(libpath){
  
  .unloadPackage()
  
}
