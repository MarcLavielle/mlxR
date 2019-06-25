mlxREnvironment <- new.env()

# assign lixoft environment variables:
assign("LIXOFT_DIRECTORY", "", envir = mlxREnvironment)
assign("ORIGINAL_OPTIONS", options(), envir = mlxREnvironment)

# !! RETRO-COMPTATIBILITY ========================================================== !!
assign("EXECUTION_SYS_PATH", "", envir = mlxREnvironment) # < 2019R1
assign("CURRENT_LIXOFT_CORE_LIBRARY", NULL, envir = mlxREnvironment) # { NULL | "mlxlibrary" | "lixoftConnectors" }

.useLixoftConnectors <- function(){
  
  currentLixoftCoreLib = get("CURRENT_LIXOFT_CORE_LIBRARY", envir = mlxREnvironment)
  return(!is.null(currentLixoftCoreLib) && currentLixoftCoreLib == "lixoftConnectors")
  
}

# store original environment variables:
myOS = .getOS()
if (myOS == "Unix") {
  assign("ORIGINAL_SYS_PATH", Sys.getenv("LD_LIBRARY_PATH"), envir = mlxREnvironment)
} else if (myOS == "Apple") {
  # path is set during installation
} else if (myOS == "Windows") {
  assign("ORIGINAL_SYS_PATH", Sys.getenv("PATH"), envir = mlxREnvironment)
}
remove(myOS)
# !! ================================================================================ !!



.unloadPackage <- function(arg = NULL){
  
  # reset R session options:
  options(get("ORIGINAL_OPTIONS", envir = mlxREnvironment))
  
  # !! RETRO-COMPTATIBILITY - < 2019R1 =============================================== !!
  # reset environment:
  myOS = .getOS()
  if (myOS == "Unix") {
    Sys.setenv( LD_LIBRARY_PATH = get("ORIGINAL_SYS_PATH", envir = mlxREnvironment) )
  } else if (myOS == "Apple") {
    # path is set during installation
  } else if (myOS == "Windows") {
    Sys.setenv( PATH = get("ORIGINAL_SYS_PATH", envir = mlxREnvironment) )
  }  
  # !! =============================================================================== !!
  
}


#' Initialize mlxR library
#' 
#' Initialize mlxR library
#' @param path (\emph{character}) [optional] Path to installation directory of the Lixoft suite.
#' If mlxR library is not already loaded and no path is given, the directory written in the lixoft.ini file is used for initialization.
#' @param ... [optional] Extra arguments passed to lixoftConnectors package when mlxR is used with a version of Lixoft(/@) software suite higher or equal to 2019R1.
#' \itemize{
#' \item \code{force} (\emph{bool}) [optional] Should mlxR initialization overpass lixoftConnectors software switch security or not. Equals FALSE by default.
#' }
#' @return A list:
#' \itemize{
#'   \item \code{software}: the software that is used (should be monolix with Rsmlx)
#'   \item \code{path}: the path to MonolixSuite
#'   \item \code{version}: the version of MonolixSuite that is used
#'   \item \code{status}: boolean equaling TRUE if the initialization has been successful.
#' }
#' @examples
#' \dontrun{
#' initMlxR(path = "/path/to/lixoftRuntime/")
#' }
#' @export
initMlxR <- function(path = NULL, ...){
  
  
  # check if mlxR needs to be (re-)initialized:
  currentLixoftCoreLibrary <- get("CURRENT_LIXOFT_CORE_LIBRARY", envir = mlxREnvironment) # RETRO
  currentPath <- get("LIXOFT_DIRECTORY", envir = mlxREnvironment)
  
  if (!is.null(path))
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  
  if (!is.null(currentLixoftCoreLibrary)){ # RETRO
    
    if (currentPath != "" && !is.null(path) && path != currentPath){
      .warning(paste0("mlxR package has already been initialized with \"", currentPath,
                      "\". R session must be restarted to use a different Lixoft installation directory."))
      return(invisible(list(status=FALSE, path=path)))   
    }
    
    # !! RETRO-COMPTATIBILITY - < 2019R1 =============================================== !!
    if (currentLixoftCoreLibrary == "mlxlibrary"){
      Sys.setenv('PATH' = get("EXECUTION_SYS_PATH", envir = mlxREnvironment))
      return(invisible(list(status=TRUE)))
    }
    # !! =============================================================================== !!
    
  }
  
  if (isNamespaceLoaded("lixoftConnectors")){
    
    lixoftConnectorsState <- NULL
    .hiddenCall('lixoftConnectorsState <- lixoftConnectors::getLixoftConnectorsState(quietly = TRUE)')
    
    
    if (!is.null(lixoftConnectorsState)){
      
      if (is.null(path)){
        
        #path <- lixoftConnectorsState$path
        
        if (lixoftConnectorsState$software == "simulx"){ # => nothing to be done
          
          assign("CURRENT_LIXOFT_CORE_LIBRARY", "lixoftConnectors", envir = mlxREnvironment) # RETRO
          assign("LIXOFT_DIRECTORY", lixoftConnectorsState$path, envir = mlxREnvironment)
          lixoftConnectorsState$status <- TRUE
          return(lixoftConnectorsState)
          
        }
        
        .info(paste0("lixoftConnectors package has already been initialized using the Lixoft installation directory \"",
                     lixoftConnectorsState$path, "\". This directory will be used to run \"simulx\"."))
        
      } else if (lixoftConnectorsState$path != path){
        .warning(paste0("lixoftConnectors package has already been initialized using an other Lixoft installation directory (\"",
                        lixoftConnectorsState$path, "\"). R session must be restarted to use a different Lixoft installation directory."))
        return(invisible(list(status=FALSE)))
      }
      
    }
    
  }
  
  
  # !! RETRO-COMPTATIBILITY ========================================================== !!
  # lixoft runtime directory identification:
  if (is.null(path))
    path = .findLixoftDirectory()
  path = normalizePath(path, winslash = "/", mustWork = FALSE)
  
  lixoftRuntimeProperties = .checkLixoftDirectory(path)
  
  if (!lixoftRuntimeProperties$status) {
    .hiddenCall('lixoftConnectors::initializeLixoftConnectors(path=path)')
    return(invisible(lixoftRuntimeProperties))  
  }
  # !! =============================================================================== !!
  
  
  
  # lixoft core library initialization:
  if (lixoftRuntimeProperties$reliesOnLixoftConnectors) { # >= 2019R1
   lixoft.status <- .initLixoftConnectorsLibrary(path, ...)
   lixoftConnectorsState <- NULL
   .hiddenCall('lixoftConnectorsState <- lixoftConnectors::getLixoftConnectorsState(quietly = TRUE)')
   lixoftConnectorsState$status <- lixoft.status
   return(lixoftConnectorsState) 
  } else { # < 2019R1 ================================================================= !!
    lixoftRuntimeProperties$status <- .initMlxRLibrary(path)
    return(lixoftRuntimeProperties)
    # !! =============================================================================== !!
  }
}

.initLixoftConnectorsLibrary <- function(path, ...){
  
  status <- FALSE
  .hiddenCall('status = lixoftConnectors::initializeLixoftConnectors(software = "simulx", path = path, ...)', ...)
  
  if (status){
    assign("CURRENT_LIXOFT_CORE_LIBRARY", "lixoftConnectors", envir = mlxREnvironment) # RETRO
    assign("LIXOFT_DIRECTORY", path, envir = mlxREnvironment)
  }
  
  return(invisible(status))
  
}

# !! < 2019R1 ======================================================================== !!
.initMlxRLibrary <- function(path){
  
  status = mlxComputeRLibraryBuilder(path)
  
  if (status){
    
    Sys.setenv(session.simulx = path)
    
    assign("LIXOFT_DIRECTORY", path, envir = mlxREnvironment)
    assign("CURRENT_LIXOFT_CORE_LIBRARY", "mlxlibrary", envir = mlxREnvironment)
    assign("EXECUTION_SYS_PATH", Sys.getenv('PATH'), envir = mlxREnvironment)
    
  }
  
  return(invisible(status))
  
}

.findLixoftDirectory <- function(){
  
  # check Lixoft softwares suite is installed:
  myOS <- .getOS()
  lixoftRootDirectory <- {
    if (myOS == "Windows"){
      file.path(Sys.getenv("USERPROFILE"),"lixoft")
    } else {
      file.path(Sys.getenv("HOME"),"lixoft")
    }
  }
  if (!file.exists(lixoftRootDirectory)){
    .error("Lixoft softwares suite has probably not been installed. You can install it from http://download.lixoft.com")
    return("")
  }
  
  
  # find lixoft.ini:
  lixoftIniPath <- file.path(lixoftRootDirectory, "lixoft.ini")
  if (!file.exists(lixoftIniPath)){
    .error(paste0("Impossible to find an initialization file for Lixoft softwares suite."))
    return("")
  }
  
  
  # read lixoft runtime path:
  readDirectory <- function(lines, key){
    
    if (length(lines) > 0){
      for (i in 1:length(lines)){
        res = regexpr(paste0("^", key, "="), lines[[i]])
        if (res != -1)
          return(substring(lines[[i]], res + attr(res, "match.length")))
      } 
    }
    return("")
    
  }
  
  lines <- readLines(lixoftIniPath) # is blocking lixoft.ini for other threads, and lixoft.ini can be modified by mlxComputeR
  
  
  lixoftRuntimeDirectory <- readDirectory(lines, "monolixSuite") # >= 2018R1
  if (lixoftRuntimeDirectory == "")
    lixoftRuntimeDirectory <- readDirectory(lines, "mlxlibrary") # < 2016R1
  if (lixoftRuntimeDirectory == "")
    .error(paste0("No setup informations found in the initialization file \"", lixoftIniPath, "\".\n-> mlxR will not been initialized."))
  
  return(lixoftRuntimeDirectory)
  
}

.checkLixoftDirectory <- function(path){
  
  res = list(status = FALSE, reliesOnLixoftConnectors = NULL)
  
  if (path == "" || !file.exists(file.path(path, 'lib')))
    return(res)
  
  
  myOS = .getOS()
  lixoftConnectorsLibPath <- file.path(path, 'lib', if (myOS == "Windows") 'lixoftConnectors.dll' else 'liblixoftConnectors.so')
  
  if (file.exists(lixoftConnectorsLibPath)){
    
    options(lixoft_lixoftConnectors_loadOptions = list(quietLoad = TRUE, initializePkg = FALSE))
    
    isLixoftConnectorsPkgAvailable <- FALSE
    .hiddenCall('isLixoftConnectorsPkgAvailable <- requireNamespace("lixoftConnectors", quietly = TRUE)')
    
    if (!isLixoftConnectorsPkgAvailable)
      .error(paste0("mlxR depends on \"lixoftConnectors\" (@Lixoft) package. Please install it using the following command:\n",
                    "> install.packages(\"", file.path(path, "connectors", "lixoftConnectors.tar.gz"), "\", repos = NULL)"))
    else
      res$status = TRUE
    
    res$reliesOnLixoftConnectors = TRUE
    
    options(lixoft_lixoftConnectors_loadOptions = NULL)
    
  }
  # !! < 2019R1 ====================================================================== !!
  else {
    
    mlxComputeR_APILibPath = file.path(path, 'lib', if (myOS == "Windows") 'mlxCompute_CAPI.dll' else 'libmlxCompute_CAPI.so')
    
    if (file.exists(mlxComputeR_APILibPath)){
      
      res$reliesOnLixoftConnectors = FALSE
      
      if (!requireNamespace("XML", quietly = TRUE))
        .error("mlxR depends on \"XML\" package. Please install it.")
      else if (!requireNamespace("Rcpp", quietly = TRUE))
        .error("mlxR depends on \"Rcpp\" package. Please install it.")
      else
        res$status = TRUE      
      
    }
    
  }
  # !! ============================================================================== !!
  
  return(invisible(res))
  
}
# !! ================================================================================ !!
