#' @importFrom Rcpp sourceCpp
#' @importFrom tcltk tk_choose.dir

NULL

NAMESPACE <- environment()
mlx_library_ready <- FALSE
SYS_PATH_mlx_library<-""
initMlxLibrary <- function(){
  if( mlx_library_ready ){
    Sys.setenv('PATH'=NAMESPACE[["SYS_PATH_mlx_library"]])
    return()
  }
  mess.mlxlibrary="\n\nMlxlibrary has probably not been installed. You can install it from  
http://download.lixoft.com/?software=mlxlibrary

Otherwise, provide the path of the Mlxlibrary using the directory browser.

You can also run the following R command from the console:
> setMlxLibraryPath(<Mlxlibrary PATH>) 

e.g.
> setMlxLibraryPath('C:/ProgramData/Lixoft/MonolixSuite2018R1') 
"
  
  #--- ensuring mlx library from lixsoft is installed
  myOS <- Sys.info()['sysname']; 
  lixoft.path <- {
    if (myOS == "Windows"){ 
      file.path(Sys.getenv("USERPROFILE"),"lixoft")
    } else {
      file.path(Sys.getenv("HOME"),"lixoft")
    }
  } 
  
  
  opt.inner <- options( show.error.messages=FALSE) 
  on.exit( options( opt.inner)) 
  
  lixoft.ini  <- file.path(lixoft.path,"lixoft.ini")
  if (!file.exists(lixoft.ini)){
    wm <- paste0("\nThe file ",lixoft.ini," does not exists.",mess.mlxlibrary)
    warning(wm, immediate.=TRUE, call.=FALSE)
    mlx.path <- setMlxLibraryPath()
    cat("\nYou can now try to run again your R script\n")
    stop("",call.=FALSE)
    
  }
  
  # small utility function to get a path from lixoft.ini file
  get_lixoft_path <- function(name){
    rx <- sprintf( "%s=", name)
    line <- grep( rx, lines, fixed=TRUE, value=TRUE)
    if( length(line) ){
      normalizePath( gsub( rx, "", line[1L] ) )
    }
  }
  
  lines <- readLines(lixoft.ini) # is blocking lixoft.ini for other threads, and lixoft.ini can be modified by mlxComputeR
  
  
  #---  Mlxlibrary and MlxPlore paths
  mlxlibrary.path <- get_lixoft_path("monolixSuite")
  if(is.null(mlxlibrary.path)){
    mlxlibrary.path <- get_lixoft_path("mlxlibrary")
  }
  if (is.null(mlxlibrary.path) || !file.exists(file.path(mlxlibrary.path,'lib')) ) {
    wm <- paste0("\n",mess.mlxlibrary)
    warning(wm, immediate.=TRUE)
    mlx.path <- setMlxLibraryPath()
    cat("\nYou can now restart R and try to run again your R script\n")
    stop("",call.=FALSE)
  }
  Sys.setenv(session.simulx=mlxlibrary.path)
  
  mlxplore.path <- get_lixoft_path("monolixSuite")
  if(is.null(mlxplore.path)){
    mlxplore.path   <- get_lixoft_path("mlxplore")
  }
  
  if (!is.null(mlxplore.path)){
    if (!file.info(mlxplore.path)$isdir){   
      msg <- sprintf("check the path to Mlxplore (%s) if you want to use mxlplore.R", mlxplore.path )
      warning(msg,call.="FALSE")
    } else {
      Sys.setenv(session.mlxplore=mlxplore.path)    
    }
  }
  
  #--- load Mlxlibrary
  mlxComputeRLibraryBuilder(mlxlibrary.path)
  
  unlock <- unlockBinding
  unlock( "mlx_library_ready", NAMESPACE )
  NAMESPACE[["mlx_library_ready"]] <- TRUE
  unlock( "SYS_PATH_mlx_library", NAMESPACE )
  NAMESPACE[["SYS_PATH_mlx_library"]] <-Sys.getenv('PATH')
}

#' Sets the MlxLibrary path in order to tell mlxR where is the MlxLibrary to use
#'     
#' @param mlxLibraryPath  the absolute path to the location of MlxLibrary 
#' @export
setMlxLibraryPath <- function(mlxLibraryPath=NULL){
  myOS <- Sys.info()['sysname']; 
  if (myOS == "Windows"){
    if (is.null(mlxLibraryPath))
      mlxLibraryPath <- tk_choose.dir(caption = 'Select the MlxLibrary folder (usually in "C:/ProgramData/Lixoft") ')
    lauchCommand<-file.path(mlxLibraryPath,"/lib/mlxSuiteInit.exe")
    if(!file.exists(lauchCommand)){
      lauchCommand<-file.path(mlxLibraryPath,"/lib/mlxLibraryFirstLaunch.exe")
    }
  } else {
    if (is.null(mlxLibraryPath))
      mlxLibraryPath <- tk_choose.dir(caption = 'Select the MlxLibrary folder (usually in user directory) ')
    lauchCommand<-file.path(mlxLibraryPath,"/lib/mlxSuiteInit")
    if(!file.exists(lauchCommand)){
      lauchCommand<-paste0(mlxLibraryPath,"/lib/mlxLibraryFirstLaunch")
    }
  }
  system(lauchCommand)
  return(mlxLibraryPath)
}
