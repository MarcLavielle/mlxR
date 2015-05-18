#' @importFrom Rcpp sourceCpp
NULL

NAMESPACE <- environment()
mlx_library_ready <- FALSE
SYS_PATH_mlx_library<-""
initMlxLibrary <- function(){
  if( mlx_library_ready ){
    Sys.setenv('PATH'=NAMESPACE[["SYS_PATH_mlx_library"]])
   return()
}
  mess.mlxlibrary="\n\nMlxlibrary has probably not been installed. 
You can install it from  http://download.lixoft.com/?software=mlxlibrary\n
Otherwise, execute <Mlxlibrary PATH>/lib/mlxLibraryFirstLaunch.exe"

  #--- ensuring mlx library from lixsoft is installed
  myOS <- Sys.info()['sysname'];
  LIXOFT_HOME <- Sys.getenv( "LIXOFT_HOME" )
  lixoft.path <- if( identical( LIXOFT_HOME, "" ) ){
    if (myOS == "Windows"){ 
       file.path(Sys.getenv("USERPROFILE"),"lixoft")
    } else {
       file.path(Sys.getenv("HOME"),"lixoft")
    }
  } else LIXOFT_HOME
  
  lixoft.ini  <- file.path(lixoft.path,"lixoft.ini")
  if (!file.exists(lixoft.ini)){
    stop("The file ",lixoft.ini," does not exists.",mess.mlxlibrary)
  } 
  lines <- readLines(lixoft.ini)
  
  # small utility function to get a path from lixoft.ini file
  get_lixoft_path <- function(name){
    rx <- sprintf( "%s=", name)
    line <- grep( rx, lines, fixed=TRUE, value=TRUE)
    if( length(line) ){
        normalizePath( gsub( rx, "", line[1L] ) )
    }
  }
  
  #---  Mlxlibrary and MlxPlore paths  
  mlxlibrary.path <- get_lixoft_path("mlxlibrary")
  if (is.null(mlxlibrary.path) || !file.exists(file.path(mlxlibrary.path,'lib')) ) {
    stop(mess.mlxlibrary,call.="FALSE")
  }
  Sys.setenv(session.simulx=mlxlibrary.path)
  
  mlxplore.path   <- get_lixoft_path("mlxplore")
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
  
 #--- load C++ Data reader for Mlxlibrary
 mlxDataReaderRLibraryBuilder(mlxlibrary.path)
 
  unlock <- unlockBinding
  unlock( "mlx_library_ready", NAMESPACE )
  NAMESPACE[["mlx_library_ready"]] <- TRUE
 unlock( "SYS_PATH_mlx_library", NAMESPACE )
 NAMESPACE[["SYS_PATH_mlx_library"]] <-Sys.getenv('PATH')
  
}

