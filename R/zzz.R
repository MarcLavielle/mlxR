#' @importFrom Rcpp sourceCpp
#' @importFrom ggplot2 ggplot
NULL

.onLoad <- function(libname, pkgname){

  mess.mlxlibrary="\n\nMlxlibrary has probably not been installed. 
You can install it from  http://download.lixoft.com/?software=mlxlibrary\n
Otherwise, execute <Mlxlibrary PATH>/lib/mlxLibraryFirstLaunch.exe"

  #--- ensuring mlx library from lixsoft is installed
  myOS <- Sys.info()['sysname'];
  lixoft.path <- if (myOS == "Windows"){ 
     file.path(Sys.getenv("USERPROFILE"),"lixoft")
  } else {
     file.path(Sys.getenv("HOME"),"lixoft")
  }
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
  
}

