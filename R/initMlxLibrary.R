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
  
  
  ## Version 4 UUIDs have the form xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
  ## where x is any hexadecimal digit and y is one of 8, 9, A, or B
  ## e.g., f47ac10b-58cc-4372-a567-0e02b2c3d479
  uuid <- function(uppercase=FALSE) {
    
    hex_digits <- c(as.character(0:9), letters[1:6])
    hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits
    
    y_digits <- hex_digits[9:12]
    
    paste(
      paste0(sample(hex_digits, 8, replace=TRUE), collapse=''),
      paste0(sample(hex_digits, 4, replace=TRUE), collapse=''),
      paste0('4', paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
      paste0(sample(y_digits,1), paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
      paste0(sample(hex_digits, 12, replace=TRUE), collapse=''),
      sep='-')
  }
  
  uuidd<-uuid()   
  lixoftInitFile <-paste0(dirname(lixoft.ini),"/",basename(lixoft.ini))
  lixoftConn<-paste0(lixoftInitFile,uuidd)
  lixoftCopyInitFile<-paste0(lixoftInitFile,"-copy")
  if (myOS == "Windows"){
   if(!file.exists(lixoftCopyInitFile))
   {
     cat(" ",file=lixoftCopyInitFile)
     copyofInitFile<-paste0("xcopy  \"",lixoftInitFile,"\"  \"",lixoftCopyInitFile,"\" /Y /F  /Q")
     system(copyofInitFile,wait=T,intern=TRUE)
   }    
    cat(" ",file=lixoftConn)
    copyInitFile<-paste0("xcopy  \"",lixoftCopyInitFile,"\"  \"",lixoftConn,"\" /Y /F  /Q")
    
  } else {
    if(!file.exists(lixoftCopyInitFile))
    {      
      copyofInitFile<-paste0("cp  ",lixoftInitFile," ",lixoftCopyInitFile)
      system(copyofInitFile,wait=T,intern=TRUE)
    }    
    copyInitFile<-paste0("cp  ",lixoftCopyInitFile," ",lixoftConn)    
  }
  
 system(copyInitFile,wait=T,intern=TRUE)
  #lines <- readLines(lixoft.ini) # is blocking lixoft.ini for other threads
  
  lines <- readLines(lixoftConn)
    
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
    wm <- paste0("\n",mess.mlxlibrary)
    warning(wm, immediate.=TRUE)
    mlx.path <- setMlxLibraryPath()
    cat("\nYou can now try to run again your R script\n")
    stop("",call.=FALSE)
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
  unlink(lixoftConn)
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
    lauchCommand<-file.path(mlxLibraryPath,"/lib/mlxLibraryFirstLaunch.exe")
    
  } else {
    if (is.null(mlxLibraryPath))
      mlxLibraryPath <- tk_choose.dir(caption = 'Select the MlxLibrary folder (usually in user directory) ')
    lauchCommand<-paste0(mlxLibraryPath,"/lib/mlxLibraryFirstLaunch")
  }
  system(lauchCommand)
  return(mlxLibraryPath)
}
