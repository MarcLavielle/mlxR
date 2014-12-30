mess.mlxlibray="\n\nMlxlibrary has probably not been installed. 
You can install it from  http://download.lixoft.com/?software=mlxlibrary\n
Otherwise, execute <Mlxlibrary PATH>/lib/mlxLibraryFirstLaunch.exe"
#--------------------------------------
#             paths  
#--------------------------------------
my.dir=getwd()

myOS <- Sys.info()['sysname'];
if (myOS == "Windows")
{ 
  lixoft.path <- file.path(Sys.getenv("USERPROFILE"),"lixoft")
} else {
  lixoft.path <- file.path(Sys.getenv("HOME"),"lixoft")
}
lixoft.ini  <- file.path(lixoft.path,"lixoft.ini")
if (!file.exists(lixoft.ini)){
  mess <- paste("The file ",lixoft.ini," does not exists.",mess.mlxlibray)
  stop(mess)
}
ll          <- file(lixoft.ini, open = "r")
lines       <- readLines(ll)
close(ll)

#---  Mlxlibrary path  
mlxlibrary.i <- grep("mlxlibrary=",lines,fixed=TRUE)
if (length(mlxlibrary.i)>0) {
  s <- strsplit(lines[mlxlibrary.i],"=")
  sp1 <- s[[1]][2]
  sp2=gsub("\\\\","/",sp1,fixed=TRUE)
  while (!identical(sp1,sp2)) {
    sp1=sp2
    sp2=gsub("//","/",sp1,fixed=TRUE)
  }
  mlxlibrary.path <- sp1
} else {
  mlxlibrary.path <- NULL
}

#---  Mlxplore path  
mlxplore.i <- grep("mlxplore=",lines,fixed=TRUE)
if (length(mlxplore.i)>0) {
  s <- strsplit(lines[mlxplore.i],"=")
  sp1 <- s[[1]][2]
  sp2=gsub("\\\\","/",sp1,fixed=TRUE)
  while (!identical(sp1,sp2)) {
    sp1=sp2
    sp2=gsub("//","/",sp1,fixed=TRUE)
  }
  mlxplore.path <- sp1
} else {
  mlxplore.path <- NULL
}

#---  Mlxtoolbox path  
mlxtoolbox.path <- dirname(parent.frame(2)$ofile) #initMlxtoolbox.R location

#--------------------------------------
#             check the paths  
#--------------------------------------
  if (is.null(mlxlibrary.path)) {
  stop(mess.mlxlibray,call.="FALSE")}

if (!file.exists(file.path(mlxlibrary.path,'lib'))) {
    stop(mess.mlxlibray,call.="FALSE")}
  
if (!is.element("initMlxR.R",list.files(mlxtoolbox.path))){
  cat("\n")
  stop("check the path to MlxR (mlxtoolbox.path) \n",call.="FALSE")}
  
  
if (!is.null(mlxplore.path)){
  if (!file.info(mlxplore.path)$isdir){
    warning("check the path to Mlxplore (mlxplore.path) if you want to use mxlplore.R",call.="FALSE")
  } else {
    Sys.setenv(session.mlxplore=mlxplore.path)}
}
#--------------------------------------
#       environment variables  
#--------------------------------------
Sys.setenv(session.simulx=mlxlibrary.path)

setwd(mlxtoolbox.path)
mlxtoolbox.path=getwd()
Sys.setenv(mlxtoolbox.path=mlxtoolbox.path)

#--------------------------------------
#       packages  
#--------------------------------------
packinfo <- installed.packages ();

#--- check R version
installedRVersion = packinfo[c("base"), c("Version")]
requiredRVersion = "3.0.0";
if (installedRVersion < requiredRVersion)
  stop("Install R version 3.0.0 or higher")

list.packages=NULL
#--- install "Rcpp" or check "Rcpp" version
if (!is.element("Rcpp", installed.packages()[,1])){
#	install.packages("Rcpp")
	list.packages=paste(list.packages,"Rcpp") 
}else{ 
installedRcppVersion = packinfo[c("Rcpp"), c("Version")]
requiredRcppVersion = "0.11.0";
myOS <- Sys.info()['sysname'];
if (myOS == "Windows") {
  	if (installedRcppVersion < requiredRcppVersion)
    		stop("Install Rcpp version 0.11.0 or higher.\n
If you don't want to update your version of Rcpp:
  - delete file <Mlxlibrary path>/runtime/lib/mlxComputeR.dll
  - install Rtools,
  - source initMlxR.R") }
}

#--- install "tools"
if (!is.element("tools", installed.packages()[,1])){
  # install.packages("tools")
  list.packages=paste(list.packages,"tools")
}

if (!is.null(list.packages)){
  stop(paste0("you need to install the following packages: \n",list.packages),call.="F")
}
#--- required libraries
library(tools)
library(Rcpp)

#--------------------------------------
#       load Mlxlibrary  
#--------------------------------------
mlxr.dir=getwd()
source("src/src_mlx/mlxComputeRLibraryBuilder.R");
mlxComputeRLibraryBuilder(mlxlibrary.path, mlxr.dir);


#--------------------------------------
#       source the R codes  
#--------------------------------------

setwd(file.path(mlxtoolbox.path,"/src"))
for (nm in list.files(pattern = "\\.[R]$")) 
{source(nm)}
setwd("src_mlx")
for (nm in list.files(pattern = "\\.[R]$")) 
{source(nm)}

setwd(my.dir)

