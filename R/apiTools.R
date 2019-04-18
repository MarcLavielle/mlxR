.getOS <- function(){
  
  if (.Platform$OS.type == "windows") { 
    return("Windows")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("Apple") 
  } else if (.Platform$OS.type == "unix") { 
    return("Unix")
  } else {
    stop("Unknown OS")
  }
  
}

.checkLixoftConnectorsAvailibility <- function(){
  
  if (!isNamespaceLoaded("lixoftConnectors")){
    .error("lixoftConnectors package is not loaded.")
    return(invisible(FALSE))
  }
    
  return(invisible(TRUE))
  
}

.error <- function(str){
  
  if (getOption("lixoft_notificationOptions")$errors == 0)
    message(paste0("[ERROR] ", str))
  
}

.warning <- function(str){
  
  if (getOption("lixoft_notificationOptions")$warnings == 0)
    message(paste0("[WARNING] ", str))
  
}

.info <- function(str){
  
  if (getOption("lixoft_notificationOptions")$info == 0)
    message(paste0("[INFO] ", str))
  
}

#' Read Lixoft@ files
#' 
#' Utility function to read Lixoft@ formated input/output files
#' 
#' @param file file path of the file to read
#' @param sep separator
#' @param \dots see \code{\link{read.table}}
#' @importFrom utils read.table
#' @export
lixoft.read.table <- function(file, sep = "", ...){
  
  if (!is.null(file) && !file.exists(file))
    return()


  testedDelimiters <- if (sep != "") c(sep) else c()
  testedDelimiters <- c(testedDelimiters, "\t", ",", ";", " ")
  if (is.null(file))
    testedDelimiters <- c(testedDelimiters, "")
  
  out <- table(0,0)
  count <- 1
  
  while(ncol(out) == 1 && count <= length(testedDelimiters)){
    
    if (!is.null(file))
      try(out <- read.table(file = file, sep = testedDelimiters[count], ...), silent = TRUE)
    else
      try(out <- read.table(sep = testedDelimiters[count], ...), silent = TRUE)
    
    count <- count + 1
    
  }
  
  return(if (ncol(out) == 1) NULL else out)
  
}

# !! RETRO-COMPTATIBILITY ============================================================ !!
.hiddenCall <- function(command){
  
  eval.parent(parse(text = command))
  
}
# ==================================================================================== !!