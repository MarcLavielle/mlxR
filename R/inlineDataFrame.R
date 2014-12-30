inlineDataFrame <- function(str,header=TRUE,colClasses=NA, ...){
  read.table( text = str, header=header, colClasses=colClasses, ... )
}