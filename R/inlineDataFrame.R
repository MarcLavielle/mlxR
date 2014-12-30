inlineDataFrame=function(str,header=TRUE,colClasses=NULL){
  if(is.null(colClasses)){
  df <- read.table(header=header,text=str)
  }else{
    df <- read.table(header=header,colClasses=colClasses,text=str)
  }
  
#  if(header==T)
#    df <- t(df)
  
#  if(header==F)
# df <- c(tapply(df$V2, df$V1, as.vector))
  
  return(df)
}