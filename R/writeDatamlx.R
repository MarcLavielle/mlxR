#' @export
writeDatamlx <- function(r,filename,sep=",", digits=3) 
{
  y.attr <- sapply(r,attr,"type")
  j.long <- which(y.attr=="longitudinal")
  y <- NULL
  if (length(j.long)==1){
    y <- r[[j.long]]
  }else if (length(j.long)>1){
    for (k in (1:length(j.long))){
      rk <- r[[j.long[k]]]
      nk <- names(r[j.long[k]])
      names(rk)[names(rk)==nk] <- "y"
      yk <- cbind(rk,list(ytype=k))
      y <- rbind(y,yk)
    }
  }
  M <- y
  
  if (!is.null(r$treatment)){
    trt <- r$treatment
    trt$y <- NA
    M <- merge(M,trt,all=TRUE)
  }
  
  j.reg <- which(y.attr=="regressor")
  if (length(j.reg)>0){
    for (k in (1:length(j.reg)))
      M <- merge(M,r[[j.reg[k]]],all=TRUE)
  }
  
  
  if (!is.null(r$parameter)){
    n1 <- ncol(M)
    M <- merge(M,r$parameter,all=T)
    n2 <- ncol(M)
    occ <- M[,(n1+1):n2]
    n <- nrow(occ)
    for (i in (2:n)){
      if (any(is.na(occ[i,])))
        occ[i,] <- occ[(i-1),]
    }
    M[,(n1+1):n2] <- occ
  }
  
  for (k in (1:ncol(M))){
    if (typeof(M[,k])=="double")
      M[,k] <- round(M[,k], digits=digits)
    i.na <- which(is.na(M[,k]))
    if (length(i.na)>0)
      M[i.na,k]="."
  }
  
  if (!is.null(M$id))
    M <- M[with(M, order(id, time)), ]
  else
    M <- M[with(M, order(time)), ]
  write.table(M,filename,row.name=FALSE,quote=FALSE,sep=sep)
  zz <- file(filename)
  close(zz)
  
}