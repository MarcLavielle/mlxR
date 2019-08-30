#' Write formatted data file
#' 
#' Write data contained in a list of dataframes in a single file (NONMEM/Monolix format)
#' or in several files as tables
#' 
#' See http://simulx.webpopix.org/mlxr/writedatamlx/ for more details.
#' @param r a list of dataframes
#' @param result.file a string with the name of the file 
#' @param result.folder a string with the name of the folder 
#' @param sep (default = ",") 
#' @param ext a string with the extension of the file names 
#' @param digits (default = 5) 
#' @param app.file  TRUE/FALSE (default=FALSE) append to file 
#' @param app.dir  TRUE/FALSE (default=FALSE) append to dir 
#' @param project  A Monolix project
#' @examples
#' \dontrun{
#' modelPK <- inlineModel("
#' [LONGITUDINAL]
#' input = {V, Cl, a1}
#' EQUATION:
#' Cc = pkmodel(V, Cl)
#' DEFINITION:
#' y1 ={distribution=lognormal, prediction=Cc, sd=a1}
#' ")
#' adm  <- list(amount=100, time=seq(0,50,by=12))
#' p <- c(V=10, Cl=1, a1=0.1)
#' y1 <- list(name=c('y1'), time=seq(5,to=50,by=5))
#' res <- simulx(model=modelPK, treatment=adm, parameter=p, output=y1)
#' writeDatamlx(res, result.file="res.csv")
#' writeDatamlx(res, result.file="res.txt", sep="\t")
#' writeDatamlx(res, result.folder="res")
#' }
#' @importFrom utils write.table
#' @export
writeDatamlx <- function(r,result.file=NULL,result.folder=NULL,sep=",",ext=NULL,digits=5,app.file=F,app.dir=F, project=NULL) 
{
  
  if (!is.null(project)) {
    dp <- readDatamlx(project=project, out.data=TRUE)
    dp.data   <- dp$data
    dp.info <- dp$infoProject
    dp.header <- dp.info$dataheader
    dp.names <- names(dp.data)
    if (is.null(result.file)) 
      result.file <- file.path(dp.info$resultFolder,paste0("sim_",basename(dp.info$datafile)))
  }
  
  if (!is.null(r$simulx) && r$simulx) project=NULL
  
  if (!is.null(result.folder)){
    if (app.dir==F){
      unlink(result.folder, recursive = TRUE, force = TRUE)
      Sys.sleep(0.1)
      dir.create(result.folder, showWarnings = FALSE, recursive = FALSE, mode = "0777")
    }
    
    nr <- names(r$parameter)
    r0 <- r
    r0[nr] <- NULL
    r0["group"] <- NULL
    nr0 <- names(r0)
    f.names <- NULL
    if (is.null(ext)){
      if (sep %in% c(",",";"))
        ext <- ".csv"
      else
        ext <- ".txt"
    }
    for (k in (1:length(nr0))){
      rk <- r[[nr0[k]]]
      if (!is.null(ncol(rk))){
        fk <- file.path(result.folder,paste0(nr0[k],ext))
        for (j in (1:ncol(rk))){
          if (typeof(rk[,j])=="double")
            rk[,j] <- round(rk[,j], digits=digits)
          i.na <- which(is.na(rk[,j]))
          if (length(i.na)>0)
            rk[i.na,j]="."
        }
        
        if (app.file == F) 
          write.table(rk,fk,row.names=FALSE,quote=FALSE,sep=sep,append=F)
        else
          write.table(rk,fk,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=sep,append=T)
      }
    }
  }
  
  if (!is.null(r['format.original'])) {
    format.original <- r[['format.original']]
    r['format.original'] <- NULL
  } else {
    format.original <- NULL
  }
  if (!is.null(result.file) && is.null(format.original)){
    ir <- which(names(r)=="regressor")
    if (length(ir)>0)
      attr(r[[ir]],"type") <- "regressor"
    y.attr <- sapply(r,attr,"type")
    j.long <- which(y.attr=="longitudinal")
    y <- NULL
    rlong.names <- sapply(r[j.long],"names")
    rln <- unique(unlist(rlong.names))
    for (k in (1:length(j.long))){
      rk <- r[[j.long[k]]]
      nk <- names(r[j.long[k]])
      names(rk)[names(rk)==nk] <- "y"
      if (length(j.long)==1){
        y <- rk
      } else {
        yk <- cbind(rk,list(ytype=k))
        if ("cens" %in% rln && is.null(yk$cens))  
          yk$cens <- "0"
        if ("limit" %in% rln && is.null(yk$limit))  
          yk$limit <- NA
        y <- rbind(y,yk)
      }
    }
    i0 <- which(y$cens==0)
    if (length(i0)>0 && !is.null(y$limit))
      y$limit[i0] <- NA
    
    
    M <- y
    
    if (!is.null(r$treatment)){
      trt <- r$treatment
      trt$y <- NA
      if (!is.null(M$cens))   trt$cens  <- "."
      if (!is.null(M$limit))  trt$limit <- NA
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
      n <- nrow(M)
      if (n2==n1+1)
        dim(occ) <- c(n,1)
      for (i in (2:n)){
        if (any(is.na(occ[i,])))
          occ[i,] <- occ[(i-1),]
      }
      M[,(n1+1):n2] <- occ
    }
    
    if (!is.null(M$occ)){
      n <- nrow(M)
      for (i in (2:n)){
        if (is.na(M$occ[i]))
          M$occ[i] <- M$occ[i-1]
      }
    }
    
    
    if (!is.null(r$covariate)){
      n1 <- ncol(M)
      M <- merge(M,r$covariate,all=T)
      n2 <- ncol(M)
      occ <- M[,(n1+1):n2]
      n <- nrow(M)
      if (n2==n1+1)
        dim(occ) <- c(n,1)
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
    
    lo <- NULL
    if (!is.null(M$pop))   lo <- c(lo, "pop")
    if (!is.null(M$rep)) lo <- c(lo, "rep")
    if (!is.null(M$id)) lo <- c(lo, "id")
    if (!is.null(M$time)) lo <- c(lo, "time")
    if (!is.null(M$ytype)) lo <- c(lo, "ytype")
    lo <- paste(lo,collapse=",")
    eval(parse(text=paste0("M <- M[with(M, order(",lo,")), ]")))
    
    if (!is.null(project)) {
      if (dim(dp.data)[1] != dim(M)[1])
        stop("Original data and simulated data don't have the same size: the original design should be used for the simulation.", call.=FALSE)
      i.id <- which(dp.header=="ID")
      dp.data[,i.id] <- uniquemlx(dp.data[,i.id])$sortIndex
      if (any(dp.data[,i.id] != M$id))
        stop("Original data and simulated data don't have the same size: the original design should be used for the simulation.", call.=FALSE)
      lo <- dp.names[i.id]
      i <- which(dp.header=="TIME")
      if (length(i)>0) lo <- c(lo, dp.names[i])
      i <- which(dp.header=="YTYPE")
      if (length(i)>0) lo <- c(lo, dp.names[i])
      lo <- paste(lo,collapse=",")
      eval(parse(text=paste0("dp.data <- dp.data[with(dp.data, order(",lo,")), ]")))
      
      i <- which(dp.header=="Y")
      if (length(i)>0) dp.data[,i] <- M$y
      i <- which(dp.header=="CENS")
      if (length(i)>0) {
        dp.data[,i] <- M$cens
      } else {
        dp.data$CENS <- M$cens
      }
      i <- which(dp.header=="LIMIT")
      if (length(i)>0) {
        dp.data[,i] <- M$limit
      } else {
        dp.data$LIMIT <- M$limit
      }
      dp.data[,i.id] <- r$originalId$oriId[dp.data[,i.id]]
      write.table(dp.data,result.file,row.names=FALSE,quote=FALSE,sep=dp.info$delimiter,append=F)
    } else {
      
      if (app.file == F) 
        write.table(M,result.file,row.names=FALSE,quote=FALSE,sep=sep,append=F)
      else
        write.table(M,result.file,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=sep,append=T)
    }
  } 
  if (!is.null(result.file) && !is.null(format.original)){
    iy <- which(format.original$infoProject$dataheader=="Y")
    itime <- which(format.original$infoProject$dataheader=="TIME")
    ilimit <- which(format.original$infoProject$dataheader=="LIMIT")
    icens <- which(format.original$infoProject$dataheader=="CENS")
    
    format.original$data[,iy] <- as.character(format.original$data[,iy])
    for (j in (1:length(format.original$obsRows))) {
    #  nj <- names(format.original$obsRows)[j]
      ndj <- dp.info$fit$data[j] 
      if (length(format.original$obsRows)>1) 
        ndj <- substr(ndj,2,nchar(ndj))
      nmj <- dp.info$fit$model[j]
      rowj <- format.original$obsRows[[j]]
      if (length(itime)>0) {
        if (identical(r[[nmj]][['time']], format.original$data[rowj,itime])) {
          format.original$data[rowj,iy] <- r[[nmj]][[nmj]]
        } else {
          iid <- which(format.original$infoProject$dataheader=="ID")
          id <- r[[nmj]][['id']]
          data.j <- format.original$data[rowj,]
          D <- NULL
          for (i in (1:nlevels(id))) {
            dij <- data.j[data.j[,iid]==r$originalId$oriId[i],]
            rij <- subset(r[[nmj]],id==r$originalId$newId[i] )
            dijt <- dij[,itime]
            sij <- sapply(rij$time, function(x) {which.min(abs(dijt-x))})
            D <- rbind(D, dij[sij,])
          }
          D[,itime] <- r[[nmj]][['time']]
          D[,iy] <- r[[nmj]][[nmj]]
          format.original$data <- format.original$data[-rowj,]
          format.original$data <- rbind(format.original$data, D)
        }
      } else {
        format.original$data[rowj,iy] <- r[[nmj]][[nmj]]
      }
      if (length(icens)>0) {
        if ("cens" %in% names(r[[nmj]]))
          format.original$data[format.original$obsRows[[j]],icens] <- r[[nmj]]$cens
        else
          format.original$data[format.original$obsRows[[j]],icens] <- 0
      }
      if ( (length(ilimit)>0) && ("limit" %in% names(r[[nmj]])))
        format.original$data[format.original$obsRows[[j]],ilimit] <- r[[nmj]]$limit
    }
    sep <- format.original$infoProject$delimiter
    write.table(format.original$data,result.file,row.names=FALSE,quote=FALSE,sep=sep,append=F)
  }
}