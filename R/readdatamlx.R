#' Read formatted data file
#' 
#' Read data in a Monolix/NONMEM format
#' 
#' See http://simulx.webpopix.org/mlxr/readdatamlx/ for more details.
#' @param data a list with fields
#' \itemize{
#'   \item \code{dataFile}: path of a formatted data file
#'   \item \code{headerTypes}: a vector of strings
#' }
#' @param project a Monolix project
#' @param out.data TRUE/FALSE (default=FALSE) returns the original data as a table and some information about the Monolix project  
#' @param nbSSDoses number of additional doses to use for steady-state (default=10) 
#' @param obs.rows a list of observation indexes 
#' @param error.iov TRUE/FALSE (default=TRUE) returns an error message if occasions are overlapping  
#' @param filter filter to apply to the data (string)  
#' @param datafile (deprecated) a formatted data file 
#' @param header (deprecated) a vector of strings  
#' @param infoProject (deprecated) an xmlfile 
#' @param addl.ss (deprecated) number of additional doses to use for steady-state  (default=10) 
#' 
#' @return A list of data frames 
#' @examples
#' \dontrun{
#' # using a Monolix project:
#' d <- readDatamlx(project='projects/warfarinPK.mlxtran')
#' 
#' 
#' # using a data file:
#' warfarinPK <- list(dataFile = "data/warfarinPK.csv",
#'                    headerTypes = c("id", "time", "observation", "amount", 
#'                                    "contcov", "contcov", "catcov"),
#'                    administration = "oral")
#' d <- readDatamlx(data=warfarinPK)
#' 
#' }
#' @importFrom stats time
#' @importFrom tools file_path_sans_ext
#' @export
readDatamlx  <- function(project=NULL, data = NULL, out.data=FALSE, nbSSDoses=10, obs.rows=FALSE, error.iov=FALSE, filter=NULL,
                         datafile=NULL, header=NULL, infoProject=NULL, addl.ss=NULL){
  id <- NULL
  observationModelName <- NULL
  datas=NULL
  
  if (!is.null(addl.ss)) 
    warning("addl.ss is deprecated. Use nbSSDoses instead.", call. = FALSE)
  if (!is.null(nbSSDoses))
    addl.ss <- nbSSDoses 
  
  
  if (!is.null(project)){
    
    if (!file.exists(project)) 
      stop(paste0("The Monolix project ", file.path(getwd(),project), " does not exists..."), call.=FALSE)
    
    infoProject <- getProjectInformation(project)
    if (is.null(infoProject))
      stop("The project could not be loaded properly", call.=FALSE)
    
    r= tryCatch(
      readPopEstimate(infoProject$resultFolder)
      , error=function(e) {
        error<-  NULL
      }      
    )    
    datas$populationParameters <- r$param
    
  }
  
  if (!is.null(infoProject)) {
    header          = infoProject$dataheader
    observationModelName = infoProject$fit$model
    observationDataName = infoProject$fit$data
    observationYtypeName = infoProject$fit$ytype
    datafile        = infoProject$datafile
  } 
  
  if (!is.null(data)) {
    header <- data$headerTypes
    datafile <- data$dataFile
  }
  
  headerList      = c('ID','TIME','AMT','ADM','RATE','TINF','Y','YTYPE',
                      'X','COV','CAT','OCC','MDV','EVID','ADDL','SS','II',
                      'CENS', 'LIMIT', 'DATE', 'IGNOREDLINE')
  newList         = tolower(headerList)
  newList[3:4] <- c('amount','type')
  newHeader       = vector(length=length(header))
  ixdose = NULL
  
  header=unlist(strsplit(header, ",")) 
  header <- toupper(header)
  header[header=="DPT"]="ADM"
  header[header=="ADMID"]="ADM"
  header[header=="AMOUNT"]="AMT"
  header[header=="OBSERVATION"]="Y"
  header[header=="OBSID"]="YTYPE"
  header[header=="CONTCOV"]="COV"
  header[header=="CATCOV"]="CAT"
  header[header=="REGRESSOR"]="X"
  nlabel = length(header)
  
  icov <- icat <- iid <- iamt <- iy <- iytype <- ix <- iocc <- imdv <- iignoredline <- NULL
  ievid <- iaddl <- iii <- iss <- iadm <- irate <- itinf <- ilimit <- icens <- NULL
  
  for (i in 1:length(headerList)) {
    hi=headerList[[i]]
    eval(parse(text= paste0("i",hi," <- NULL")))
    ih <- which(header %in% hi)
    if (length(ih)==0)
      ih=NULL
    ii <- paste0( 'i', tolower(hi) )
    eval(parse(text= paste0(ii,"=ih")))
    if (!is.null(ih))
      newHeader[ih]=newList[i]      
  }
  
  if (length(iocc)>1) 
    stop("Multiple levels of occasions are not supported", call.=FALSE)
  
  # iss <- ists
  
  ##************************************************************************
  #       Gestion du format du fichier de donnees
  #*************************************************************************  
  fileFormat=NULL
  if (is.list(infoProject)) {
    if (isfield(infoProject, 'dataformat'))
      fileFormat = infoProject$dataformat      
  }
  if (is.null(fileFormat)) {
    tmp=unlist(strsplit(datafile, "\\."))
    e= tmp[[length(tmp)]]
    if (e=='csv')
      fileFormat="csv"
    else
      fileFormat="space"
  }
  if (tolower(fileFormat)=="csv") {
    h1 = lixoft.read.table(file = datafile, sep = ",", nrows = 1)
    h2 = lixoft.read.table(file = datafile, sep = ";", nrows = 1)
    if (length(h1)>length(h2)) 
      delimiter=','
    else 
      delimiter=';'
    
  } else if (tolower(fileFormat)=="space") {
    delimiter=""
  } else if (tolower(fileFormat)==" ") {
    delimiter=""
  } else if (tolower(fileFormat)=="tab") {
    delimiter='\t'
  } else if (tolower(fileFormat)==";"||tolower(fileFormat)=="semicolumn"||tolower(fileFormat)=="semicolon") {
    delimiter=';'
  } else if (tolower(fileFormat)=="\t") {
    delimiter='\t'
  } else
    delimiter=','
  
  catNames<-NULL
  headerTest =lixoft.read.table(file = datafile, comment.char = "", sep = delimiter, nrows = 1, stringsAsFactors = FALSE)
  headerTest[1,which(is.na(headerTest[1,]))] <- "ignore"
  if(headerTest[1,1]=="#" ) {
    headerToUse<-headerTest[,-1]
    dataNoHeader    =  tryCatch(
      lixoft.read.table(file = datafile,comment.char = "#", sep=delimiter,stringsAsFactors=FALSE)
      , error=function(e) {
        error<-  geterrmessage()
        message(paste0("WARNING: reading data using delimiter '",delimiter,"' failed: ", geterrmessage()))
        return( lixoft.read.table(file = datafile,comment.char = "#",stringsAsFactors=FALSE))
      }      
    )    
    data<- dataNoHeader
    names(data)<- headerToUse
    
  } else {
    
    istrCols = c()
    
    if (!is.null(iytype))
      istrCols = append(istrCols, iytype)
    
    if (!is.null(icat)) {
      
      catNames <- headerTest[icat]
      names(catNames) <- NULL
      catNames <- trimws(unlist(catNames))
      istrCols = append(istrCols, icat)
      
    }
    
    colClasses <- rep(NA, length(headerTest))
    colClasses[istrCols] <- rep("character", length(istrCols))
    
    
    data = tryCatch({ 
      
      lixoft.read.table(file = datafile, comment.char="", header = TRUE, sep=delimiter, colClasses = colClasses)
      
    } , error=function(e) {
      
      error<-  geterrmessage()
      message(paste0("WARNING: reading data using delimiter '", delimiter, "' failed: ", geterrmessage()))
      return( lixoft.read.table(file = datafile, comment.char = "", header = TRUE, colClasses = colClasses) )
      
    }      
    )
  }
  
  if (!is.null(filter))
    eval(parse(text= paste0("data <- subset(data, ",filter,")")))
  
  if (out.data) {
    infoProject$delimiter <- delimiter
    infoProject$dataheader <- header
    return(list(data=data, infoProject=infoProject))
  }
  
  #---remove ignored lines -------
  if (!is.null(iignoredline)) {
    il <- which((data[iignoredline])==1)
    if(length(il)>0)
      data <- data[-il,]
  }

  #---remove lines containing NA-------
  if (!is.null(iid)) {
    narowsData <- which(is.na(data[iid])) # removed in ID column only
    if(length(narowsData)>0)
      data <- data[-narowsData,]
  }
  
  #-------------------------------
  if (!is.null(iii))
    levels(data[[iii]])[levels(data[[iii]])=="."]=0
  
  #-------------------------------
  if (!is.null(iocc)) {
    names(data)[iocc] <- "occ"
    data$OCC <- as.factor(data[[iocc]])
    icat <- c(icat,which(names(data)=="OCC"))
  }
  
  if (!is.null(iid)) names(data)[iid] <- "id"
  if (!is.null(itime)) names(data)[itime] <- "time"
  
  S       = data
  S0      = names(data)
  i.new <- c(icov,icat,ix,iocc)
  newHeader[i.new] = S0[i.new]
  
  if (!is.null(iid)) {
    iobs1   = findstrcmp(S[[iy]],'.', not=TRUE)
    if (!is.null(imdv)) {
      for (kmdv in (1: length(imdv)))
        iobs1 <- iobs1[S[iobs1,imdv[kmdv]]!=1]
    }
    if (!is.null(ievid)) {
      levels(S[,ievid])[levels(S[,ievid])=="."] <- "0"  
      S[,ievid] <- as.numeric(as.character(S[,ievid]))
      iobs1 <- iobs1[S[iobs1,ievid] %in% c(0, 2)]
    }
    i0 <- c(grep(' .',S[iobs1,iy],fixed=TRUE),grep('. ',S[iobs1,iy],fixed=TRUE))
    if (length(i0)>0)
      iobs1 <- iobs1[-i0]
    #keep  the data only for  ids which have observation
    if (!is.null(iytype)) {
      idObs <-NULL
      ytype <- factor(S[iobs1,iytype])
      l.ytype <- levels(ytype)
      if (is.null(observationModelName))
        observationModelName <- paste0("y",l.ytype)
      n.y <- length(observationModelName)
      for (in.y in (1:n.y)) 
        idObs <-c(idObs,as.character(S[iobs1[which(ytype==l.ytype[in.y])],iid]))
      idObs<-unique(idObs)
    } else {
      idObs<-unique(S[[iid]][iobs1])
    }
    idObsRows<-which(S[[iid]]%in%idObs)
    S <- S[idObsRows,]
    
    ans    = funique(S[[iid]])
    iduf   = ans$arg1
    iuf    = ans$arg2
    #idnumf = ans$arg3
    ans = fsort(iuf)
    ia  = ans$arg1
    ib  = ans$arg2
    
    ans = fsort(ib)
    ic  = ans$arg2
    ids = iduf[ib]
    
    iu    = ia
    # idnum = as.factor(ic[idnumf])
    idnum = as.factor(S[[iid]])
  } else {
    iduf <- 1
    idnum <- as.factor(1)
    idObsRows <- nrow(S)
  }
  
  iduf = as.factor(iduf)
  N     = length(iduf)
  
  if (is.null(itime)) 
    itime=ix[1]
  
  if (!is.null(itime)) {
    t=S[[itime]]
  } else {
    # no time,  no regressor
    t = 1:nrow(S) 
  }
  nx=length(ix)
  
  if (!is.null(icat)) {
    for (j in (1:length(icat))) {
      Scatj <- S[[icat[j]]]  
      #      Scatj <- gsub(" ", "", Scatj, fixed = TRUE)
      S[[icat[j]]] <- as.factor(Scatj)  
    }
  }
  
  if (!is.null(iocc)) {
    socc <- S[,c(iid,itime,iocc)]
    socc['time'] <- socc[names(S)[itime]]
    socc1 <- socc[with(socc, order(id, time,occ)), 1:3 ]
    socc2 <- socc[with(socc, order(id,occ, time)), 1:3 ]
    if (!identical(socc1,socc2) & error.iov==TRUE)
    stop("Overlapping occasions are not handled by Simulx", call.=FALSE)
    #  return(list(error="Overlapping occasions are not handled by Simulx"))
    #stop("Only occasions within a same period of time are supported", call.=FALSE)
  }
  ##************************************************************************
  #       TREATMENT FIELD
  #**************************************************************************
  #  i1.evid <- NULL
  u.evid <- NULL
  if (!is.null(iamt)) {
    i1 = findstrcmp(S[[iamt]],'.', not=TRUE)
    if (!is.null(ievid)) {
      i1 <- i1[S[i1,ievid]!=0 & S[i1,ievid]!=2]
      #      i1.evid <- i1[S[i1,ievid]==4]
    }
    i0 <- c(grep(' .',S[i1,iamt],fixed=TRUE),grep('. ',S[i1,iamt],fixed=TRUE))
    if (length(i0)>0)
      i1 <- i1[-i0]
    si1 <- S[i1,]
    si1[[iamt]] <- as.numeric(as.character(si1[[iamt]]))
    if (!is.null(iadm)) si1[[iadm]] <- as.numeric(as.character(si1[[iadm]]))
    if (!is.null(ievid)) si1[[ievid]] <- as.numeric(as.character(si1[[ievid]]))
    ixdose <- c(iamt, irate, itinf, iadm, ievid)
    #si1dose <- data.frame(sapply(si1[,ixdose], function(x) as.numeric(as.character(x))))
    si1dose <- si1[,ixdose]
    if (length(ixdose)==1)
      u=data.frame(idnum[i1], t[i1], si1dose)
    else
      u=cbind(list(idnum[i1], t[i1]), si1dose)
    if (!is.null(itime))
      names(u) = c('id',newHeader[[itime]],newHeader[ixdose])
    else {
      u <- u[-2]
      names(u) = c('id',newHeader[ixdose])
    }
    #
    u.addl <- NULL
    u.ss <- NULL
    if (!is.null(iaddl)) {
      levels(S[,iaddl])[levels(S[,iaddl])=="."] <- "0"  
      addl <- as.numeric(as.character(S[i1,iaddl]))
      levels(S[,iii])[levels(S[,iii])=="."] <- "0"  
      ii <- as.numeric(as.character(S[i1,iii]))
      j.addl <- which(addl>0)
      if (length(j.addl)>0) {
        for (j in (1:length(j.addl))) {
          k <- j.addl[j]
          adk <- addl[k]
          uk <- u[rep(k, adk),]
          uk$time <- u$time[k] + ii[k]*seq(1:adk)
          u.addl <- rbind(u.addl,uk)
        }
      }
      j.addl <- which(addl<0)
      if (length(j.addl)>0) {
        for (j in (1:length(j.addl))) {
          k <- j.addl[j]
          adk <- -addl[k]
          uk <- u[rep(k, adk),]
          uk$time <- u$time[k] - ii[k]*seq(1:adk)
          u.addl <- rbind(u.addl,uk)
        }
      }
    }
    i1.ss <- NULL
    if (!is.null(iss)) {
      ss <- S[i1,iss]
      ii <- as.numeric(as.character(S[i1,iii]))
      j.ss <- which(ss==1)
      if (length(j.ss)>0) {
        i1.ss <- i1[j.ss]
        for (j in (1:length(j.ss))) {
          k <- j.ss[j]
          uk <- u[rep(k, addl.ss),]
          uk$time <- u$time[k] - ii[k]*seq(1:addl.ss)
          u.ss <- rbind(u.ss,uk)
          #          u.evid <- rbind(u.evid,uk[addl.ss,c(1,2)])
        }
      }
    }
    #    if (!is.null(ievid)) {   }
    u <- rbind(u,u.addl)
    u <- rbind(u,u.ss)
    # u <- u[order(u$id,u$time),]
    
    if ("evid" %in% names(u)){
      if (any(u$evid==4 & error.iov))
        stop("Washout (EVID=4) is not supported", call.=FALSE)
      u$evid <- NULL
    }
    
    if(nrow(u)) {
      datas   = c(datas,list(treatment = u))
    }
  }
  
  ##************************************************************************
  #       OBSERVATION FIELD
  #**************************************************************************
  iobs1   = findstrcmp(S[[iy]],'.', not=TRUE)
  if (!is.null(imdv)) {
    for (kmdv in (1: length(imdv)))
      iobs1 <- iobs1[S[iobs1,imdv[kmdv]]!=1]
  }
  if (!is.null(ievid))
    iobs1 <- iobs1[S[iobs1,ievid] %in% c(0, 2)]
  i0 <- c(grep(' .',S[iobs1,iy],fixed=TRUE),grep('. ',S[iobs1,iy],fixed=TRUE))
  if (length(i0)>0)
    iobs1 <- iobs1[-i0]
  
  yvalues = data.frame(id=idnum[iobs1], time=t[iobs1], y=as.numeric(as.character(S[iobs1,iy])))
  if (!is.null(icens))
    yvalues['cens'] <- S[iobs1,icens]
  if (!is.null(ilimit))
    yvalues['limit'] <- S[iobs1,ilimit]
  if (!is.null(iytype)){ 
    ytype <- factor(S[iobs1,iytype])
    if (!exists("observationYtypeName"))
      observationYtypeName <- levels(ytype)
    if (!exists("observationDataName"))
      observationDataName <- paste0("y",observationYtypeName)
    n.y <- length(observationDataName)
    y<- obsRows <- list()
    for (in.y in (1:n.y)) {
      idObs.i <- which(ytype==observationYtypeName[in.y])
      y[[in.y]] <- yvalues[idObs.i,]
      names(y[[in.y]])[3] <- observationDataName[in.y]
      attr(y[[in.y]],'type') <- "longitudinal"
      obsRows[[in.y]] <-  idObsRows[iobs1[idObs.i]]
    }
    names(obsRows) <- observationDataName
    #     yvalues$ytype <- observationModelName[S[[iytype]][iobs1]]
  } else {
    y <- yvalues
    if (!exists("observationDataName"))
      observationDataName <- "y"
    names(y)[3] <- observationDataName
    attr(y,'type') <- "longitudinal"
    y <- list(y)
    obsRows <- list(idObsRows[iobs1])
    names(obsRows) <- observationDataName
    
  }
  names(y) <- observationDataName
  datas <- c(datas,y)
  # datas$observation = y
  
  ##************************************************************************
  #       REGRESSOR FIELD
  #**************************************************************************
  
  if (nx>0) {
    Sx <- S[ix]
    Sx[Sx=="."] <- NA
    ix.num <- which(!sapply(Sx,is.numeric))
    if (!is.null(ix.num)) {
      for (k in (ix.num)) {
        Sx[,k] <- as.numeric(as.character(Sx[,k])) 
      }
    }
    Dx <- data.frame(id=idnum, time=t, Sx)
    datas$regressor <- subset(Dx, !duplicated(cbind(id,time)))
  }
  
  ##************************************************************************
  #       OCCASION FIELD
  #**************************************************************************
  
  if (!is.null(iocc)) {
    S[[iocc]] <- as.numeric(as.character(S[[iocc]]))
    ov <-data.frame(id=idnum, time=t, occ=S[iocc])
    oo=ov[-2]
    u <- unique(oo)
    #io=match(data.frame(t(u)), data.frame(t(oo)))
    io=match(data.frame(t(as.numeric(rownames(u)))), data.frame(t(as.numeric(rownames(oo)))))
    ov.io <- ov[io,]
    datas$occasion <- ov.io
  } else {
    ov.io <- NULL
    if (!is.null(ievid)) {
      io <- sort(unique(c(iu,which(S[[ievid]]==4))))
      ov.io <- S[io,c(iid,itime)]
    }
    if (!is.null(u.evid)) 
      ov.io <- u.evid
    if (!is.null(ov.io)) {
      if (is.null(nrow(ov.io)))
        ov.id <- ov.io[1]
      else
        ov.id <- ov.io[1]
      occ <- rep(1,length(ov.id))
      if (length(ov.id)>=2) {
        for (i in (2:length(ov.id))) {
          if (ov.id[i]==ov.id[i-1])
            occ[i] <- occ[i-1] + 1
          else
            occ[i] <- 1
        }     
      }
      ov.io$occ <- occ
      datas$occasion <- ov.io
      iocc <- length(names(data))+1
    }
  }
  
  if (!is.null(datas$occasion)) {
    if (length(unique(datas$occasion$occ)) == 1)
      datas$occasion <- iocc <- ov.io$occ <- NULL
  }
  
  ##************************************************************************
  #       COVARIATE FIELD
  #*************************************************************************
  
  cdf <- cdf.iov <- NULL
  nc = length(icov) + length(icat)
  for (j in seq_len(length(icat))) {
    icatj <- icat[j]
    if ("." %in% levels(S[[icatj]]))
      levels(S[[icatj]])[levels(S[[icatj]])=="."] <- "NA"
    if (sum(is.na(S[[icatj]]))>0) {
      if (!("NA" %in% levels(S[[icatj]])))
        levels(S[[icatj]]) <- c(levels(S[[icatj]]), "NA")
      S[[icatj]][is.na(S[[icatj]])] <- "NA"
    }
  }
  
  if (nc>0) {
    ic <- c(icov,icat)
    cdf <- data.frame(id=iduf)
    if (!is.null(iocc)) 
      cdf.iov <- ov.io[,c(1,2)]
    k1 <- 1
    k2 <- 2
    for (k in (1:nc)) {
      if (is.null(iocc) | dim(unique(S[,c(iid,ic[k])]))[1]==N) {
        k1 <- k1+1
        cdf[[k1]] <- S[[ic[k]]][iu]
        names(cdf)[k1]=names(S)[ic[k]]    
      } else {
        k2 <- k2+1
        cdf.iov[[k2]] <- S[[ic[k]]][io]
        names(cdf.iov)[k2]=names(S)[ic[k]]    
      }
    }
    if (dim(cdf)[2]>1 & k1>1)
      datas[["covariate"]] = cdf
    if (!is.null(cdf.iov) & k2>2)
      datas[["covariate.iov"]] = cdf.iov
  }
  
  for (k in (1:length(datas))) {
    dk <- datas[[k]]
    if (is.data.frame(dk)) {
      ik <- match(dk$id,iduf)
      if (!is.null(dk$time))
        ok <- order(ik,dk$time)
      else
        ok <- order(ik)
      datas[[k]] <- dk[ok,]
      if (is.null(iid))
        datas[[k]]$id <- NULL
      imk <- match(names(datas)[k], names(obsRows))
      if (!is.na(imk))
        obsRows[[imk]] <- obsRows[[imk]][ok]
    }
  }
  if (obs.rows)
    datas$obsRows <- obsRows
  if (!is.null(iid)) {
    datas$id <- iduf  
    datas$N <- N
  }
  foo <- catNames[catNames %in% names(cdf)]
  if (length(foo) >0) 
    datas$catNames <- foo
  foo <-catNames[catNames %in% names(cdf.iov)]
  if (length(foo) >0) 
    datas$catNames.iov <- foo
  if (!is.null(datas$covariate.iov)) {
    datas[["covariate.iiv"]] <- datas[["covariate"]]
    datas[["catNames.iiv"]] <- datas[["catNames"]]
    datas[["catNames"]] <- datas[["covariate"]] <- NULL
  }
  return(datas)
}


