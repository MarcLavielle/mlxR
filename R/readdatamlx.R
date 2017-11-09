#' Read formatted data file
#' 
#' Read data in a Monolix/NONMEM format
#' 
#' See http://simulx.webpopix.org/mlxr/readdatamlx/ for more details.
#' @param project a Monolix project
#' @param datafile a formatted data file 
#' @param header a vector of strings (mandatory if \code{datafile} is used) 
#' @param infoProject an xmlfile 
#' @param out.data TRUE/FALSE (default=FALSE) returns the original data as a table and some information about the Monolix project  
#' @param addl.ss number of additional doses to use for steady-state  (default=10) 
#' 
#' @return A list of data frames 
#' @examples
#' \dontrun{
#' d <- readDatamlx(project='monolixRuns/warfarin_project.mlxtran')
#' names(d)
#' head(d$treatment)
#' head(d$covariate)
#' head(d$y1)
#' 
#' #-- reserved key-words for the header:
#' #   ID,TIME,AMT,ADM,RATE,TINF,Y,YTYPE,X,COV,CAT,OCC,MDV,EVID,ADDL,SS,II,IGNORE
#' d <- readDatamlx(datafile='monolixRuns/warfarin_data.txt', 
#'                  header=c('id','time','amt','y','ytype','cov','cov','cat'))
#' }
#' @importFrom stats time
#' @export
readDatamlx  <- function(project=NULL, datafile=NULL, header=NULL, infoProject=NULL, out.data=FALSE, addl.ss=10){
  # READDATAMLX
  #
  # READDATAMLX reads a datafile and create a list.
  id <- NULL
  observationName <- NULL
  datas=NULL
  
  if (!is.null(project)) {
    infoProject <- getInfoXml(project)
    r= tryCatch(
      readPopEstimate(file.path(infoProject$resultFolder,'populationParameters.txt'))
      , error=function(e) {
        error<-  geterrmessage()
        return(readPopEstimate(file.path(infoProject$resultFolder,'estimates.txt'))
        )
      }      
    )    
    datas$populationParameters <- r[[1]]
  }
  
  if (!is.null(infoProject)) {
    header          = infoProject$dataheader
    observationName = infoProject$output
    datafile        = infoProject$datafile
  } 
  
  headerList      = c('ID','TIME','AMT','ADM','RATE','TINF','Y','YTYPE',
                      'X','COV','CAT','OCC','MDV','EVID','ADDL','SS','II')
  newList         = tolower(headerList)
  newList[3:4] <- c('amount','type')
  newHeader       = vector(length=length(header))
  ixdose = NULL
  
  header=unlist(strsplit(header, ",")) 
  header <- toupper(header)
  header[header=="DPT"]="ADM"
  nlabel = length(header)
  
  icov <- icat <- iid <- iamt <- iy <- iytype <- ix <- iocc <- imdv <- NULL
  ievid <- iaddl <- iii <- iss <- NULL
  
  for (i in 1:length(headerList)) {
    hi=headerList[[i]]
    ih <- which(header %in% hi)
    if (length(ih)==0)
      ih=NULL
    ii <- paste0( 'i', tolower(hi) )
    eval(parse(text= paste0(ii,"=ih")))
    if (!is.null(ih))
      newHeader[ih]=newList[i]      
  }
  
  if (length(iocc)>1) 
    stop("Multiple levels of occasions are not supported")
  
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
    h1 = read.table(datafile, sep=",", nrows=1)
    h2 = read.table(datafile, sep=";", nrows=1)
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
  } else if (tolower(fileFormat)==";"||tolower(fileFormat)=="semicolumn") {
    delimiter=';'
  } else if (tolower(fileFormat)=="\t") {
    delimiter='\t'
  } else
    delimiter=','
  
  catNames<-NULL
  headerTest = read.table(datafile, comment.char="",sep=delimiter, nrows=1,stringsAsFactors=FALSE)
  if(headerTest[1,1]=="#") {
    headerToUse<-headerTest[,-1]
    dataNoHeader    =  tryCatch(
      read.table(datafile,comment.char = "#", sep=delimiter,stringsAsFactors=FALSE)
      , error=function(e) {
        error<-  geterrmessage()
        message(paste0("WARNING: reading data using delimiter '",delimiter,"' failed: ", geterrmessage()))
        return( read.table(datafile,comment.char = "#",stringsAsFactors=FALSE))
      }      
    )    
    data<- dataNoHeader
    names(data)<- headerToUse
    
  } else {
    data = tryCatch(
      (if(!is.null(icat)) {
        colCatType<-rep(NA,length(headerTest))
        catNames <-headerTest[icat]
        names(catNames)<-NULL
        catNames<-trimws(unlist(catNames))
        colCatType[icat]<-rep("character",length(icat))
        read.table(datafile, comment.char="", header = TRUE, sep=delimiter,colClasses = colCatType)
      } else {
        read.table(datafile, comment.char="", header = TRUE, sep=delimiter)
      }), error=function(e) {
        error<-  geterrmessage()
        message(paste0("WARNING: reading data using delimiter '",delimiter,"' failed: ", geterrmessage()))
        return( read.table(datafile, comment.char="", header = TRUE))
      }      
    )
  }
  
  if (out.data) {
    infoProject$delimiter <- delimiter
    infoProject$dataheader <- header
    return(list(data=data, infoProject=infoProject))
  }
  
  #---remove rows containing NA-------
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
    if (!is.null(imdv))
      iobs1 <- iobs1[S[iobs1,imdv]!=1]
    if (!is.null(ievid))
      iobs1 <- iobs1[S[iobs1,ievid]==0]
    i0 <- c(grep(' .',S[iobs1,iy],fixed=TRUE),grep('. ',S[iobs1,iy],fixed=TRUE))
    if (length(i0)>0)
      iobs1 <- iobs1[-i0]
    #keep  the data only for  ids which have observation
    if (!is.null(iytype)) {
      idObs <-NULL
      ytype <- factor(S[iobs1,iytype])
      l.ytype <- levels(ytype)
      if (is.null(observationName))
        observationName <- paste0("y",l.ytype)
      n.y <- length(observationName)
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
      Scatj <- gsub(" ", "", Scatj, fixed = TRUE)
      S[[icat[j]]] <- as.factor(Scatj)  
    }
  }
  
  if (!is.null(iocc)) {
    socc <- S[,c(iid,itime,iocc)]
    socc1 <- socc[with(socc, order(id, time,occ)), ]
    socc2 <- socc[with(socc, order(id,occ, time)), ]
    if (!identical(socc1,socc2))
      stop("Only occasions within a same period of time are supported")
  }
  ##************************************************************************
  #       TREATMENT FIELD
  #**************************************************************************
  #  i1.evid <- NULL
  u.evid <- NULL
  if (!is.null(iamt)) {
    i1 = findstrcmp(S[[iamt]],'.', not=TRUE)
    if (!is.null(ievid)) {
      i1 <- i1[S[i1,ievid]!=0]
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
    si1dose <- si1[,ixdose]
    if (length(ixdose)==1)
      u=data.frame(idnum[i1], t[i1], si1dose)
    else
      u=cbind(list(idnum[i1], t[i1]), si1dose)
    names(u) = c('id',newHeader[[itime]],newHeader[ixdose])
    #
    u.addl <- NULL
    u.ss <- NULL
    if (!is.null(iaddl)) {
      addl <- as.numeric(as.character(S[i1,iaddl]))
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
          u.evid <- rbind(u.evid,uk[addl.ss,c(1,2)])
        }
      }
    }
    #    if (!is.null(ievid)) {   }
    u <- rbind(u,u.addl)
    u <- rbind(u,u.ss)
    # u <- u[order(u$id,u$time),]
    if (("evid" %in% names(u)) & any(u$evid==4) )
      stop("Washout (EVID=4) is not supported")
    
    if(nrow(u)) {
      datas   = c(datas,list(treatment = u))
    }
  }
  
  ##************************************************************************
  #       OBSERVATION FIELD
  #**************************************************************************
  iobs1   = findstrcmp(S[[iy]],'.', not=TRUE)
  if (!is.null(imdv))
    iobs1 <- iobs1[S[iobs1,imdv]!=1]
  if (!is.null(ievid))
    iobs1 <- iobs1[S[iobs1,ievid]==0]
  i0 <- c(grep(' .',S[iobs1,iy],fixed=TRUE),grep('. ',S[iobs1,iy],fixed=TRUE))
  if (length(i0)>0)
    iobs1 <- iobs1[-i0]
  
  yvalues = data.frame(id=idnum[iobs1], time=t[iobs1], y=as.numeric(as.character(S[iobs1,iy])))
  if (!is.null(iytype)){ 
    ytype <- factor(S[iobs1,iytype])
    l.ytype <- levels(ytype)
    if (is.null(observationName))
      observationName <- paste0("y",l.ytype)
    n.y <- length(observationName)
    # n.y <- length(l.ytype)
    # if (length(observationName)<n.y)
    #   observationName <- paste0("y",l.ytype)
    y<- list()
    for (in.y in (1:n.y)) {
      y[[in.y]] <- yvalues[ytype==l.ytype[in.y],]
      names(y[[in.y]])[3] <- observationName[in.y]
      attr(y[[in.y]],'type') <- "longitudinal"
    }
    #     yvalues$ytype <- observationName[S[[iytype]][iobs1]]
  } else {
    y <- yvalues
    if (is.null(observationName))
      observationName <- "y"
    names(y)[3] <- observationName
    attr(y,'type') <- "longitudinal"
    y <- list(y)
  }
  names(y) <- observationName
  datas <- c(datas,y)
  # datas$observation = y
  
  ##************************************************************************
  #       REGRESSOR FIELD
  #**************************************************************************
  
  if (nx>0) {
    Sx <- S[ix]
    Dx <- data.frame(id=idnum, time=t, Sx)
    ix.num <- which(!sapply(Sx,is.numeric))
    if (!is.null(ix.num)) {
      jx <- NULL
      for (k in (ix.num)) {
        jx <- c(jx, findstrcmp(Sx[[ix.num[k]]],'.'))
      }
      if (!is.null(jx))
        Dx <- Dx[-jx,]
      for (k in (ix.num))
        Dx[[k+2]] <- as.numeric(as.character(Dx[[k+2]]))        
    }
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
      ov.id <- ov.io[,1]
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
      datas$occasion <- NULL
  }
  
  ##************************************************************************
  #       COVARIATE FIELD
  #*************************************************************************
  
  cdf <- cdf.iov <- NULL
  nc = length(icov) + length(icat)
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
        datas[[k]] <- dk[order(ik,dk$time),]
      else
        datas[[k]] <- dk[order(ik),]
      if (is.null(iid))
        datas[[k]]$id <- NULL
    }
  }
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

getInfoXml  <- function (project)
{
  myOldENVPATH = Sys.getenv('PATH');
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  infoProject = list(datafile=NULL, dataformat=NULL, dataheader=NULL, output=NULL, resultFolder=NULL, mlxtranpath=NULL );
  
  # get path and name of monolix project
  mlxtranpath      = dirname(project);
  mlxtranpathfile = file_path_sans_ext(project)
  mlxtranfile = file_path_sans_ext(basename(project))
  infoProject$mlxtranpath = mlxtranpath
  if(file_ext(project) == "mlxtran") {
    #  project<-mlxProject2xml(project)
    session<-Sys.getenv("session.simulx")
    xmlfile <- file.path(mlxtranpath,paste0(mlxtranfile,"_tr.xmlx"))
    zz=file.path(session,'lib','lixoftLanguageTranslator')
    str=paste0('"',zz,'" --from=mlxproject --to=xmlx')  
    str=paste0(str,' --output-file=',xmlfile,' --input-file=',project,' --option=with-observation-model') 
    system(str, wait=T)
  } else {
    xmlfile <- project
  }
  
  infoResultFolder         = myparseXML(xmlfile, mlxtranpath, "resultFolder")
  infoProject$resultFolder = infoResultFolder[[1]]$uri
  ##************************************************************************
  #       GET DATA INFO
  #*************************************************************************
  #  get data format and data header used in the current project
  #   Exemple : 
  #
  #   infoProject = 
  #           datafile         : './warfarin_data.txt'
  #           dataformat       : '\t'
  #           dataheader       : {'ID'  'TIME'  'AMT'  'Y'  'YTYPE'  'COV'  'IGNORE'  'IGNORE'}
  #
  #
  infoData                = myparseXML(xmlfile, mlxtranpath, "data")
  infoProject$datafile    = infoData[[1]]$uri
  infoProject$dataformat  = infoData[[1]]$columnDelimiter
  # regressor in monolixC+++ are now named REG
  headers <- gsub("REG","X",infoData[[1]]$headers)
  infoProject$dataheader  = headers
  ##************************************************************************
  #       GET OUTPUT INFO
  #*************************************************************************
  #   Exemple : 
  #
  #   infoProject = 
  #           output           : {'conc'  'pca'}
  infoOutput         = myparseXML(xmlfile, mlxtranpath, 'observationModel')
  
  for (k in 1:length(infoOutput)) 
    infoProject$output[[k]] = infoOutput[[k]]$name
  
  infoParam = myparseXML(xmlfile, mlxtranpath, "parameter")
  info.length <- unlist(lapply(infoParam,length))
  infoParam <- infoParam[info.length==2]
  p.names <- do.call("rbind", lapply(infoParam, "[[", 1))[,1]
  p.trans <- do.call("rbind", lapply(infoParam, "[[", 2))[,1]
  infoProject$parameter <- list(name=p.names, trans=p.trans)
  
  infoFixedParam = myparseXML(xmlfile, mlxtranpath, "fixedParameter")
  info.length <- unlist(lapply(infoFixedParam,length))
  infoFixedParam <- infoFixedParam[info.length==2]
  fixp.names <- do.call("rbind", lapply(infoFixedParam, "[[", "name"))[,1]
  fixp.values <- do.call("rbind", lapply(infoFixedParam, "[[", "value"))[,1]
  fixedParamValues <-as.numeric(fixp.values)
  names(fixedParamValues) = fixp.names
  infoProject$fixedParameters <- fixedParamValues
  
  if(file_ext(project) == "mlxtran")
    unlink(xmlfile, recursive=T)
  
  return(infoProject)
}
