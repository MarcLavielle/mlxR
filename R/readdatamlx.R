readdatamlx  <- function(infoProject=NULL, project=NULL){
  # READDATAMLX
  #
  # READDATAMLX reads a datafile and create a list.
  
  myOldENVPATH = Sys.getenv('PATH');
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  
  if (!is.null(project))
    infoProject <- getInfoXml(project)
  
  header          = infoProject$dataheader
  observationName = infoProject$output
  datafile        = infoProject$datafile
  headerList      = c('ID','TIME','AMT','ADM','RATE','TINF','Y','YTYPE','X','COV','CAT','OCC')
  newList         = c('id','time','amount','type','rate','tinf','y','ytype','x','cov','cat','occ')
  # newList         = cellfun(@lower,headerList,'UniformOutput',false)
  newHeader       = vector(length=length(header))
  ixdose = NULL
  datas=NULL
  
  header=unlist(strsplit(header, ","))  
  nlabel = length(header)
  
  icov <- icat <- iid <- iamt <- iy <- iytype <- ix <- iocc <- NULL
  
  for (i in 1:length(headerList))
  {
    hi=headerList[[i]]
    ih <- which(header %in% hi)
    if (length(ih)==0)
      ih=NULL
    ii <- paste0( 'i', tolower(hi) )
    eval(parse(text= paste0(ii,"=ih")))
    if (!is.null(ih))
      newHeader[ih]=newList[i]      
  }
  
  ##************************************************************************
  #       Gestion du format du fichier de donnees
  #*************************************************************************  
  fileFormat=NULL
  if (is.list(infoProject))
  {
    if (isfield(infoProject, 'dataformat'))
      fileFormat = infoProject$dataformat      
  }
  if (is.null(fileFormat))
  {  
    tmp=unlist(strsplit(datafile, "\\."))
    e= tmp[[length(tmp)]]
    if (e=='csv'){
      fileFormat="csv"
    }else{
      fileFormat="space"
    }
  }
  if (tolower(fileFormat)=="csv"){
    h1 = read.table(datafile, sep=",", nrows=1)
    h2 = read.table(datafile, sep=";", nrows=1)
    if (length(h1)>length(h2)) 
      delimiter=','
    else 
      delimiter=';'
    
  }else if (tolower(fileFormat)=="space"){
    delimiter=""
  }else if (tolower(fileFormat)==" "){
    delimiter=""
  }else if (tolower(fileFormat)=="tab"){
    delimiter='\t'
  }else if (tolower(fileFormat)==";"){
    delimiter=';'
  }else if (tolower(fileFormat)=="\t"){
    delimiter='\t'
  }else
    delimiter=','
  
  
  data    = read.table(datafile, comment.char="", header = TRUE, sep=delimiter)
  S       = data
  S0      = names(data)
  i.new <- c(icov,icat,ix,iocc)
  newHeader[i.new] = S0[i.new]
  
  ans    = funique(S[[iid]])
  iduf   = ans$arg1
  iuf    = ans$arg2
  idnumf = ans$arg3
  
  ans = fsort(iuf)
  ia  = ans$arg1
  ib  = ans$arg2
  
  ans = fsort(ib)
  ic  = ans$arg2
  ids = iduf[ib]
  
  iu    = ia
  idnum = ic[idnumf]
  N     = length(iduf)
  
  #   iop_id = 0
  #   i      = 1
  #   while(iop_id==0 && i<=N) {
  #     iop_id = (i==ids[[i]])
  #     i      = i+1
  #   }
  
  if (is.null(itime)) {
    itime=ix[1]
    ix=ix[2:length(ix)]
  }
  t=S[[itime]]
  nx=length(ix)
  
  nocc <- length(iocc)
  
  if (!is.null(icat)) {
    for (j in (1:length(icat)))
      S[[icat[j]]] <- as.factor(S[[icat[j]]])    
  }
  ##************************************************************************
  #       SOURCE FIELD
  #**************************************************************************
  if (!is.null(iamt)) {
    if (is.null(irate))
      irate = NULL
    if (is.null(iadm))
      iadm = NULL
    
    ixdose=rbind(c(iamt, irate, iadm))
  }
  if (!is.null(ixdose)) {
    i1       = findstrcmp(S[[ixdose[1]]],'.', not=TRUE)
    ndose    = length(i1) #match(FALSE,(!(is.na(ixdose))))
    nxdose   = length(ixdose)
    
    uv       = cbind(idnum[i1], t[i1], matrix(data=0,nrow=ndose,ncol=length(ixdose)))
    for (i in 1:ndose) {
      for(j in 1:nxdose)
        uv[i,2+j]=as.numeric(as.character(S[[ixdose[j]]][i1[i]]))
    }
    
    uh = c('id',newHeader[[itime]])
    for(j in 1:nxdose)
      uh=c(uh,newHeader[[ixdose[j]]])
    u        = list(label  = 'source',
                    name   = 'doseRegimen',
                    colNames = uh,
                    value=NULL)
    u$value =   uv
    #u      = {u}
    datas   = list(sources = u)
  }
  
  ##************************************************************************
  #       OBSERVATION FIELD
  #**************************************************************************
  
  iobs1   = findstrcmp(S[[iy]],'.', not=TRUE)
  yvalues = data.frame(id=idnum[iobs1], time=t[iobs1], y=S[iobs1,iy])
  if (!is.null(iytype)){ 
    ytype <- factor(S[iobs1,iytype])
    l.ytype <- levels(ytype)
    n.y <- length(observationName)
    y<- list()
    for (iy in (1:n.y)){
      y[[iy]] <- yvalues[ytype==l.ytype[iy],]
      names(y[[iy]])[3] <- observationName[iy]
    }
    #     yvalues$ytype <- observationName[S[[iytype]][iobs1]]
  } else {
    y <- yvalues
    names(y)[3] <- observationName
  }
  datas$observation = y
  
  ##************************************************************************
  #       REGRESSOR FIELD
  #**************************************************************************
  
  if (nx>0){
    datas$regressor = data.frame(id=idnum[iobs1], time=t[iobs1], S[ix][iobs1,])
  }
  
  ##************************************************************************
  #       OCCASION FIELD
  #**************************************************************************
  
  if (nocc>0){
    ov <-data.frame(id=idnum, time=t, S[iocc])
    oo=ov[-2]
    u <- unique(oo)
    io=match(data.frame(t(u)), data.frame(t(oo)))
    datas$occasion <- ov[io,]
  }
  
  
  ##************************************************************************
  #       COVARIATE FIELD
  #*************************************************************************
  
  nc = length(icov) + length(icat)
  if (nc>0) {
    ic <- c(icov,icat)
    cdf <- data.frame(id=seq(1:N))
    for (k in (1:nc))
      cdf[[k+1]] <- S[[ic[k]]][iu]
    names(cdf)[2:(nc+1)]=names(S)[ic]    
    datas$covariate = cdf
  }
  
  
  datas$N <- N
  datas$idOri <- iduf
  
  Sys.setenv(LIXOFT_HOME="")
  Sys.setenv('PATH'=myOldENVPATH);
  return(datas)
=======
#' @export
readdatamlx  <- function(infoProject=NULL, project=NULL){
  # READDATAMLX
  #
  # READDATAMLX reads a datafile and create a list.
     
  if (!is.null(project))
    infoProject <- getInfoXml(project)
  
  header          = infoProject$dataheader
  observationName = infoProject$output
  datafile        = infoProject$datafile
  headerList      = c('ID','TIME','AMT','ADM','RATE','TINF','Y','YTYPE','X','COV','CAT','OCC')
  newList         = c('id','time','amount','type','rate','tinf','y','ytype','x','cov','cat','occ')
  # newList         = cellfun(@lower,headerList,'UniformOutput',false)
  newHeader       = vector(length=length(header))
  ixdose = NULL
  datas=NULL
  
  header=unlist(strsplit(header, ","))  
  nlabel = length(header)
  
  icov <- icat <- iid <- iamt <- iy <- iytype <- ix <- iocc <- NULL
  
  for (i in 1:length(headerList))
  {
    hi=headerList[[i]]
    ih <- which(header %in% hi)
    if (length(ih)==0)
      ih=NULL
    ii <- paste0( 'i', tolower(hi) )
    eval(parse(text= paste0(ii,"=ih")))
    if (!is.null(ih))
      newHeader[ih]=newList[i]      
  }
  
  ##************************************************************************
  #       Gestion du format du fichier de donnees
  #*************************************************************************  
  fileFormat=NULL
  if (is.list(infoProject))
  {
    if (isfield(infoProject, 'dataformat'))
      fileFormat = infoProject$dataformat      
  }
  if (is.null(fileFormat))
  {  
    tmp=unlist(strsplit(datafile, "\\."))
    e= tmp[[length(tmp)]]
    if (e=='csv'){
      fileFormat="csv"
    }else{
      fileFormat="space"
    }
  }
  if (tolower(fileFormat)=="csv"){
    h1 = read.table(datafile, sep=",", nrows=1)
    h2 = read.table(datafile, sep=";", nrows=1)
    if (length(h1)>length(h2)) 
      delimiter=','
    else 
      delimiter=';'
    
  }else if (tolower(fileFormat)=="space"){
    delimiter=""
  }else if (tolower(fileFormat)==" "){
    delimiter=""
  }else if (tolower(fileFormat)=="tab"){
    delimiter='\t'
  }else if (tolower(fileFormat)==";"){
    delimiter=';'
  }else if (tolower(fileFormat)=="\t"){
    delimiter='\t'
  }else
    delimiter=','
  
  
  data    = read.table(datafile, comment.char="", header = TRUE, sep=delimiter)
  S       = data
  S0      = names(data)
  i.new <- c(icov,icat,ix,iocc)
  newHeader[i.new] = S0[i.new]
  
  ans    = funique(S[[iid]])
  iduf   = ans$arg1
  iuf    = ans$arg2
  idnumf = ans$arg3
  
  ans = fsort(iuf)
  ia  = ans$arg1
  ib  = ans$arg2
  
  ans = fsort(ib)
  ic  = ans$arg2
  ids = iduf[ib]
  
  iu    = ia
  idnum = ic[idnumf]
  N     = length(iduf)
  
  #   iop_id = 0
  #   i      = 1
  #   while(iop_id==0 && i<=N) {
  #     iop_id = (i==ids[[i]])
  #     i      = i+1
  #   }
  
  if (is.null(itime)) {
    itime=ix[1]
    ix=ix[2:length(ix)]
  }
  t=S[[itime]]
  nx=length(ix)
  
  nocc <- length(iocc)
  
  if (!is.null(icat)) {
    for (j in (1:length(icat)))
      S[[icat[j]]] <- as.factor(S[[icat[j]]])    
  }
  ##************************************************************************
  #       SOURCE FIELD
  #**************************************************************************
  if (!is.null(iamt)) {
    if (is.null(irate))
      irate = NULL
    if (is.null(iadm))
      iadm = NULL
    
    ixdose=rbind(c(iamt, irate, iadm))
  }
  if (!is.null(ixdose)) {
    i1       = findstrcmp(S[[ixdose[1]]],'.', not=TRUE)
    ndose    = length(i1) #match(FALSE,(!(is.na(ixdose))))
    nxdose   = length(ixdose)
    
    uv       = cbind(idnum[i1], t[i1], matrix(data=0,nrow=ndose,ncol=length(ixdose)))
    for (i in 1:ndose) {
      for(j in 1:nxdose)
        uv[i,2+j]=as.numeric(as.character(S[[ixdose[j]]][i1[i]]))
    }
    
    uh = c('id',newHeader[[itime]])
    for(j in 1:nxdose)
      uh=c(uh,newHeader[[ixdose[j]]])
    u        = list(label  = 'source',
                    name   = 'doseRegimen',
                    colNames = uh,
                    value=NULL)
    u$value =   uv
    #u      = {u}
    datas   = list(sources = u)
  }
  
  ##************************************************************************
  #       OBSERVATION FIELD
  #**************************************************************************
  
  iobs1   = findstrcmp(S[[iy]],'.', not=TRUE)
  yvalues = data.frame(id=idnum[iobs1], time=t[iobs1], y=S[iobs1,iy])
  if (!is.null(iytype)){ 
    ytype <- factor(S[iobs1,iytype])
    l.ytype <- levels(ytype)
    n.y <- length(observationName)
    y<- list()
    for (iy in (1:n.y)){
      y[[iy]] <- yvalues[ytype==l.ytype[iy],]
      names(y[[iy]])[3] <- observationName[iy]
    }
    #     yvalues$ytype <- observationName[S[[iytype]][iobs1]]
  } else {
    y <- yvalues
    names(y)[3] <- observationName
  }
  datas$observation = y
  
  ##************************************************************************
  #       REGRESSOR FIELD
  #**************************************************************************
  
  if (nx>0){
    datas$regressor = data.frame(id=idnum[iobs1], time=t[iobs1], S[ix][iobs1,])
  }
  
  ##************************************************************************
  #       OCCASION FIELD
  #**************************************************************************
  
  if (nocc>0){
    ov <-data.frame(id=idnum, time=t, S[iocc])
    oo=ov[-2]
    u <- unique(oo)
    io=match(data.frame(t(u)), data.frame(t(oo)))
    datas$occasion <- ov[io,]
  }
  
  
  ##************************************************************************
  #       COVARIATE FIELD
  #*************************************************************************
  
  nc = length(icov) + length(icat)
  if (nc>0) {
    ic <- c(icov,icat)
    cdf <- data.frame(id=seq(1:N))
    for (k in (1:nc))
      cdf[[k+1]] <- S[[ic[k]]][iu]
    names(cdf)[2:(nc+1)]=names(S)[ic]    
    datas$covariate = cdf
  }
  
  Sys.setenv(LIXOFT_HOME="")
  Sys.setenv('PATH'=myOldENVPATH);
  
  datas$N <- N
  datas$idOri <- iduf
  return(datas)
}