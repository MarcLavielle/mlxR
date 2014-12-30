readdatamlx  <- function(infoProject)
{
  # READDATAMLX
  #
  # READDATAMLX reads a datafile and create a list.
  
  header          = infoProject$dataheader
  observationName = infoProject$output
  datafile        = infoProject$datafile
  headerList      = c('ID','TIME','AMT','ADM','RATE','TINF','Y','YTYPE','X','COV','CAT')
  newList         = c('id','time','amount','type','rate','tinf','y','ytype','x','cov','cat')
  # newList         = cellfun(@lower,headerList,'UniformOutput',false)
  newHeader       = list()
  ixdose = NULL
  datas=NULL
  
  header=unlist(strsplit(header, ","))  
  nlabel = length(header)
  for (i in 1:length(headerList))
  {
    hi=headerList[[i]]
    ih <- which(header %in% hi)
    if (length(ih)==0)
      ih=NULL
    eval(parse(text= paste0("ii='i",tolower(hi),"'")))
     eval(parse(text= paste0(ii,"=ih")))
    if (!is.null(ih))
      newHeader[ih]=newList[i]      
  }
#  newHeader = unlist(newHeader)
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
    if (length(h1)>length(h2)) {
    delimiter=','
    } else {
      delimiter=';'
    }
  }else if (tolower(fileFormat)=="space"){
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
  newHeader[icov] = S0[icov]
  newHeader[icat] = S0[icat]
  
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
  
  iop_id = 0
  i      = 1
  while(iop_id==0 && i<=N)
  {
    iop_id = (i==ids[[i]])
    i      = i+1
  }
  
  if (is.null(itime))
  {# cas pas encore testé
    itime=ix[1]
    ix=ix[2:length(ix)]
  }
  t=S[[itime]]
  if (is.null(ix))
  {
    nx=0
  }else
  {
    nx=length(ix)
  }
  
  ##************************************************************************
  #       SOURCE FIELD
  #**************************************************************************
  if (!is.null(iamt))
  {
    if (is.null(irate))
      irate = NULL
    if (is.null(iadm))
      iadm = NULL
    
    ixdose=rbind(c(iamt, irate, iadm))
  }
  if (!is.null(ixdose))
  {
    i1       = findstrcmp(S[[ixdose[1]]],'.', not=TRUE)
    ndose    = length(i1) #match(FALSE,(!(is.na(ixdose))))
    nxdose   = length(ixdose)
    
    uv       = cbind(idnum[i1], t[i1], matrix(data=0,nrow=ndose,ncol=length(ixdose)))
    for (i in 1:ndose)
    {
      for(j in 1:nxdose)
      {
        uv[i,2+j]=as.numeric(as.character(S[[ixdose[j]]][i1[i]]))
      }
    }

    uh = c('id',newHeader[[itime]])
    for(j in 1:nxdose){
      uh=c(uh,newHeader[[ixdose[j]]])
    }
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
  nobs    = length(iobs1)
  yvalues = cbind(idnum[iobs1], t[iobs1], matrix(data=0,nrow=nobs,ncol=nx))
  ytype   = rep(1,nobs)
  j <- 1
  while (j <= nx)
  {
    for (i in 1:nobs)
    {
      yvalues[i,2+j]=S[[ix[j]]][iobs1[i]]
    }
    j=j+1
  }
  if (!is.null(iytype)){ 
    Schar <- as.character(S[[iytype]])
    ytype <- as.numeric(Schar[iobs1])
  }
  ans   = funique(ytype)
  yu    = ans$arg1
  #ytype = ans$arg3
  nyu   = length(yu)
  
  observation = list(name= observationName)
  nyu <- min(length(observation$name),nyu)
  #y=cell(1,nyu)
  y = list(list(label=NULL,name=NULL,colNames=NULL, value=NULL))
  length(y)= nyu
  # if (is.na(ix))
  #     ix=NULL
  for (k in 1:nyu)
  {
    y[[k]]$label  ='observation'
    ik            = grep(k,ytype)
    yvk           = yvalues[ik,]
    y[[k]]$name   = observation$name[[k]]
    if (is.null(ix))
    {
      y[[k]]$colNames = c('id',newHeader[[itime]])
    }else
    {
      y[[k]]$colNames = c('id',newHeader[[itime]],newHeader[[ix]])
    }
    y[[k]]$value  = yvk
  }
  #datas$observation=y
  datas   = c(datas, list(observation = y))
  
  ##************************************************************************
  #       COVARIATE FIELD
  #*************************************************************************
  
  if (is.null(icov))
  {
    ncov=0
  }else
  {
    ncov = length(icov)
  }
  if (is.null(icat))
  {
    ncat=0
  }else
  {
    ncat = length(icov)
  }
  nc   = ncov+ncat
  if (nc>0)
  {
    c         = list()
    length(c) = nc
    ans       = fsort(c(icov, icat))
    isc       = ans$arg2
    iscov     = isc[1:ncov]
    iscat     = isc[(ncov+1):nc]
    for( k in 1:ncov ) 
    {
      ic            = iscov[k]
      c[[ic]]$name  = newHeader[[icov[k]]]
      c[[ic]]$value = cbind((1:N), S[[icov[k]]][iu])
    }
    k = 1
    while(k<=ncat)
    {
      ic            = iscat[k]
      ans           = funique(S[[icat[k]]][iu])
      cc            = ans$arg3
      c[[ic]]$name  = newHeader[[icat[k]]]
      c[[ic]]$value = cbind((1:N), cc)
      k = k+1
    }
    for( k in 1:nc)
    {
      c[[k]]$label  = 'covariate'
      c[[k]]$colNames = c('id', c[[k]]$name)
      #c[[k]]        = orderfields(c[[k]],c('name','label','header','value'))
    }
    
    datas   = c(datas, list(covariate = c))
  }
  return(datas)
}