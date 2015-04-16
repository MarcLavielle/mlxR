#' @importFrom XML xmlParse
#' @importFrom XML getNodeSet
#' @importFrom XML xmlAttrs
NULL

processing_monolix  <- function(project,model,treatment,param,output,group)
{
  ### processing_monolix
  #
  #  processing_monolix(project,model, admin, param, output )
  #     takes a monolix project and extract information from
  #     mlxtran file such as model, admin, param, output, if
  #     they missed in the input parameters. Theses informations are read in
  #     files.
  
  ##************************************************************************
  #       XML FILENUL
  #*************************************************************************
  infoProject <- getInfoXml(project)
  
  ##************************************************************************
  #       DATA FILE
  #**************************************************************************
  # datas <- readdatamlx(infoProject)
  
  # Read file with  c++  code called by mlxDataReader
  colTypes <- strsplit(infoProject$dataheader, ",")
  argList <- list(TXT_FILE=infoProject$datafile, COL_TYPES=colTypes[[1]])
  datas2 <- .Call( "mlxDataReaderR", argList, PACKAGE = "mlxDataReaderR");
  
  # set the storage of datas2 into the  format of datas 
  obsi = 0
  covi = 0
  idsources = 0
  idcovariate = 0
  idobservation <- c()
  idcovariate <-c()
  for(i in 1:(length(datas2)-1))
  {
    if(datas2[[i]]$label == "dose")
    {
      idsources = i
    }
    if(datas2[[i]]$label == "longitudinal")
    {
      obsi = obsi+1
      idobservation<- c(idobservation,i)      
    }
    if(datas2[[i]]$label == "covariate")
    {
      covi = covi +1
      idcovariate <-c(idcovariate,i)
    }
    
  }
  
  if(idsources)
  {
    sources<-list(label="source",name="doseRegimen", colNames=datas2[[idsources]]$colTypes,
                  value=matrix(unlist(datas2[[idsources]]$values),nrow=length(datas2[[idsources]]$values),byrow = TRUE))  
  } else
  {
    sources =NULL
  }
  observation <-c()
  for(i in  1:obsi)
  {
    obsvalue=matrix(unlist(datas2[[idobservation[i]]]$values),nrow=length(datas2[[idobservation[i]]]$values),byrow = TRUE)
    observation<- c(observation,list(list( label="observation", name=infoProject$output[i],colNames=c("id", "time"),
                                           value=obsvalue[,1:2])))
  }
  
  datas <-list(sources=sources,observation=observation)
  
  if(covi)
  {
    covariate <-c()
    for(i in  1:covi)
    {
      covariate<-c(covariate,list(list(name=datas2[[idcovariate[i]]]$colNames[2:length(datas2[[idcovariate[i]]]$colNames)],
                                       value=matrix(unlist(datas2[[idcovariate[i]]]$values),nrow=length(datas2[[idcovariate[i]]]$values),byrow = TRUE),
                                       label=datas2[[idcovariate[i]]]$label, colNames=c("id",datas2[[idcovariate[i]]]$colNames[2:length(datas2[[idcovariate[i]]]$colNames)]) )))
    }
    datas <- append(datas,list(covariate=covariate))
  }
  if (!is.null(group)){
    if ((length(names(group[[1]]))>1) | (is.null(group[[1]]$size)))
      stop("When simulx is used with a monolix project, 'group' should be a list with only one field 'size'")
    datas <- resample.data(datas,group[[1]]$size)
  }
  
  ##************************************************************************
  #       treatment (TREATMENT)
  #**************************************************************************
  if (is.null(treatment)){
    treatment = data.frame(datas$sources$value)
    names(treatment) <- datas$sources$colNames 
  }
  ##************************************************************************
  #       PARAMETERS
  #**************************************************************************
  ind_param = datas$covariate;
  nb_param  = length(ind_param);
  paramp    = ind_param;
  pop_param = readPopEstimate(file.path(infoProject$resultFolder,'estimates.txt'));
  paramp[[nb_param+1]] = pop_param;
  iop_indiv=0;
  if (is.null(param))
  {
    param=paramp;
  }else if (is.character(param))  {
    file      = file.path(infoProject$resultFolder,'indiv_parameters.txt') 
    param     = readIndEstimate(file,param);
    iop_indiv = 1;
  }else 
  {
    param <- formatp(param)
    param <- mergeArg(paramp,param)   
  }
  
  ##************************************************************************
  #       OUTPUT 
  #**************************************************************************
  
  outputp = datas$observation;
  if (is.null(output)){
    output = outputp;
  }else{
    output <- formato(output)
    output <- mergeArg(outputp,output)
  }
  
  ##************************************************************************
  #       MODEL
  #**************************************************************************
  if (is.null(model))
  {
    # generate model from mlxtran file  
    mlxtranfile = file_path_sans_ext(basename(project))
    mlxtranpath <- dirname(project)
    model = file.path(mlxtranpath,paste0(mlxtranfile,"_model.txt"))
    session<-Sys.getenv("session.simulx")
    zz=file.path(session,'lib','lixoftLanguageTranslator')
    str=paste0('"',zz,'" --from=mlxproject --to=mlxtran')  
    str=paste0(str,' --output-file=',model,' --input-file=',project,' --option=with-observation-model') 
    system(str, wait=T)
    pathLib = Sys.getenv("LIXOFT_HOME")
    if (iop_indiv==1) {      
      # create a submodel file of model_file corresponding to the specified sections specified 
      sections       = c("LONGITUDINAL")    
      myparseModel(model, sections, model )
      inputList      = getInputSection(model, sections)
      var_m          = setdiff(inputList, param$name)  
      name  = NULL
      value = NULL
      for (i in 1:length(var_m))  {
        idx   = grep(var_m[[i]], pop_param$name)
        name  = c(name, pop_param$name[[idx]])
        value = c(value, pop_param$value[[idx]])  
      }  
      param  = list(param, list(name=name, value=value))    
    }
  }
  #**************************************************************************
  test.colNames <- testC(list(treatment,param,output))
  if (test.colNames==TRUE){
    group=NULL
  }else{
    group=NULL
    # TODO BUG  !!!!!!!! *************************
    #group <- list(size=c(group$size, 1) , level=c("individual","longitudinal"))
  }
  ans = list(model=model, treatment=treatment, param=param, output=output, group=group)
  return(ans)
}

getInfoXml  <- function (project)
{
  ### getInfoXml
  #
  #   getInfoXml(project)
  #     return infoProject which contains informations about the given mlxtran project
  #
  #   infoProject
  #   $datafile
  #   [1] "./warfarin_data.txt"
  #   $dataformat
  #   [1] "tab"
  #   $dataheader
  #   [1] "ID,TIME,AMT,Y,YTYPE,COV,IGNORE,IGNORE"  
  #   $output
  #   [1] "conc" "pca" 
  #   $resultFolder
  #   [1] "./warf2_project"
  
  
  infoProject = list(datafile=NULL, dataformat=NULL, dataheader=NULL, output=NULL, resultFolder=NULL, mlxtranpath=NULL );
  
  # get path and name of monolix project
  mlxtranpath      = dirname(project);
  mlxtranpathfile = file_path_sans_ext(project)
  mlxtranfile = file_path_sans_ext(basename(project))
  infoProject$mlxtranpath = mlxtranpath
  if(file_ext(project) == "mlxtran")
  {
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
  infoProject$dataheader  = infoData[[1]]$headers
  ##************************************************************************
  #       GET OUTPUT INFO
  #*************************************************************************
  #   Exemple : 
  #
  #   infoProject = 
  #           output           : {'conc'  'pca'}
  infoOutput         = myparseXML(xmlfile, mlxtranpath, 'observationModel')
  
  for (k in 1:length(infoOutput)){
    infoProject$output[[k]] = infoOutput[[k]]$name;
  }
  if(file_ext(project) == "mlxtran")
  {unlink(xmlfile, recursive=T)}
  return(infoProject)
}

myparseXML  <- function (filename, mlxtranpath, node)
{
  ### myparseXML
  #
  #  myparseXML(filename, node)
  #     return information of mentioned node contained in the xmlx file (filename)
  #
  
  tree    = xmlParse(filename)
  set            = getNodeSet(tree,paste0("//", node))
  tmp=list(name=NULL)
  ans=list()
  for (i in 1 : length(set))
  {
    attributs      = xmlAttrs(set[[i]])
    namesAttributs = names(attributs)
    tmp['name']= node
    for (j in 1 : length(namesAttributs))
    {      
      tmp[namesAttributs[[j]]]=attributs[[j]]
      # replace '%MLXPROJECT%' by the symbol of current folder "."
      if (namesAttributs[[j]] == "uri")
      {
        tmp[namesAttributs[[j]]]=sub("%MLXPROJECT%", mlxtranpath, tmp[namesAttributs[[j]]]) 
      }
      # repalce '\\t' by "tab"
      if (namesAttributs[[j]] == "columnDelimiter")
      {
        if (tmp[namesAttributs[[j]]] == '\\t' )
          tmp[namesAttributs[[j]]]= "tab"
      }
    } 
    ans= c(ans, list(tmp))
  }
  return(ans)
}

readPopEstimate  <-  function(filename)
{
  #readPopEstimate 
  #
  #   readPopEstimate(filename)
  #       get the population parameter situated in filename file    
  #
  if (file.exists(filename))
  {
    data        = read.table(filename, header = TRUE, sep=";")
    name        = as.character(data[[1]])
    name        = sub(" +", "", name)
    name        = sub(" +$", "", name)
    value       = as.numeric(as.character(data[[2]]))
    
    ic <- grep("corr",name)
    for (j in ic){
      nj <- name[j]
      i1 <- regexpr(",",nj)
      i2 <- regexpr(")",nj)
      n1 <- substr(nj,6,i1[1]-1)
      n2 <- substr(nj,i1[1]+1,i2[1]-1)
      #      nc <-sort(c(n1,n2))
      name[j] <- paste0('r_',n1,'_',n2)
    }
    param       = list(name  = name,
                       value = value)
    return(param)
  }else
  {
    stop(paste("file : ",filename, " does not exist" ))
  }  
}

readIndEstimate  <-  function(filename, estim=NULL)
{
  #readIndEstimate 
  #
  #  readIndEstimate(filename,estim)
  #       get the individual parameter situated in filename file    
  # 
  if (file.exists(filename))
  {
    data         = read.table(filename,  header = TRUE)
    data[[1]]    = c(1: length(data[[1]]))
    header       = names(data)
    idx          = grep(paste0("_", estim), header)
    name         = header[idx]
    name         = gsub(paste0("_", estim),"", name)
    header       = c( 'id', name)
    value        = data[, c(1, idx)];
    names(value) = header;
    value        = as.matrix(data[, c(1, idx)])
    #value       = data.matrix(data[, c(1, idx)])
    if(!(is.null(estim)))
    { 
      param        = list( value= value, name=name, colNames=header,label=estim)
    }else{
      param        = list( value= value, name=name, colNames=header)
    }
    return(param)
  }else
  {
    stop(paste("file : ",filename, " does not exist" ))
  }
}


mergeArg  <- function(p1,p2)
{
  #mergeArg  
  #
  #    mergeArg(p1,p2)
  
  if (!(is.list(p1[[1]])))
    p1 = list(p1)
  if (!(is.list(p2[[1]])))
    p2 = list(p2)
  
  n1 = length(p1);
  n2 = length(p2);
  p  = p1;
  np = length(p);
  for (i in 1:n2)
  {
    p2i=p2[[i]];
    if (isfield(p2i,'colNames'))
    {
      testi  = 0;
      namei2 = p2i$name;
      for (j in 1:n1) 
      {
        p1j = p1[[j]];
        if (isfield(p1j,'colNames'))
        {
          if (namei2==p1j$name)
          {
            p[[j]] = p2i
            testi  = 1
          }
        }else
        {
          #ifs = find(strcmp(p1j$name,namei2));
          #ifs = grep(namei2,p1j$name)
          ifs = match(namei2,p1j$name)
          #   if (length(ifs)>0)
          if(!is.na(ifs))
          {
            p[[j]]$name  = p[[j]]$name[-ifs]
            p[[j]]$value = p[[j]]$value[-ifs]
            np                = np+1
            p[[np]]           = p2i
            testi             = 1
          }
        }
      }
      if (testi==0)
      {
        np      = np+1
        p[[np]] = p2i
      }
    }else
    { 
      if (length(p2i$name)>0){
        for (k in 1:length(p2i$name))
        {
          namek2 = p2i$name[k];
          testk  = 0;
          for (j in 1:n1)
          {
            p1i = p1[[j]];
            if (isfield(p1i,'colNames'))
            {
              if (namek2==p1i$name)
              {
                p[[j]] =list(name= list(namek2));
                if (isfield(p2i,'value'))
                {
                  p[[j]]$value=p2i$value[k];
                }
                if( isfield(p2i,'time'))
                {
                  p[[j]]$time=p2i$time;
                  #                p[[j]]$time=p2i$time[k];
                }
                testk = 1
              }
            }else
            {
              #ifs=grep(namek2,p1i$name);
              ifs=match(namek2,p1i$name);
              if (length(ifs)>0)
              {
                p[[j]]$value[ifs] = p2i$value[k];
                testk             = 1;
              }
            }
          }
          if (testk==0)
          {
            np = np+1;
            p[[np]] =list(name= list(namek2))
            if (isfield(p2i,'value'))
            {
              p[[np]]$value=p2i$value[k];
            }
            if (isfield(p2i,'time'))
            {
              p[[np]]$time=p2i$time;
            }
          }
        }
      }
    }
  }
  return(p)
}

myparseModel  <-  function(model_file, sections, submodel_file)
{
  #   myparseModel create a submodel_file corresponding to the specified sections of model_file
  #
  #    myparseModel(model_file, sections, submodel_file)
  #       myparseModel create a submodel file corresponding to the specified sections of model_file
  #
  #       The specified sections of a model (model_file) are written in a new file (submodel_file) 
  #       In case of multiple sections, each section are concatenated in a single model. 
  #
  #       sections :  a list of string containing the name of the sections we want to write into a new file. 
  #                   could be "POPULATION",  "OBSERVATION", "INDIVIDUAL", "COVARIATE"
  #
  #   Examples
  #   --------
  #       model_file    = "home/model.txt"
  #       sections      =  c("COVARIATE", "OBSERVATION")
  #       submodel_file = "home/submodel.txt"
  #       myparseModel(model_file, sections, submodel_file)
  #
  
  #splitModel a model file_model into multiple terms corresponding to the specified sections 
  terms = splitModel( model_file,sections) 
  
  str= ""
  for (i in 1 :  length( terms))
  {
    str= c(str, terms[[i]]$model)
  }  
  write(str,submodel_file)
}

splitModel  <-  function(file_model, sections)
{
  #   splitModel split a model file_model into multiple terms corresponding to the specified sections 
  #
  #   terms = splitModel(file_model, sections)
  #       splitModel split a model file_model into multiple terms corresponding to the specified sections 
  #
  #       The extracted terms are returned in form of a list of strings. 
  #       Each element of terms have two fileds :  name and model  
  #       name corresponds to the name of the section contained in model. 
  #
  #       sections :  a list of string containing the name of the sections we want to use to split the model. 
  #                   could be "POPULATION", "COVARIATE","INDIVIDUAL", "OBSERVATION"
  #   Examples
  #   --------
  #       file_model  = "home/model.txt"
  #       sections    =  c("COVARIATE", "OBSERVATION")
  #       terms       = splitModel(file_model, sections)
  #
  #      > terms[[1]]$name
  #          "COVARIATE"
  #      > terms[[1]]$model
  #          chr [1:9]
  #      > terms[[2]]$name
  #          "OBSERVATION"
  #      > terms[[2]]$model
  #          chr [1:20]
  #
  if (file.exists(file_model))
  {
    terms         = list()
    length(terms) = length(sections)
    for (i in 1 : length(sections))
    {
      sections_i = sections[[i]]
      con        = file(file_model, open = "r")
      lines      = readLines(con)
      close(con)
      
      idx_sections   = grep("[",lines, fixed=TRUE)
      idx_sections   = c(idx_sections,length(lines))
      idx            = grep(sections_i,lines, fixed=TRUE)
      fin_sections   = idx_sections[idx_sections>idx]
      model_temp     = ""
      while (idx < fin_sections[[1]]) {
        model_temp = c(model_temp, lines[idx])
        idx        = idx +1
      }
      terms[[i]]$name = sections_i
      terms[[i]]$model= model_temp
    }
    return(terms)
    
  }else
  {
    stop(paste("file : ",file_model, " does not exist" ))
  }
  
}

getInputSection  <-  function(model_file, section)
{
  #   getInputSection extract the input list corresponding to the specified section of model_file
  #
  #    inputList = getInputSection(model_file, section)
  #       getInputSection extract the input list corresponding to the specified section of model_file
  #
  #
  #       section :  a string containing the name of the section we want to get the input list. 
  #                   could be "POPULATION",  "OBSERVATION", "INDIVIDUAL", "COVARIATE"
  #
  #   Examples
  #   --------
  #       model_file    = "home/model.txt"
  #       section      =  c("INDIVIDUAL")
  #       getInputSection(model_file, section)
  #       inputList
  
  #split a model file_model into multiple terms corresponding to the specified sections 
  subsection  = splitModel( model_file,section) 
  temp        = subsection[[1]]
  # extract the input list of the subsection 
  idx         = grep("input",temp$model, fixed=TRUE)
  inputList   = temp$model[[idx]] 
  
  #   Exemple : 
  #   "input = {V_pop, Cl_pop, omega_V, omega_Cl, beta_V, weight}"
  #
  
  chaine1      = strsplit(inputList,"\\{")
  lc1 <- length(chaine1[[1]])
  chaine1      = chaine1[[1]][lc1]
  chaine2      = strsplit(chaine1,"\\}")
  chaine2      = chaine2[[1]][1]
  #   split en fonction de ","  dans chaine2 = "V_pop, Cl_pop, omega_V, omega_Cl, beta_V, weight"
  chaine3       = strsplit(chaine2,"\\,")
  chaine3       = chaine3[[1]]
  chaine3        = sub(" +", "", chaine3)
  chaine3        = sub(" +$", "", chaine3)
  inputList     = chaine3
  
  
  return(inputList)
}

#---------------------------------------
formatp <- function(param)
{
  if (!is.null(names(param))){  
    param=list(param) 
  }
  parameter <- vector("list",length(param))
  for (k in seq(1,length(param))){
    paramk <- param[[k]]
    if(isfield(paramk,'header')){
      warning("deprecated syntax:  use 'colNames' instead of 'header'",immediate.=TRUE)
      paramk$colNames=paramk$header
      paramk$header=NULL
    } 
    if (!isfield(paramk,'colNames')){
      if (!is.data.frame(paramk)){
        if (!is.list(paramk)){
          paramk <- list(name=names(paramk),value=as.vector(paramk))
        }else{
          #         paramk$colNames=c("id",paramk$name)
          #         paramk$colNames=paramk$name
          #          N <- length(paramk$value)
          #           paramk$value <- cbind((1:N),paramk$value) 
          #         paramk$value <- data.matrix(data.frame(id=(1:N),value=paramk$value))
          #          paramk$value <- data.matrix(data.frame(value=paramk$value))
        }
      }
    }
    parameter[[k]] <- paramk
  }
  return(parameter)
}

#-------------------------------------------
formato <- function(out)
{
  if (!is.null(names(out))){  
    out=list(out) 
  }
  output <- vector("list",length(out))
  for (k in seq(1,length(out))){
    outk <- out[[k]]
    if (!isfield(outk,"name"))
      outk <- list(name=outk)
    
    output[[k]] <- outk
  }
  return(output)
}
#----------------------------------
resample.data  <- function(data,N)
{
  for  (k in (1:length(data))){
    datak <- data[[k]]
    if (!is.null(datak$colNames)){
      ik  <- which(datak$colNames=="id")
      idk <- datak$value[,ik]
      if (!exists('new.id')){
        n <- length(unique(idk))
        new.id <- sample(1:n,N,replace=TRUE)
      }
      dkv=NULL
      for (i in 1:N){
        ji <- which(idk==new.id[i])
        dkji <- datak$value[ji,]
        if (is.null(dim(dkji))){
          dkji[ik] <- i
        }else{
          dkji[,ik] <- i
        }
        dkv <- rbind(dkv,dkji,deparse.level=0) 
      }
      datak$value <- dkv
      data[[k]] <- datak
    }else{
      for  (j in (1:length(datak))){
        datakj <- datak[[j]]
        if (!is.null(datakj$colNames)){
          ik  <- which(datakj$colNames=="id")
          idk <- datakj$value[,ik]
          if (!exists('new.id')){
            n <- length(unique(idk))
            new.id <- sample(1:n,N,replace=TRUE)
          }
          dkv=NULL
          for (i in 1:N){
            ji <- which(idk==new.id[i])
            dkji <- datakj$value[ji,]
            if (is.null(dim(dkji))){
              dkji[ik] <- i
            }else{
              dkji[,ik] <- i
            }
            dkv <- rbind(dkv,dkji,deparse.level=0) 
          }
          datakj$value <- dkv
          data[[k]][[j]] <- datakj
        } 
      }  
    }
  }
  return(data)
}

#----------------------------------
testC  <- function(x)
{
  testC <- FALSE
  d <- length(x)
  for (k in seq(1,d)) {
    xk <- x[[k]]
    dk <- length(xk)
    for (j in seq(1,dk)) {
      if (any( "colNames" %in% names(xk[[j]]) ))
        testC <- TRUE
    }
  }
  return(testC)
}
