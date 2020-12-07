#' @importFrom tools file_path_sans_ext
processing_monolix  <- function(project,model=NULL,treatment=NULL,parameter=NULL,regressor=NULL,
                                output=NULL,group=NULL,r.data=TRUE,fim=NULL,create.model=TRUE, 
                                format.original=FALSE, error.iov=TRUE)
{
  ### processing_monolix
  #     takes a monolix project and extract information from
  #     mlxtran file such as model, admin, param, output, if
  #     they missed in the input parameters. 
  #     Theses informations are read in files.
  
  ##************************************************************************
  #       XML FILENUL
  #*************************************************************************
  if (!file.exists(project)) 
    stop(paste0("The Monolix project ", file.path(getwd(),project), " does not exists..."), call.=FALSE)
  
  if (grepl(" ", project))
    stop("Please, remove the spaces in the project name...", call.=FALSE)
  
  id <- NULL
  id1 <- lapply(treatment,function(x) {if (is.data.frame(x)) as.character(x$id) else NULL})
  id2 <- lapply(parameter,function(x) {if (is.data.frame(x)) as.character(x$id) else NULL})
  id3 <- lapply(regressor,function(x) {if (is.data.frame(x)) as.character(x$id) else NULL})
  id4 <- lapply(output,   function(x) {if (is.data.frame(x)) as.character(x$id) else NULL})
  idt <- c(id1,id2,id3,id4)
  id.input <- unique(unlist(idt))
  #id.input <- NULL
  # for (j in seq_len(length(idt)))
  #   id.input <- c(id.input, idt[[j]])
  # id.input <- as.factor(id.input)
  # id.input <- as.factor(unique(unlist(idt[which(!unlist(lapply(idt, is.null)))])))
  for (k2 in seq_len(length(id2))) {
    if (!is.null(id2[[k2]]) & !identical(id.input, unique(id2[[k2]])))
      stop(paste0("Some id's are missing in 'parameter'"), call.=FALSE)
  }
  for (k3 in seq_len(length(id3))) {
    if (!is.null(id3[[k3]]) & !identical(id.input, unique(id3[[k3]])))
      stop(paste0("Some id's are missing in 'regressor'"), call.=FALSE)
  }
  
  infoProject <- getProjectInformation(project)
  if (is.null(infoProject))
    stop("", call=FALSE)
  
  n.output <- length(infoProject$output)
  param <- parameter
  
  #   infoProject$resultFolder <- "project_simul"
  ##************************************************************************
  #       DATA FILE
  #**************************************************************************
  names.proj <- NULL
  if (r.data==TRUE) {
    #    datas <- readDatamlx(infoProject=infoProject)
    datas <- readDatamlx(project=project, obs.rows=format.original, error.iov=error.iov)
    if (format.original) {
      datao <- readDatamlx(project=project, out.data=TRUE, error.iov=error.iov)
      datao$obsRows <- datas$obsRows
      datas$format.original <- datao
    }
    names.proj <- names(datas$covariate)
    y.attr <- sapply(datas,attr,"type")
    j.long <- which(y.attr=="longitudinal")
    datas$observation=list()
    # y <- list()
    for (iy in (1:length(j.long))) {
      yi <- datas[[j.long[iy]]]
      niy <- names(yi)
      yk <- list(ylabel="observation", colNames=niy, name=names(j.long[iy]), value=yi )
      datas$observation[[iy]] <- yk
    }    
    if (!is.null(datas$treatment)) {
      ntr <- names(datas$treatment)
      datas$sources <- list(ylabel="sources", colNames=ntr, name="doseRegimen", value=datas$treatment )
      datas$treatment <- NULL
    }
    
    if (any(sapply(param,is.character))) 
      datas$parameter <- readIndEstimate(infoProject$resultFolder,param[which(sapply(param,is.character))])
    else if (any(sapply(param,is.data.frame))) {
      datas$parameter <- param[[which(sapply(param,is.data.frame))]]
      
      icat <- which(names(datas$parameter) %in% datas$catNames)
      if (length(icat)>0) {
        for (ijcat in names(datas$parameter)[icat]) {
          datas$parameter[[ijcat]] <- factor(as.character(datas$parameter[[ijcat]]), levels(datas$covariate[[ijcat]]) )
          if (any(is.na(datas$parameter[[ijcat]])))
            stop(paste("Some values of", ijcat, "do not match the levels of this covariate"), call.=FALSE)
        }
      }
      
      n.diff <- setdiff(names(datas$covariate), names(datas$parameter))
      if (length(n.diff)>0) {
        if (is.null(datas$parameter$id))
          datas$covariate <- datas$covariate[unique(c('id', n.diff))]
        else 
          if (length(n.diff)>0 && identical(as.factor(datas$covariate$id),as.factor(datas$parameter$id)))
            datas$covariate <- datas$covariate[c('id', n.diff)]
          else
            datas$covariate <- NULL
      } else {
        datas$covariate <- NULL
      }
      param[[which(sapply(param,is.data.frame))]] <- NULL
    }
    
    if (!is.null(datas$parameter)) {
      if (is.null(datas$parameter$time)  && !is.null(datas$occasion)  && nrow(datas$occasion)==nrow(datas$parameter))
        datas$parameter$time <- datas$occasion$time
      iop_indiv=1
    } else {
      iop_indiv=0
    }
    
    if (length(id.input)>0) {
      if (!is.null(datas$parameter$id)) {
        datas$parameter <- subset(datas$parameter, id %in% id.input)
        if (nrow(datas$parameter) == 0) datas$parameter <- NULL
      }
      if (!is.null(datas$covariate$id)) {
        datas$covariate <- subset(datas$covariate, id %in% id.input)
        if (nrow(datas$covariate) == 0) datas$covariate <- NULL
      }
      if (!is.null(datas$sources$value$id)) {
        datas$sources$value <- subset(datas$sources$value, id %in% id.input)
        if (nrow(datas$sources$value) == 0) datas$sources <- NULL
      }
      if (!is.null(datas$regressor$id)) {
        datas$regressor <- subset(datas$regressor, id %in% id.input)
        if (!is.null(datas$regressor$value) && nrow(datas$regressor$value) == 0) datas$regressor <- NULL
      }
    }
    
    datas$id <- data.frame(newId=seq(1:datas$N),oriId=datas$id)
    if  (length(param)>0) {
      for (k in (1:length(param))) {
        if (isfield(param[[k]],"id")) {
          did <- unique(param[[k]]$id)
          datas$id <- data.frame(newId=did,oriId=did)
        }
      }
    }
    ##*********************************************************************
    #       treatment (TREATMENT)
    #**********************************************************************
    if (is.null(treatment)) {
      if (is.null(datas$sources$value)) { 
        treatment = datas$sources
      } else {
        treatment = data.frame(datas$sources$value)
        names(treatment) <- datas$sources$colNames 
      }
    }
  } else {
    datas <- NULL
    iop_indiv <- 0
  }
  
  if (identical(fim,"needed")) {
    if  (file.exists(file.path(infoProject$resultFolder,'correlationEstimates_sa.txt')) |
         file.exists(file.path(infoProject$resultFolder,'FisherInformation','correlationEstimatesSA.txt')))
      fim <- 'sa'
    else if (file.exists(file.path(infoProject$resultFolder,'correlationEstimates_lin.txt')) |
             file.exists(file.path(infoProject$resultFolder,'FisherInformation','correlationEstimatesLin.txt')))
      fim <- "lin"
    else
      fim <- NULL
  }
  
  ##************************************************************************
  #       PARAMETERS
  #**************************************************************************
  
  r <- readPopEstimate(infoProject$resultFolder,fim)
  names.proj <- unique(c(names.proj, unlist(lapply(r,names))))
  names.param <- unique(setdiff(unlist(lapply(parameter,names)),c("id", "time", "occ", "pop")))
  test2 <- !(names.param %in% unique(c(names.proj,gsub("_pop","",names.proj))))
  if (any(test2))
    warning(paste0("Parameter ",names.param[test2]," is not used in the project\n"), call.=FALSE)
  
  pop_param <- r$param
  if (!is.null(datas$covariate.iiv) | !is.null(datas$covariate.iov))
    paramp <- list(pop_param,datas$covariate.iiv,datas$parameter,datas$covariate.iov)
  else
    paramp <- list(pop_param,datas$covariate,datas$parameter)
  
  if (length(param)>0)
    paramp <- mergeDataFrame(paramp, param)
  
  ##************************************************************************
  #       FIM
  #**************************************************************************
  fim <- r$fim
  if (!is.null(fim)) {
    pop_se <- r$se
    fm = readFIM(infoProject$resultFolder, fim)
    
    if (!is.null(fm)){
      ifm <- match(names(fm), names(pop_param))
      f.mat <- matrix(NaN, length(pop_param), length(pop_param))
      f.mat[ifm,ifm] <- as.matrix(fm)
      colnames(f.mat) <- row.names(f.mat) <- names(pop_param)
      if0 <- which(names(f.mat) %in% names(unlist(param)))
      f.mat[if0,] <- f.mat[,if0] <- NaN
      pop_se[if0] <- 0
      fim <- list(mat=f.mat,se=pop_se)
    }
  }  
  
  #   M1 = readFIM(file.path(infoProject$resultFolder,'correlationEstimates_sa.txt'))
  #   M2 = readFIM(file.path(infoProject$resultFolder,'fimTransPop_sa.txt'))
  #   M3 = readFIM(file.path(infoProject$resultFolder,'jacobian.txt'))
  #   
  #   M1 <- as.matrix(M1)
  #   M2 <- as.matrix(M2)
  #   M3 <- as.matrix(M3)
  #   se=r$se
  #   C <- diag(se)%*%M1%*%diag(se)
  #   M3%*%M2%*%M3
  
  ##************************************************************************
  #       OUTPUT 
  #**************************************************************************
  
  outputp = datas$observation
  for (k in seq_len(length(outputp))) {
    outk <- outputp[[k]]
    dnk <- infoProject$fit$data[k]
    mnk <- infoProject$fit$model[k]
    outk[['name']] <- mnk
    outk[['colNames']] <- lapply(outk[['colNames']], function(x) {gsub(dnk,mnk,x)})
    names(outk[['value']]) <- outk[['colNames']]
    outputp[[k]] <- outk
  }
  #  if (!is.null(id.input)) {
  if (length(id.input)>0) {
    for (k in seq_len(length(outputp))) {
      ik <- which(unlist(lapply(outputp[[k]], function(x) {"id" %in% names(x)})))
      if (length(which(!(id.input %in% outputp[[k]][[ik]]$id )))>0  && is.null(output))
        stop(paste0("Some id's defined as inputs do not match with the id's of the original project\n"), call.=FALSE)
      outputp[[k]][[ik]] <- subset(outputp[[k]][[ik]], id %in% id.input)
      # if (nrow(outputp[[k]][[ik]])==0)
      #   outputp[[k]] <- NULL
    }
  }
  if (is.null(output)) {
    output = outputp
  } else {
    output <- formato(output)
    # if (!is.null(outputp))
    output <- mergeArg(outputp,output)
  }
  
  ##************************************************************************
  #       MODEL
  #**************************************************************************
  if (create.model) {
    
    if (is.null(model)) {
      
      # generate model from mlxtran file 
      use.translate <- TRUE
      
      if (!use.translate) {
        
        lines <- myTranslate(project)
        mlxtranfile = file_path_sans_ext(basename(project))
        mlxtranpath <- dirname(project)
        model = file.path(mlxtranpath,paste0(mlxtranfile,"_simulxModel.txt"))
        write(lines,model)
        
      } else if (initMlxR()$status){ # init mlxR package if needed
        
        mlxtranfile = file_path_sans_ext(basename(project))
        mlxtranpath <- dirname(project)
        
        if (.useLixoftConnectors()){ # >= 2019R1
          
          #     model = file.path(normalizePath(mlxtranpath),paste0(mlxtranfile,"_simulxModel.txt"))
          model = paste0(mlxtranfile,"_simulxModel.txt")
          dmlx <- dir(normalizePath(mlxtranpath))
          dmlx0 <- dmlx[grep(paste0(mlxtranfile,"_model"), dmlx)]
          .hiddenCall('lixoftConnectors::writeProjectModelSection(project, model)')
          dmlx <- dir(normalizePath(mlxtranpath))
          dmlx1 <- dmlx[grep(paste0(mlxtranfile,"_model"), dmlx)]
          dmlx01 <- setdiff(dmlx1,dmlx0)
          if (length(dmlx01)>0)
            file.remove(file.path(normalizePath(mlxtranpath),dmlx01))
          
        } else { # !! < 2019R1 ======================================================= !!
          
          session <- Sys.getenv("session.simulx")
          
          model = file.path(mlxtranpath, paste0(mlxtranfile, "_simulxModel.txt"))
          zz = file.path(session,'lib','lixoftLanguageTranslator')
          str = paste0('"',zz,'" --from=mlxproject --to=mlxtran')
          str = paste0(str,' --output-file=',model,' --input-file=',project,' --option=with-observation-model')
          system(str, wait=T)
          
        }
        # !! ========================================================================= !!
        
        transPatch(model)          
        
      }
      
    }
    
    #change regressor names and use these defined in the model in the same order 
    if (!is.null(datas$regressor)) {
      
      namesReg<-names(datas$regressor)
      nbModelreg<-0
      lines <- readLines(model)
      regressorLine <-  grep('regressor', lines, fixed=TRUE, value=TRUE)
      regressorLine <- gsub("\t","",regressorLine)
      if(length(regressorLine)) {
        regModelNames<-c()
        for(line in seq(1:length(regressorLine))) {
          comment<-";"
          lineNoComment<-strsplit(regressorLine[line],comment)[[1]]
          regModelNamesTable<-strsplit(lineNoComment,"[\\{ \\} , =]")[[1]]
          for( i in seq(1:length(regModelNamesTable))) {
            regi <- regModelNamesTable[i]
            if (!identical(regi,"") &&!length(grep("regressor",regi,fixed=TRUE,value=TRUE))
                &&!length(grep("use",regi,fixed=TRUE,value=TRUE))) {
              regModelNames<-c(regModelNames,regi)
              nbModelreg = nbModelreg +1
            }
          }
        }
        nbregOrig<-0
        iregModel <-1
        for( i in seq(1:length(namesReg))) {
          if(!identical(tolower(namesReg[i]),"id") &&
             !identical(tolower(namesReg[i]),"time")) {
            namesReg[i] <- regModelNames[iregModel]
            
            iregModel <-iregModel +1 
            nbregOrig <- nbregOrig +1
          }
        }
        if (nbregOrig+1 != iregModel)
          stop("inconsistent number of regressors between model and regressor field", call.=FALSE)
        
        names(datas$regressor)<-namesReg
      }
      regressorp = datas$regressor
      if (is.null(regressor)) {
        regressor = regressorp
      } else {
        for (kr in (1:length(regressor))) {
          if (is.data.frame(regressor[[kr]])) {
            if (!all(names(regressor[[kr]]) %in% namesReg))
              stop(paste0("Column names of 'regressor' should be ", paste0(namesReg,collapse=', ')), call.=FALSE )
            nrk <- setdiff(names(regressor[[kr]]),c("id","time"))
          } else {
            nrk <- regressor[[kr]]$name
          }
          regressorp[nrk] <- NULL
        }
        if (dim(regressorp)[2]>2)
          regressor <- list(regressorp,regressor)
      }
      
      #---------------------------------------------------------------
    }
    
    ##set correct name of error model in parameter,  it can change  in the V2 model
    paramp[[1]]<-setErrorModelName(paramp[[1]],model)
    ##initialize latent covariates defined in the model but not used,  in parameter
    paramp[[1]]<-initLatentCov(paramp[[1]],model)
    
  } else
    model <- NULL
  
  # gr <- group
  gr    <- mklist(group)
  #   parameter <- mklist(paramp)
  parameter <- paramp
  treatment <- mklist(treatment)
  regressor <- mklist(regressor)
  occ  <- mklist(datas$occ)
  output    <- mklist(output)
  
  ans = list(model=model, 
             treatment=treatment, 
             param=parameter, 
             output=output, 
             group=gr,
             regressor=regressor, 
             id=datas$id,
             occasion=occ,
             fim=fim,
             infoParam=infoProject$parameter,
             catNames=datas$catNames,
             iop_indiv = iop_indiv,
             format.original = datas$format.original)
  
  return(ans)
}

##
readPopEstimate <- function(resultFolder, fim = NULL) {
  
  file.pop <- file.path(resultFolder,'populationParameters.txt')
  if (!file.exists(file.pop)) 
    file.pop <- file.path(resultFolder,'estimates.txt')
  if (!file.exists(file.pop)) 
    stop(("
Sorry but the file with the estimated population parameters does not exist. Are you sure you estimated the population parameters?" ), call.=FALSE)
  
  
  res <- list(param = NULL, se = NULL, fim = fim)
  
  data = lixoft.read.table(file = file.pop, header = TRUE, fill = TRUE)
  if (is.null(data)){
    .warning("Error while reading estimated population parameters file.")
    return(res)
  }
  
  name        = as.character(data[[1]])
  name        = sub(" +", "", name)
  name        = sub(" +$", "", name)
  
  if(is.element("value",names(data))) {
    res$param <- as.numeric(as.character(data[['value']]))
  } else {
    res$param <- as.numeric(as.character(data[['parameter']]))
  }
  names(res$param) <- name
  if (!is.null(fim)) {
    if (fim=='lin') {
      res$se <- as.numeric(as.character(data[['s.e._lin']]))
      if (length(res$se)==0)
        res$se <- as.numeric(as.character(data[['se_lin']]))
      if (length(res$se)==0)
        stop("Fisher Information matrix estimated by linearization is not available", call.=FALSE)
      names(res$se) <- name
    } else if (fim=='sa') {
      res$se <- as.numeric(as.character(data[['s.e._sa']]))
      if (length(res$se)==0)
        res$se <- as.numeric(as.character(data[['se_sa']]))
      if (length(res$se)==0)
        stop("Fisher Information matrix estimated by stochastic approximation is not available", call.=FALSE)
      names(res$se) <- name
    }
  }
  
  return(res)
}

##
readIndEstimate  <-  function(resultFolder, estim = NULL) {
  
  mlx.version <- 2017
  file.ind <- file.path(resultFolder,'IndividualParameters','estimatedIndividualParameters.txt') 
  if (!file.exists(file.ind)) {
    mlx.version <- 2016
    file.ind = file.path(resultFolder,'indiv_parameters.txt') 
  }
  if (!file.exists(file.ind)) 
    stop(("
  Sorry but the file with the individual estimates does not exist. Are you sure you estimated the individual parameters?" ), call.=FALSE)
  
  
  param <- NULL
  
  data <- lixoft.read.table(file = file.ind, header = TRUE)
  if (is.null(data)){
    .warning("Error while reading estimated individual parameters file.")
    return(param)
  }
  
  header       = names(data)
  idx          = grep(paste0("_", estim), header)
  
  if(length(idx)){
    name         = header[idx]
    name         = gsub(paste0("_", estim),"", name)
    headn       = c( 'id', name)
    headn       = gsub("\\.","",headn)
    value        = (as.matrix(data[,  idx]))
    param <- data.frame(id=data[,1], data.frame(value))
    names(param) <- headn
    param$id <- as.factor(param$id)
    #    param[,name] <- as.numeric(param[,name])
  }
  if (is.null(param))
    stop(paste0("
  Sorry but the ",estim," of the conditional distribution has not been computed... Are you sure you computed the individual estimates?" ), call.=FALSE)
  return(param)
}

##
readFIM  <-  function(path, fim){
  
  if (fim=="sa") {
    filename <- file.path(path,'correlationEstimates_sa.txt')
    mlx.version <- 2016
    if (!file.exists(filename)) {
      filename = file.path(path,'FisherInformation','correlationEstimatesSA.txt')
      mlx.version <- 2017
    }
  } else {
    filename <- file.path(path,'correlationEstimates_lin.txt')
    mlx.version <- 2016
    if (!file.exists(filename)) {
      filename = file.path(path,'FisherInformation','correlationEstimatesLin.txt')
      mlx.version <- 2017
    }
  }
  if (file.exists(filename)) {
    data <- lixoft.read.table(file = filename, header = FALSE)
    if (is.null(data)){
      .warning("Error while reading fisher estimation results file.")
      return()
    }
    
    name        = as.character(data[[1]])
    name        = sub(" +", "", name)
    name        = sub(" +$", "", name)
    if (mlx.version==2016) {
      ic <- grep("corr_",name)
      #      name[ic] <- sub("corr_","r_",name[ic])
    }
    data[[1]] <- NULL
    row.names(data) <- name   
    names(data) <- name  
    return(data)
  } else
    stop(paste0("file: ",filename, " does not exist" ), call.=FALSE)
}

mergeDataFrame  <- function(p1,p2) {
  if  (!is.null(names(p2))) 
    p2 <- list(p2)
  for (k in (1:length(p2))) {
    paramk <- p2[[k]]  
    # if (is.list(paramk)) {
    if (is.list(paramk) & !is.data.frame(paramk)) {
      if (is.null(paramk$id)) {
        if (is.vector(paramk$value)) {
          p.temp <- as.vector(paramk$value)
          names(p.temp) <- paramk$name
        } else {
          p.temp <- data.frame(paramk$value)
          names(p.temp) <- paramk$colNames
        }
      } else 
        p.temp <- paramk
      p2[[k]] <- p.temp
    }
  } 
  n1 = length(p1)
  i1 <- which(!unlist(lapply(p1, is.null)))
  n2 = length(p2)
  p.i2 <- NULL
  for (i in 1:n2) {
    p2i=p2[[i]]
    testi  = 0
    namei2 = names(p2i)
    if ("id" %in% namei2) {
      p2i[['id']] <- as.factor(p2i[['id']])
      test.i2 <- TRUE
      for (j in i1) {
        p1j = p1[[j]]
        i12 <- which(namei2 %in% names(p1j))
        if (length(i12)>1) {
          test.i2 <- FALSE
          #          fact1 <- which(sapply(p1j[namei2], class)=="factor")
          fact1 <- which(sapply(p1j[namei2], class)=="factor")
          fact2 <- which(sapply(p2i[namei2], class)=="factor")
          if (!identical(fact1,fact2))
            stop("The parameters defined as input of SIMULX don't have the original types", call.=FALSE)
          p1j=p2i
          p1[[j]] = p1j
        }
      }     
      if (test.i2 == TRUE)
        p.i2 <- c(p.i2, i)
      
    } else {
      for (j in i1) {
        p1j = p1[[j]]
        i12 <- which(namei2 %in% names(p1j))
        namei2=namei2[namei2!="id"]
        if (length(namei2[i12])>0)
          p1j[namei2[i12]]=p2i[namei2[i12]]
        p1[[j]] = p1j
      }
    }
  }
  if (length(p.i2)>0)
    p1 <- c(p1, p2[p.i2])
  return(p1)
}

mergeArg  <- function(p1,p2)
{
  
  if (!(is.list(p1[[1]])))
    p1 = list(p1)
  if (!(is.list(p2[[1]])))
    p2 = list(p2)
  
  n1 = length(p1)
  n2 = length(p2)
  p  = p1
  np = length(p)
  for (i in 1:n2) {
    p2i=p2[[i]]
    if (!is.null(p2i$colNames)) {
      testi  = 0
      namei2 = p2i$name
      for (j in 1:n1) {
        p1j = p1[[j]]
        if (!is.null(p1j$colNames)) {
          if (namei2==p1j$name) {
            p[[j]] = p2i
            testi  = 1
          }
        } else {
          ifs = match(namei2,p1j$name)
          if(!is.na(ifs)) {
            p[[j]]$name  = p[[j]]$name[-ifs]
            p[[j]]$value = p[[j]]$value[-ifs]
            np                = np+1
            p[[np]]           = p2i
            testi             = 1
          }
        }
      }
      if (testi==0){
        np      = np+1
        p[[np]] = p2i
      }
    } else { 
      if (length(p2i$name)>0) {
        for (k in 1:length(p2i$name)) {
          namek2 = p2i$name[k]
          testk  = 0
          for (j in 1:n1) {
            p1i = p1[[j]]
            if (!is.null(p1i$colNames)) {
              if (namek2==p1i$name) {
                p[[j]] =list(name= list(namek2))
                if (!is.null(p2i$value))
                  p[[j]]$value=p2i$value[k]
                if ("time" %in% names(p2i))
                  p[[j]]$time=p2i$time
                testk = 1
              }
            } else {
              ifs=match(namek2,p1i$name)
              if (length(ifs)>0) {
                p[[j]]$value[ifs] = p2i$value[k]
                testk             = 1
              }
            }
          }
          if (testk==0) {
            np = np+1
            p[[np]] =list(name= list(namek2))
            if (!is.null(p2i$value))
              p[[np]]$value=p2i$value[k]
            if (!is.null(p2i$time))
              p[[np]]$time=p2i$time
            if (!is.null(p2i$formula))
              p[[np]]$formula=p2i$formula
          }
        }
      }
    }     
  }
  ik <- NULL
  for (k in (1:length(p))) {
    pk <- p[[k]]
    if (!is.null(pk$time) && pk$time=="none")
      ik <- c(ik,k)
    if (!is.null(pk$value) && nrow(pk$value)==0)
      ik <- c(ik,k)
  }
  p[ik] <- NULL
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
    str= c(str, terms[[i]]$lines)
  
  write(str,submodel_file)
}

splitModel  <-  function(file_model, sections) {
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
  #                   could be xc
  #   Examples
  #   --------
  #       file_model  = "home/model.txt"
  #       sections    =  c("COVARIATE", "LONGITUDINAL")
  #       terms       = splitModel(file_model, sections)
  #
  #      > terms[[1]]$name
  #          "COVARIATE"
  #      > terms[[1]]$model
  #          chr [1:9]
  #      > terms[[2]]$name
  #          "LONGITUDINAL"
  #      > terms[[2]]$model
  #          chr [1:20]
  #
  if (file.exists(file_model)) {
    con        = file(file_model, open = "r")
    lines      = readLines(con, warn=FALSE)
    close(con)
    
    lines <- gsub("\\;.*","",lines)
    lines <- gsub("^\\s+|\\s+$", "", lines)
    lines <- gsub("\\s*=\\s*", "=", lines)
    lines <- gsub("\\s*,\\s*", ",", lines)
    lines <- lines[sapply(lines, nchar) > 0]
    lines <- c(lines,"")
    idx_sections   = grep("[",lines, fixed=TRUE)
    idx_sections   = c(idx_sections,length(lines))
    terms         = list()
    length(terms) = length(sections)
    for (i in 1 : length(sections)) {
      sections_i = sections[[i]]
      idx        = grep(sections_i,lines, fixed=TRUE)
      terms[[i]]$name = sections_i
      if (length(idx)>0) {
        idx_sections_i <- setdiff(idx_sections,idx)
        idx <- idx[1]
        fin_sections   = idx_sections_i[idx_sections_i>idx]
        model_temp     = c()
        while (idx < fin_sections[[1]]) {
          model_temp = c(model_temp, lines[idx])
          idx        = idx +1
        }
        terms[[i]]$lines= strmerge(model_temp)
      } else {
        terms[[i]]$lines= NULL
      }
    }
    return(terms)
    
  } else
    stop(paste0("file: ",file_model, " does not exist" ), call.=FALSE)
  
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
  #                   could be "POPULATION",  "LONGITUDINAL", "INDIVIDUAL", "COVARIATE"
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

sectionsModel  <-  function(file_model)
{
  sections <- c("[POPULATION]", "[COVARIATE]", "[INDIVIDUAL]",  "[LONGITUDINAL]")
  if (file.exists(file_model)) {
    terms   <- NULL
    con     <- file(file_model, open = "r")
    lines   <- readLines(con)
    close(con)
    for (i in 1 : length(sections)) {
      sections_i <- sections[[i]]      
      idx <- grep(sections_i,lines, fixed=TRUE)
      if (length(idx)>0)
        terms <- c(terms,sections_i)
    }
    return(terms)
  } else
    stop(paste0("file: ",file_model, " does not exist" ), call.=FALSE)
}


#-------------------------------------------
formato <- function(out)
{
  if (!is.null(names(out))) 
    out=list(out) 
  
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
testC  <- function(x)
{
  testC <- FALSE
  d <- length(x)
  for (k in seq(1,d)) {
    xk <- x[[k]]
    if (length(xk)>0) {
      if (!is.null(names(xk))) {
        if (any("id" %in% names(xk)))
          testC <- TRUE
      } else {
        dk <- length(xk)
        for (j in seq(1,dk)) {
          if (any( "colNames" %in% names(xk[[j]]) ))
            testC <- TRUE
        }
      }
    }
  }
  return(testC)
}


setErrorModelName<- function(param,model)
{
  ## set correct the name of error model in param, it can change  in the V2 model
  ## if user's parameter is the same as error model name  
  
  modelread <- readLines(model)
  errorline<-grep("errorModel",modelread)
  errorused<-NULL
  for (i in seq(1:length(errorline))) {    
    comment<-";"
    line<-strsplit(modelread[errorline[i]],comment)[[1]][1]    
    if (length(line)) {       
      testerr<-strsplit(line,"errorModel")
      if (length(testerr[[1]])==2) {          
        errorsub<-strsplit(testerr[[1]][2],'[/(/)]',perl=TRUE)
        errorargs<-strsplit(errorsub[[1]][2],',')
        for (ee in seq(1:length(errorargs[[1]])))
          errorused<-c(errorused,(errorargs[[1]][ee]))
      }       
    }
  }
  
  ##replace names without _ in param  
  replaced=FALSE
  endFlag<-"_"
  paramRead <- names(param)
  if(length(errorused)) {    
    for(i in seq(1:length(errorused))) {
      erri<-errorused[i]
      erriChars <- strsplit(erri,"")[[1]]
      lastChar <-  erriChars[length(erriChars)]
      if(identical(lastChar,endFlag)) {
        errifirstchars<-strsplit(erri,'(.$)',perl=TRUE)[[1]]
        erriline<-paste0("^",erri,"$")
        errparamf<-grep(erriline,paramRead,perl=TRUE)
        if(!length(errparamf)) {
          for(i in seq(1:length(paramRead))) { 
            paramname<- NULL
            linesplitted<-strsplit(paramRead[i],'\\s',perl=TRUE)
            is<-1
            if(length(linesplitted[[1]])) {
              for(j in seq(1:length(linesplitted[[1]]))) {
                if(!identical(linesplitted[[1]][j],"")) {
                  paramname<-linesplitted[[1]][j]
                  is<-j
                  break
                }
              }
            }            
            if(identical(paramname,errifirstchars)) {            
              paramRead[i]<-sub(linesplitted[[1]][is],erri,paramRead[i]) 
              replaced=TRUE 
              break
            }          
          }
        }
      }
    }
  }
  names(param)<-paramRead
  return(param)
}

initLatentCov<- function(param,model)
{
  ## initialize latent covariates defined in the model but nit used, 
  ## thus not present in estimates.txt
  
  modelRead <- readLines(model)
  inputLinesNum  <- plcatLine<-grep("input",modelRead)  
  inputRead <- modelRead[inputLinesNum]
  latentCovPrefix <-"plcat"
  plcatLine<-grep(latentCovPrefix,inputRead)
  plcatUsed <-NULL
  if(length(plcatLine)) {
    for(i in seq(1:length(plcatLine))) {    
      comment<-";"
      line<-strsplit(inputRead[plcatLine[i]],comment)[[1]][1]    
      if(length(line)){
        lineSplit<-strsplit(line,'[/{/}," "]',perl=TRUE)[[1]]
        iPlcat<-grep(paste0("^",latentCovPrefix),lineSplit)
        for(ee in seq(1:length(iPlcat)))
          plcatUsed<-c(plcatUsed,(lineSplit[iPlcat[ee]]))            
      }
    }
    
    ## add plcatused in param with 0 as value    
    if(length(plcatUsed)) {   
      namesParam<-names(param)
      plcatInParam<-NULL
      for(iused in seq(1:length(plcatUsed))) {
        for(iparam in seq(1:length(namesParam))) {
          if(identical(plcatUsed[iused],namesParam[iparam])) {
            plcatInParam <-c(plcatInParam,-iused)
            break
          }          
        }
      }
      if(!is.null(plcatInParam))
        plcatUsed <-plcatUsed[plcatInParam]
      
      if(length(plcatUsed)) {
        for(i in seq(1:length(plcatUsed))) {
          namesParam <-c(namesParam,plcatUsed[i])
          param<-c(param,0)
        }
        names(param) <- namesParam
      }
    }    
  }
  return(param)
}

####

myTranslate <- function(project) {
  con        = file(project, open = "r")
  lines      = readLines(con, warn=FALSE)
  close(con)
  i1 <- grep("<MODEL>", lines) + 1
  i2 <- grep("<FIT>", lines) - 1
  lines <- lines[i1:i2]
  lines <- gsub("b, c","b",lines)
  vc <- sub("\\=.*","",lines)
  vc <- gsub(" ","",vc)
  i.file <- which(vc=="file")
  if (length(i.file)>0) {
    l.file <- gsub(" ","",lines[i.file])
    ig <- gregexpr("'",l.file)
    struct.model <- substr(l.file,ig[[1]][1]+1,ig[[1]][2]-1)
    rl <- grep("'lib:",l.file)
    if (length(rl)==0) {
      path.model <- dirname(project)
    } else {
      struct.model <- sub("lib:","",struct.model)
      runtime=Sys.getenv("session.simulx")
      path.dir <- file.path(runtime,"factory/library")
      dir.lib <- dir(path.dir)
      for (lib in dir.lib) {
        path.lib <- file.path(path.dir,lib)
        if (file.exists(file.path(path.lib,struct.model)))
          break
      }
      path.model <- path.lib
    }
    con        = file(file.path(path.model,struct.model), open = "r")
    struct.lines      = readLines(con, warn=FALSE)
    close(con)
    struct.lines <- gsub("\\[LONGITUDINAL\\]", "",struct.lines)
    struct.lines <- sub("\\;.*","",struct.lines)
    i.out <- grep("OUTPUT:",struct.lines)
    if (length(i.out)>0)
      struct.lines <- struct.lines[1:(i.out-1)]
    i.def <- grep("DEFINITION:",struct.lines)
    lines1 <- lines[1:(i.file-1)]
    if (length(lines)>i.file) {
      lines2 <- lines[(i.file+1):length(lines)]
      if (length(i.def)>0)
        lines2 <- gsub("DEFINITION:","",lines2)
      lines <- c(lines1,struct.lines,lines2)
    } else {
      lines <- c(lines1,struct.lines)
    }
  }
  i.des <- grep("DESCRIPTION:", lines)
  if (length(i.des)>0) {
    lines <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lines, perl=TRUE)
    lines <- gsub(" =", "=", lines)
    for (k in (1:length(i.des))) {
      ls <- sapply(c("DEFINITION:", "EQUATION:", "PK:", "LONGITUDINAL", "INDIVIDUAL", "input="), 
                   function(x) {grep(x, lines)} )
      ls <- unlist(ls)
      ik <- min(ls[ls>i.des[k]])
      lines <- lines[-(i.des[k]:(ik-1))]
    }
  }
  return(lines)
}


####

transPatch <- function(model) {
  con        = file(model, open = "r")
  lines      = readLines(con, warn=FALSE)
  close(con)
  gl <- grep("\\[LONGITUDINAL\\]",lines)
  if (length(gl)>1)
    lines <- lines[-gl[2:length(gl)]] 
  write(lines,model)
}


getProjectInformation <- function(project){
  
  if (!initMlxR()$status)
    return(NULL)
  
  projectInfo = list(datafile = NULL, dataformat = NULL, dataheader = NULL, output = NULL, resultFolder = NULL, mlxtranpath = NULL);
  
  
  if (.useLixoftConnectors()) { # >= 2019R1
    
    dataOut <- NULL
    .hiddenCall('dataOut <- lixoftConnectors::getProjectInformation(project)')
    if (is.null(dataOut))
      return(NULL)
  
    # get path and name of monolix project
    mlxtranpath      = dirname(project);
    mlxtranpathfile = file_path_sans_ext(project)
    mlxtranfile = file_path_sans_ext(basename(project))
    projectInfo$mlxtranpath = mlxtranpath
    
    projectInfo$resultFolder = dataOut$resultFolder
    projectInfo$datafile = dataOut$dataFile
    projectInfo$dataformat = dataOut$columnDelimiter
    
    dataHeaders <-paste(toupper(dataOut$headerTypes), collapse =",")
    # regressor in monolixC+++ are now named REG
    dataHeaders <- gsub("REG","X",dataHeaders)
    projectInfo$dataheader =  dataHeaders
    
    for (k in 1:length(dataOut$observationNames))
      projectInfo$output[[k]] = dataOut$observationNames[k]
    
    paramNames <-names(dataOut$individualParameters)
    paramTrans <-as.vector(dataOut$individualParameters)
    paramTrans <- transformationsAlias(paramTrans)
    projectInfo$parameter <- list(name=paramNames, trans=paramTrans)
    if (any(paramTrans=="G")) {
      op0 <- options()
      op1 <- op0
      op1$lixoft_notificationOptions$warnings <- 1
      op1$lixoft_notificationOptions$info <- 1
      options(op1)
      gip <- NULL
      .hiddenCall('lixoftConnectors::initializeLixoftConnectors(software="monolix", force=TRUE)')
      .hiddenCall('lixoftConnectors::loadProject(project)')
      .hiddenCall('gip <- lixoftConnectors::getIndividualParameterModel()')
      projectInfo$parameter$limits <- gip$limits
      .hiddenCall('lixoftConnectors::initializeLixoftConnectors(software="simulx", force=TRUE)')
      options(op0)
    }
    
    
  }
  
  # !! < 2019R1 ====================================================================== !!
  else {
    
    if (requireNamespace("XML", quietly = TRUE)){
      
      session = Sys.getenv("session.simulx")
      Sys.setenv(LIXOFT_HOME = session)
      projectInfo = getInfoXmlFromTranslator(project)
      
    } else
      warning("Impossible to read project information for \"XML\" package is not installed.")
    
  }
  # !! =============================================================================== !!
  
  if (.useLixoftConnectors()){
    lixoftConnectorsVersion <- NULL
    .hiddenCall('lixoftConnectorsVersion <- lixoftConnectors::getLixoftConnectorsState(quietly = TRUE)$version')
    projectInfo$fit = if (lixoftConnectorsVersion >= "2019R2") getFit(dataOut) else getFit_2019R1(project, header = projectInfo$dataheader)
    
  } else
    projectInfo$fit = getFit_2019R1(project, header = projectInfo$dataheader)

  
  return(projectInfo)
  
}

getFit <- function(data){
  
  fit = list(data = data$observationNames, ytype = data$observationMapping)
  fit$model = data$modelMapping
  return(fit)
  
}

nameToAlias <- function(paramVect, name, alias)
{
  params<-paramVect
  for (k in 1:length(params)){
    if(identical(params[k], name)){
      params[k] <- alias
    }
  }
  return(params)
}

transformationsAlias <- function(paramTrans)
{
  params <- nameToAlias(paramTrans, "logNormal", "L")
  params <- nameToAlias(params, "logitNormal", "G")
  params <- nameToAlias(params, "normal", "N")
  params <- nameToAlias(params, "probitNormal", "P")
  params <- nameToAlias(params, "user", "U")
  return(params)
}

# !! RETRO-COMPTATIBILITY - < 2019R1 ================================================= !!
myparseXML <- function (filename, mlxtranpath, node) {
  
  ### myparseXML
  #
  #  myparseXML(filename, node)
  #     return information of mentioned node contained in the xmlx file (filename)
  #
  tree    = XML::xmlParse(filename)
  set     = XML::getNodeSet(tree,paste0("//", node))
  tmp=list(name=NULL)
  ans=list()
  if(length(set)){
    for (i in 1 : length(set)) {
      attributs      = XML::xmlAttrs(set[[i]])
      namesAttributs = names(attributs)
      tmp['name']= node
      for (j in 1 : length(namesAttributs)) {
        tmp[namesAttributs[[j]]]=attributs[[j]]
        # replace '%MLXPROJECT%' by the symbol of current folder "."
        if (namesAttributs[[j]] == "uri")
          tmp[namesAttributs[[j]]]=sub("%MLXPROJECT%", mlxtranpath, tmp[namesAttributs[[j]]])
        # repalce '\\t' by "tab"
        if (namesAttributs[[j]] == "columnDelimiter") {
          if (tmp[namesAttributs[[j]]] == '\\t' )
            tmp[namesAttributs[[j]]]= "tab"
        }
      }
      ans= c(ans, list(tmp))
    }
  }
  
  return(ans)
  
}

getInfoXmlFromTranslator <- function(project){
  
  infoProject = list(datafile = NULL, dataformat = NULL, dataheader = NULL, output = NULL, resultFolder = NULL, mlxtranpath = NULL)
  
  # get path and name of monolix project
  mlxtranpath = dirname(project);
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
# !! ================================================================================= !!

# !! RETRO-COMPTATIBILITY - < 2019R1 ================================================= !!
getFit_2019R1 <- function(project, header) {
  
  con     <- file(project, open = "r")
  lines.complete   <- readLines(con, warn=FALSE)
  close(con)
  lines.complete <- gsub("\\;.*","",lines.complete)
  l.fit <- grep("<FIT>", lines.complete)
  lines <- lines.complete[l.fit:length(lines.complete)]
  l.data <- lines[grep("data", lines)[1]]
  l.model <- lines[grep("model", lines)[1]]
  
  ll <- gsub(" ","",l.data)
  ll <- gsub("data=","",ll)
  if (grepl("\\{",ll)) {
    i1 <- regexpr("\\{",ll)
    i2 <- regexpr("\\}",ll)
    ll <- substr(ll,i1+1,i2-1)
    data.names  <- strsplit(ll,",")[[1]]
  } else {
    data.names <- ll
  }
  ll <- gsub(" ","",l.model)
  ll <- gsub("model=","",ll)
  if (grepl("\\{",ll)) {
    i1 <- regexpr("\\{",ll)
    i2 <- regexpr("\\}",ll)
    ll <- substr(ll,i1+1,i2-1)
    model.names  <- strsplit(ll,",")[[1]]
  } else {
    model.names <- ll
  }
  
  if (grepl("YTYPE",header)) {
    i.obs <- FALSE
    i <- grep("\\[CONTENT\\]", lines.complete)
    while(!i.obs) {
      i <- i+1
      s <- line2field(lines.complete[i])
      if (identical(s$fields$use,"observation"))
        i.obs <- TRUE
    }
    sf <- s$fields
    if (is.null(sf$yname)) sf$yname <- sf$ytype 
    sf$yname <- gsub("'","",sf$yname)
    data.ytype <- sf$yname[unlist(lapply(data.names, function(x) {grep(x,sf$name)}))]
  } else
    data.ytype=data.names
  return(list(data=data.names, model=model.names, ytype=data.ytype))
}


# !! ================================================================================= !!
