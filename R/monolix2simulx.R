#' Convert a Monolix Project  into an executable for the simulator  Simulx 
#' @param project : the name of a Monolix project 
#' @param parameter : string $(NameOfTypeOfParameter), the type of specific parameters to use 
#'                   example: "mode", "mean"...
#' @param group : a list with the number of subjects 
#' @param open : load the R script created if \code{open=TRUE}
#' @param r.data : read the data if \code{r.data=TRUE}
#' @return  creates a folder projectNameR  containing files : 
#' \itemize{
#'   \item \code{projectName.R} :  executable R code for the simulator,
#'   \item \code{treatment.txt} :  contains the treatment informations,
#'   \item \code{populationParameter.txt} : contains the  population parameters estimated from Monolix,
#'   \item \code{individualParameter.txt} : contains the  individual parameters (mode/mean) estimated from Monolix (if used for the simulation),
#'   \item \code{individualCovariate.txt} : contains the individual covariates,
#'   \item \code{originalId.txt} : contains the original id's when group is used with a different size than the original one,
#'   \item \code{outputi.txt} : contains the output number i informations (time, id),
#'   \item \code{$(NameOfTypeOfParameter)s.txt} : contains the specific parameter used.
#' }       
#'  
#' @return A list of data frames. Each data frame is an output of simulx the mlxtran model
#' the data inputs: treatment, parameters, output of monolix, group... 
#' 
#' @export

monolix2simulx <-function(project,parameter=NULL,group=NULL,open=FALSE,r.data=TRUE,fim=NULL)
{ 
  #------- project to be converted into Simulx project
  myOldENVPATH = Sys.getenv('PATH');
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  if  (!is.null(names(group)))
    group <- list(group)
  ans <- processing_monolix(project=project,
                            model=NULL,
                            treatment=NULL,
                            parameter=parameter,
                            output=NULL,
                            group=group,
                            r.data=r.data,
                            fim=fim)
  model         <- ans$model
  treatment     <- ans$treatment
  parameter     <- ans$param
  output        <- ans$output
  group         <- ans$group
  regressor     <- ans$regressor
  occasion      <- ans$occ
  fim           <- ans$fim
  mlxtranpath <- dirname(project)
  mlxtranfile = file_path_sans_ext(basename(project))
  mypath <- getwd()
  Rproject <- file.path(mypath,paste0(mlxtranfile,"_simulx"))
  if(file.exists(Rproject) )
    unlink(Rproject, recursive = TRUE, force = TRUE)
  modelname = basename(model)
  dir.create(Rproject, showWarnings = FALSE, recursive = FALSE, mode = "0777")
  file.copy(model, Rproject, overwrite = FALSE)
  file.remove(model)
  model<-file.path(Rproject,modelname)
  
  #configure and write output 
  RprojectPath <- dirname(model)
  mlxtranfile = file_path_sans_ext(basename(project))
  projectExe <- file.path(RprojectPath,paste0(mlxtranfile,".R"))
  cat(paste0("# File generated automatically on ", Sys.time(),"\n \n"), file =projectExe, fill = FALSE, labels = NULL,append = TRUE)
  cat("library(mlxR)  \n \nsetwd(dirname(parent.frame(2)$ofile)) \n\n# model \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  cat(paste0("model<-\"",modelname,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  # write  treatment 
  if(!(is.null(treatment)) && length(treatment)>0){ 
    if (!is.null(treatment$value)){
      treat2<-matrix(treatment$value,nrow=nrow(treatment$value),ncol=ncol(treatment$value))
      colnames(treat2)<-treatment$colNames
      treatment <- treat2
    }
    write.table(treatment,file=file.path(Rproject,"/treatment.txt"),row.names=FALSE,quote=FALSE)
    cat("\n# treatment\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    cat("trt <- read.table(\"treatment.txt\", header = TRUE) \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  
  # write  parameters   
  if(!(is.null(parameter))){  
    param.list <- NULL
    cat("\n# parameters \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    
    if (!is.null(ans$id)){
      outfile = file.path(Rproject,paste0("/originalId.txt"))      
      write.table(ans$id,file=outfile,row.names=FALSE,quote=FALSE)
      cat(paste0("originalId<- read.table('originalId.txt', header=TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
    }
    
    populationParameter <- parameter[[1]]
    if (!is.null(populationParameter)){
      outfile = file.path(Rproject,paste0("/populationParameter.txt"))      
      cat(paste0("populationParameter<- read.vector('populationParameter.txt') \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      write.table(populationParameter,file=outfile,col.names=FALSE,quote=FALSE)
      ## edit populationParameter.txt, check and  set correct name of error moel,  it can change  in the V2 model
      
      paramfile<-outfile
      setErrorModelName(paramfile,model)
      if (!is.null(param.list))
        param.list <- paste(param.list,"populationParameter",sep=",")  
      else
        param.list <- "populationParameter"
    } 
    
    individualCovariate <- parameter[[2]]
    if (!is.null(individualCovariate)){
      outfile = file.path(Rproject,paste0("/individualCovariate.txt"))      
      cat(paste0("individualCovariate<- read.table('individualCovariate.txt', header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      write.table(individualCovariate,file=outfile,row.names=FALSE,quote=FALSE)
      if (!is.null(param.list))
        param.list <- paste(param.list,"individualCovariate",sep=",")  
      else
        param.list <- "individualCovariate"
    } 
    individualParameter <- parameter[[3]]
    if (!is.null(individualParameter)){
      outfile = file.path(Rproject,paste0("/individualParameter.txt"))      
      cat(paste0("individualParameter<- read.table('individualParameter.txt', header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      write.table(individualParameter,file=outfile,row.names=FALSE,quote=FALSE)
      if (!is.null(param.list))
        param.list <- paste(param.list,"individualParameter",sep=",")  
      else
        param.list <- "individualParameter"
    } 
    
    param.list <- paste(param.list,sep=",")
    param.str <- paste0("list.param <- list(",param.list,")")
    cat(param.str, file =projectExe, fill = FALSE, labels = NULL, append = TRUE)   
  }
  
  # write f.i.m
  if(!(is.null(fim))) 
    write.table(fim,file=file.path(Rproject,"/fim.txt"),row.names=FALSE,quote=FALSE) 
  
  # write  requested output 
  if(!(is.null(output)))
  {  
    cat("\n# output \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    
    if(length(output)==1)
    {
      # many types of output could exist
      cat(paste0("name<-\"",output[[1]]$name,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      cat(paste0("time<-read.table(\"output.txt\",header=TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      cat(paste0("out<-list(name=name,time=time) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      #       out2 <-matrix(output[[1]]$value,nrow=nrow(output[[1]]$value),ncol=ncol(output[[1]]$value))
      #       colnames(out2)<-output[[1]]$colNames
      outfile = file.path(Rproject,"/output.txt")
      write.table(output[[1]]$value,file=outfile,row.names=FALSE,quote=FALSE) 
      #       write.table(out2,file=outfile,row.names=FALSE,quote=FALSE) 
      #cat("out<-list(out)\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    } else {    # many types of output could exist
      for(i in seq(1:length(output))) {
        cat(paste0("name<-\"",output[[i]]$name,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        if(!(is.null(output[[i]]$colNames))) {
          
          cat(paste0("time<-read.table(\"output",i,".txt\",header=TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
          
          cat(paste0("out",i,"<-list(name=name,time=time) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
          
          out2 <-matrix(output[[i]]$value,nrow=nrow(output[[i]]$value),ncol=ncol(output[[i]]$value))
          colnames(out2)<-output[[i]]$colNames
          outfile = file.path(Rproject,paste0("/output",i))
          outfile = paste0(outfile,".txt")
          #           write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
          write.table(output[[i]]$value,file=outfile,row.names=FALSE,quote=FALSE) 
        } else {
          cat(paste0("out",i,"<-list(name=name) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        }
      }
      
      cat("out<-list(out1", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      for(i in seq(2,length(output))) {
        cat(paste0(",out",i), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)   
      }
      cat(")\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    }
  }
  
  # regressor    
  if(!(is.null(regressor))) {  
    cat("\n# regressor \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    outfile = file.path(Rproject,paste0("/regressor.txt"))
    
    #change regressor names and use these defined in the model in the same order 
    namesReg<-names(regressor)
    nbModelreg<-0
    lines <- readLines(model)
    regressorLine <-  grep('regressor', lines, fixed=TRUE, value=TRUE)
    regModelNamesTable<-strsplit(regressorLine,"[\\{ \\} , ]")[[1]]
    regModelNames<-c()
    for( i in seq(1:length(regModelNamesTable))){
      if(!identical(regModelNamesTable[i],"")&&!length(grep("=",regModelNamesTable[i],fixed=TRUE,value=TRUE))
         &&!length(grep("regressor",regModelNamesTable[i],fixed=TRUE,value=TRUE))
      ){
        regModelNames<-c(regModelNames,regModelNamesTable[i])
        nbModelreg = nbModelreg +1
      }
    }
    nbregOrig<-0
    iregModel <-1
    for( i in seq(1:length(namesReg))){
      if(!identical(tolower(namesReg[i]),"id") &&
           !identical(tolower(namesReg[i]),"time")){
        namesReg[i] <- regModelNames[iregModel]
        
        iregModel <-iregModel +1 
        nbregOrig <- nbregOrig +1
      }
    }
    if(nbregOrig +1 !=  iregModel)
    {
      stop("inconsistent number of regressor between model and dregressor Field")
    }
    names(regressor)<-namesReg
    #---------------------------------------------------------------
    
    write.table(regressor,file=outfile,row.names=FALSE,quote=FALSE)
    cat(paste0("regressor <-read.table(\"regressor.txt\", header = TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
  }
  
  # occasion    
  if(!(is.null(occasion))) {  
    cat("\n# occasion \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    outfile = file.path(Rproject,paste0("/occasion.txt"))      
    write.table(occasion,file=outfile,row.names=FALSE,quote=FALSE)
    cat(paste0("occasion <-read.table(\"occasion.txt\", header = TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
  }
  
  # call the simulator
  cat("\n# call the simulator \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  cat("res <- simulx(model=model", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  if(!(is.null(treatment))&& length(treatment)>0)  
    cat(",treatment=trt",file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
  
  if(!(is.null(parameter)))
    cat(",parameter=list.param",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(group))) 
    cat(",group=grp",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(output)))
    cat(",output=out",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(regressor)))
    cat(",regressor=regressor",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(occasion)))
    cat(",varlevel=occasion",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  cat(")\n",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  Sys.setenv('PATH'=myOldENVPATH);
  if( (Sys.getenv("RSTUDIO")=="1") & (open==TRUE) ) {
    file.edit(projectExe) 
    setwd(mypath)
  }
  return(projectExe)
}

## set correct the name of error model in paramfile, it can change  in the V2 model
## if user's parameter is the same as error model name


setErrorModelName<- function(paramfile,model)
{
  modelread <- readLines(model)
  errorline<-grep("errorModel",modelread)
  errorused<-NULL
  for(i in seq(1:length(errorline)))
  {    
    comment<-";"
    line<-strsplit(modelread[errorline[i]],comment)[[1]][1]    
    if(length(line))
    {       
      testerr<-strsplit(line,"errorModel")
      if(length(testerr[[1]])==2)
      {          
        errorsub<-strsplit(testerr[[1]][2],'[/(/)]',perl=TRUE)
        errorargs<-strsplit(errorsub[[1]][2],',')
        for(ee in seq(1:length(errorargs[[1]])))
#           errorused<-c(errorused,trimws(errorargs[[1]][ee]))
        errorused<-c(errorused,(errorargs[[1]][ee]))
      }        
      
    }
  }
  ## todo replace names without _ in paramfile
  
  replaced=FALSE
  endFlag<-"_"
  if(length(errorused))
  {
    paramread <- readLines(paramfile)
    for(i in seq(1:length(errorused)))
    {
      erri<-errorused[i]
      erriChars <- strsplit(erri,"")[[1]]
      lastChar <-  erriChars[length(erriChars)]
      if(identical(lastChar,endFlag))
      {
        errifirstchars<-strsplit(erri,'(.$)',perl=TRUE)[[1]]
        erriline<-paste0("^",erri,"$")
        errparamf<-grep(erriline,paramread,perl=TRUE)
        if(!length(errparamf))
        {
          for(i in seq(1:length(paramread)))
          { 
            paramname<- NULL
            linesplitted<-strsplit(paramread[i],'\\s',perl=TRUE)
            is<-1
            if(length(linesplitted[[1]]))
            {
              for(j in seq(1:length(linesplitted[[1]])))
              {
                if(!identical(linesplitted[[1]][j],""))
                {
                  paramname<-linesplitted[[1]][j]
                  is<-j
                  break
                }
              }
            }            
            if(identical(paramname,errifirstchars))
            {            
              paramread[i]<-sub(linesplitted[[1]][is],erri,paramread[i]) 
              replaced=TRUE 
              break
            }          
          }
        }
      }
    }
  }
  if(replaced==TRUE)
  {
    tmpFile<-paste0(paramfile,"_tmp")
    file.rename(paramfile,tmpFile)
    cat("",file =paramfile, fill = FALSE, labels = NULL, append = FALSE)    
    for(i in seq(1:length(paramread)))
    {
      cat(paste0(paramread[i],"\n"),file =paramfile, fill = FALSE, labels = NULL, append = TRUE)   
    }
    unlink(tmpFile)
  }
}
