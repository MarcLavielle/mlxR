#' Convert a Monolix Project  into an executable for the simulator  Simulx 
#' @param project : the name of a Monolix project 
#' @param parameter : string $(NameOfTypeOfParameter), the type of specific parameters to use 
#'                   example: "mode", "mean"...
#' @return  creates a folder projectNameR  containing files : 
#' \itemize{
#'   \item \code{projectName.R} :  executable R code for the simulator,
#'   \item \code{treatment.txt} :  contains the treatment informations,
#'   \item \code{parameters.txt} : contains the  population parameters ouput from Monolix,
#'   \item \code{group.txt} : contains the group informations,
#'   \item \code{outputi.txt} : contains the output number i informations (time, id),
#'   \item \code{covariates.txt} : contains the covariates parameters,
#'   \item \code{$(NameOfTypeOfParameter)s.txt} : contains the specific parameter used.
#' }       
#'  
#' @return A list of data frames. Each data frame is an output of simulx the mlxtran model
#' the data inputs: treatment, parameters, output of monolix, group... 
#' 
#' @export

#monolix2simulx <-function(project, graphics=FALSE,output=NULL,parameter=NULL)
monolix2simulx <-function(project,parameter=NULL)
{ 
  #------- project to be converted into Simulx project
  myOldENVPATH = Sys.getenv('PATH');
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  model=NULL
  group=NULL
  treatment=NULL  
  #graphics=FALSE
  output=NULL
  regressor=NULL
  ans           <- processing_monolix(project,model,treatment,parameter,output,group)
  model         <- ans$model
  treatment     <- ans$treatment
  parameter     <- ans$param
  output        <- ans$output
  group         <- ans$group
  regressor     <- ans$regressor
  mlxtranpath <- dirname(project)
  mlxtranfile = file_path_sans_ext(basename(project))
  mypath <- getwd()
  Rproject <- file.path(mypath,paste0(mlxtranfile,"_simulx"))
  if(file.exists(Rproject) )
  {
    unlink(Rproject, recursive = TRUE, force = TRUE)
  }
  modelname = basename(model)
  dir.create(Rproject, showWarnings = TRUE, recursive = FALSE, mode = "0777")
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
  if(!(is.null(treatment))){ 
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
  if(!(is.null(parameter)))
  {  
    cat("\n# parameters \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    # many types of output could exist
    nameOtherParam<-NULL
    for(i in seq(1:length(parameter)))
    {  
      if(!(is.null(parameter[[i]]$colNames)))
      {
        if(!(is.null(parameter[[i]]$label)))
        {
          namePi<-parameter[[i]]$label
        }else {
          namePi<-paste0("parameters",i)
        }
        
        nameOtherParam<-c(nameOtherParam,namePi)
        outfile = file.path(Rproject,paste0("/",namePi,".txt"))      
        cat(paste0(namePi,"<- read.table(\"",namePi,".txt\", header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
        out2<-NULL
        out2 <-matrix(parameter[[i]]$value,nrow=nrow(parameter[[i]]$value),ncol=ncol(parameter[[i]]$value))
        colnames(out2)<-parameter[[i]]$colNames
        write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
      } else{
        outfile = file.path(Rproject,paste0("/parameter.txt"))
        write.table(parameter[[i]],file=outfile,row.names=FALSE,col.names=FALSE,quote=FALSE)
        if(length(parameter)==1)
        {
          cat("param <- read.vector(\"parameter.txt\") \n",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
        } else {
          cat("pop <- read.vector(\"parameter.txt\") \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
        }   
      }
    }
    if(length(parameter)>1)
      #     {
      #       cat("param <- pop \n ",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
      #     } else  
    { 
      cat("param <- list(pop",file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      for (i in seq(1:length(nameOtherParam)))
      {
        cat(paste0(",",nameOtherParam[[i]]),file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      }
      cat(")\n ",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    } 
  }
  # write groups
  if(!(is.null(group)))
  { 
    cat("\n# groups \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    write.table(group,file=file.path(Rproject,"/group.txt"),row.names=FALSE,col.names=FALSE,quote=FALSE) 
    cat("grp <- read.table(\"group.txt\",header= TRUE) \n ",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  
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
      out2 <-matrix(output[[1]]$value,nrow=nrow(output[[1]]$value),ncol=ncol(output[[1]]$value))
      colnames(out2)<-output[[1]]$colNames
      outfile = file.path(Rproject,"/output.txt")
      write.table(out2,file=outfile,row.names=FALSE,quote=FALSE) 
      #cat("out<-list(out)\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    }else
    {    # many types of output could exist
      for(i in seq(1:length(output)))
      {
        cat(paste0("name<-\"",output[[i]]$name,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        if(!(is.null(output[[i]]$colNames)))
        {
          
          cat(paste0("time<-read.table(\"output",i,".txt\",header=TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
          
          cat(paste0("out",i,"<-list(name=name,time=time) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
          
          out2 <-matrix(output[[i]]$value,nrow=nrow(output[[i]]$value),ncol=ncol(output[[i]]$value))
          colnames(out2)<-output[[i]]$colNames
          outfile = file.path(Rproject,paste0("/output",i))
          outfile = paste0(outfile,".txt")
          write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
        }else{
          cat(paste0("out",i,"<-list(name=name) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        }
      }
      
      cat("out<-list(out1", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      for(i in seq(2,length(output)))
      {
        cat(paste0(",out",i), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)   
      }
      cat(")\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    }
  }
  
  # regressor    
  if(!(is.null(regressor)))
  {  
    # re-order regressor if duplicated or unsorted data (by time) exists on an id.
    for(ir in seq(1:length(regressor)))
    {
      regvalue = regressor[[ir]]$value      
      #remove duplicated rows for the same id
      duprows <-c()
      for(i in 1: (nrow(regvalue)-1))
      {
        for(j in (i+1):nrow(regvalue))
        {
          
          if( (regvalue[i,1]==regvalue[j,1]) ) #same id
          {
            if(regvalue[i,2]==regvalue[j,2]) #duplicated time for the same id
            {
              duprows<-c(duprows,j)             
            }
          }else
          {
            #assuming that the regressor is already sorted by id
            break
          }
        }
      }
      if(!is.null(duprows))
      {
        regvalue<-regvalue[-duprows,]
      }
      #sort according to id and to time
      index<-order(regvalue[,1],regvalue[,2])
      regressor[[ir]]$value <-regvalue[index,]      
    }
    #write regresssor
    cat("\n# regressor \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    # many types of output could exist
    nameOtherReg<-NULL
    if(length(regressor)>1)    
    {  
      cat(paste0("regressor<-c() \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      
      for(i in seq(1:length(regressor)))
      {
        if(!(is.null(regressor[[i]]$name)))
        {
          namePi<-regressor[[i]]$name
        }else {
          namePi<-paste0("regressor",i)
        }
        
        nameOtherReg<-c(nameOtherReg,namePi)
        outfile = file.path(Rproject,paste0("/",namePi,".txt"))      
        cat(paste0(namePi,"<- read.table(\"",namePi,".txt\", header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
        cat(paste0("regressor<-c(regressor,",namePi,")\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        out2<-NULL
        out2 <-matrix(regressor[[i]]$value,nrow=nrow(regressor[[i]]$value),ncol=ncol(regressor[[i]]$value))
        colnames(out2)<-tolower(regressor[[i]]$colNames)
        write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
      }
    }else{
      outfile = file.path(Rproject,paste0("/regressor.txt"))      
      out2<-NULL
      out2 <-matrix(regressor[[1]]$value,nrow=nrow(regressor[[1]]$value),ncol=ncol(regressor[[1]]$value))
      colnames(out2)<-tolower(regressor[[1]]$colNames)
      write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
      cat(paste0("regressor <-read.table(\"regressor.txt\", header = TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
    }
  }
  # call the simulator
  cat("\n# call the simulator \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  cat("res <- simulx(model=model", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  if(!(is.null(treatment)))
  {
    cat(",treatment=trt",file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
  }
  if(!(is.null(parameter)))
  { 
    cat(",parameter=param",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  if(!(is.null(group)))
  { 
    cat(",group=grp",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  if(!(is.null(output)))
  {
    cat(",output=out",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  if(!(is.null(regressor)))
  {
    cat(",regressor=regressor",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  cat(")\n",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  #   if(graphics==TRUE)
  #   {   
  #     # write graphics
  #     cat("\n# display the results \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  #     
  #     if(length(output)==1)
  #     {
  #       cat(paste0("plot <- ggplot() + geom_line(data=res$",output[[1]]$name,"
  #                  , aes(x=time, y=",output[[1]]$name,", colour=id)) +
  #                  geom_point(data=res$",output[[1]]$name,", aes(x=time, y=",output[[1]]$name,",colour=id)) +
  #                  scale_x_continuous(\"Time\") + scale_y_continuous(\"",output[[1]]$name,"\")\n"),
  #           file =projectExe, fill = FALSE, labels = NULL, append = TRUE)      
  #       cat("print(plot)\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  #     }else{    
  #       
  #       for(i in seq(1:length(output)))
  #       {
  #         cat(paste0("plot",i," <- ggplot() + geom_line(data=res$",output[[i]]$name,"
  #                  , aes(x=time, y=",output[[i]]$name,", colour=id)) +
  #                  geom_point(data=res$",output[[i]]$name,", aes(x=time, y=",output[[i]]$name,",colour=id)) +
  #                  scale_x_continuous(\"Time\") + scale_y_continuous(\"",output[[i]]$name,"\")\n"),
  #             file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  #       }
  #       #theme(legend.position=\"none\") + ylab(\"",output[[i]]$label,"\")\n"),
  #       #scale_x_continuous("Time") + scale_y_continuous(as.character(yname))))
  #       cat("grid.arrange(plot1", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  #       for(i in seq(2,length(output)))
  #       {
  #         cat(paste0(",plot",i), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  #       }
  #       cat(paste0(",ncol=",floor(sqrt(length(output))),")\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  #     }
  #   }
  Sys.setenv(LIXOFT_HOME="")
  Sys.setenv('PATH'=myOldENVPATH);
  file.edit(projectExe) 
  setwd(mypath)
}
